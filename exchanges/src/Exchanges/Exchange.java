package Exchanges;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

interface ExchangeController {
    void auctionCreated ( Auction auction );

    void auctionBid ( Auction auction, AuctionBidding bidding );

    void auctionClosed ( int company, boolean success, List< AuctionBidding > biddings );

    void auctionBiddingInvalidated ( int company, int investor, AuctionBidding lowest );

    void emissionCreated ( Emission emission );

    void emissionSubscribed ( Emission emission, EmissionSubscription subscription );

    void emissionClosed ( int company, List< EmissionSubscription > subscriptions );

    void scheduleClose ( int company );
}

class ExchangeCloseRunnable implements Runnable {
    private       int                company;
    private final ExchangeController controller;

    public ExchangeCloseRunnable ( ExchangeController controller, int company ) {
        this.company = company;
        this.controller = controller;
    }

    @Override
    public void run () {
        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.scheduleClose( this.company );
            }
        }
    }
}

public class Exchange {
    // The history HashMap stores the last event (either auction or emission) for each company
    private Map< Integer, Either< Auction, Emission > > history   = new HashMap<>();
    // The auctions HashMap contains the current (if any) auction for each company
    private Map< Integer, Auction >                     auctions  = new ConcurrentHashMap<>();
    // The emissions HashMap contains the current (if any) emission for each company
    private Map< Integer, Emission >                    emissions = new ConcurrentHashMap<>();
    // Each company can only have either an emission or an auction running at a time; not both
    private Set< Integer >                              companies = new HashSet<>();


    private DirectoryClient directory;

    // Class used to schedule the closure of emissions and auctions automatically
    // The scheduled task runs on a separate thread and interacts in a synchronized manner
    // With the controller, notifying it to close the event
    private ScheduledExecutorService scheduler = new ScheduledThreadPoolExecutor( 1 );

    private long     durationTime  = 15;
    private TimeUnit durationUnits = TimeUnit.SECONDS;

    private ExchangeController controller;

    public Exchange ( DirectoryClient directory ) {
        this.directory = directory;
    }

    public Exchange ( DirectoryClient directory, ExchangeController controller ) {
        this( directory );

        this.setController( controller );
    }

    public void setController ( ExchangeController controller ) {
        this.controller = controller;
    }

    public long getDurationTime () {
        return this.durationTime;
    }

    public void setDurationTime ( long durationTime ) {
        this.durationTime = durationTime;
    }

    public TimeUnit getDurationUnits () {
        return durationUnits;
    }

    public void setDurationUnits ( TimeUnit durationUnits ) {
        this.durationUnits = durationUnits;
    }


    public boolean hasCompany ( int id ) {
        if ( this.companies.contains( id ) ) {
            return true;
        }

        try {
            boolean exists = this.directory.companyExists( id );

            if ( exists ) {
                this.companies.contains( id );
            }

            return exists;
        } catch ( Exception e ) {
            return false;
        }
    }


    public boolean hasAuctionFor ( int company ) {
        return this.auctions.containsKey( company );
    }

    public void createAuction ( int company, int amount, double maxInterestRate ) throws ExchangeException {
        if ( !this.hasCompany( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        if ( this.hasAuctionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateAuction );
        }

        if ( this.hasEmissionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateEvents );
        }

        Auction auction = new Auction( company, amount, maxInterestRate );

        this.auctions.put( company, auction );

        this.history.put( company, Either.left( auction ) );

        // The new Auction object is the one returned from the Directory after being created
        // And so already has the correct auction id
        this.directory.createAuctionAsync( auction ).thenAccept( newAuction -> {
            this.auctions.get( newAuction.getCompany() ).setId( newAuction.getId() );
        } );

        this.scheduler.schedule( new ExchangeCloseRunnable( this.controller, company ), this.durationTime, this.durationUnits );

        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.auctionCreated( auction );
            }
        }
    }

    public void bidAuction ( int company, int investor, int amount, double rate ) throws ExchangeException {
        if ( !this.auctions.containsKey( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Auction auction = this.auctions.get( company );

        Collection< AuctionBidding > invalidated = auction.bid( investor, amount, rate );

        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.auctionBid( auction, new AuctionBidding( investor, amount, rate ) );

                for ( AuctionBidding bidding : invalidated ) {
                    this.controller.auctionBiddingInvalidated( company, bidding.getInvestor(), auction.getLowestBid() );
                }
            }
        }
    }

    public void closeAuction ( int company ) throws ExchangeException {
        if ( !this.auctions.containsKey( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Auction auction = this.auctions.get( company );

        boolean success = auction.close();

        this.directory.closeAuctionAsync( auction, success ? auction.getBiddings() : null );

        this.auctions.remove( company );

        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.auctionClosed( company, success, auction.getBiddings() );
            }
        }
    }

    /**
     * Helper function that queries the directory and finds the appropriate interest rate for a new emission
     *
     * @param company
     * @return
     */
    public double getEmissionInterestRate ( int company ) {
        Either< Auction, Emission > history = this.history.get( company );

        if ( history == null ) {
            return -1;
        }

        if ( history.isLeft() ) {
            List< AuctionBidding > biddings = history.getLeft().getBiddings();

            if ( biddings != null ) {
                return biddings
                        .stream()
                        .mapToDouble( AuctionBidding::getInterestRate )
                        .max()
                        .orElse( -1 );
            } else {
                return -1;
            }
        } else if ( history.isRight() ) {
            Emission emission = history.getRight();

            if ( emission.isCompleted() ) {
                return emission.getInterestRate();
            } else {
                return emission.getInterestRate() * 1.1;
            }
        } else {
            return -1;
        }
    }

    public boolean hasEmissionFor ( int company ) {
        return this.emissions.containsKey( company );
    }

    public void createEmission ( int company, int amount ) throws ExchangeException {
        if ( !this.hasCompany( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        if ( this.hasEmissionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateEmission );
        }

        if ( this.hasAuctionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateEvents );
        }

        double maxInterestRate = this.getEmissionInterestRate( company );

        if ( maxInterestRate < 0 ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidInterestRate );
        }

        Emission emission = new Emission( company, amount, maxInterestRate );

        this.emissions.put( company, emission );

        this.history.put( company, Either.right( emission ) );

        // The new Emission object is the one returned from the Directory after being created
        // And so already has the correct emission id
        this.directory.createEmissionAsync( emission ).thenAccept( newEmission -> {
            this.emissions.get( newEmission.getCompany() ).setId( newEmission.getId() );
        } );

        this.scheduler.schedule( new ExchangeCloseRunnable( this.controller, company ), this.durationTime, this.durationUnits );

        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.emissionCreated( emission );
            }
        }
    }

    public void subscribeEmission ( int company, int investor, int amount ) throws ExchangeException {
        if ( !this.hasEmissionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Emission emission = this.emissions.get( company );

        EmissionSubscription subscription = emission.subscribe( investor, amount );

        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.emissionSubscribed( emission, subscription );
            }
        }
    }

    public void closeEmission ( int company ) throws ExchangeException {
        if ( !this.hasEmissionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Emission emission = this.emissions.get( company );

        List< EmissionSubscription > subscribed = emission.close();

        this.directory.closeEmissionAsync( emission, subscribed );

        this.emissions.remove( company );

        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.emissionClosed( company, subscribed );
            }
        }
    }
}


class Either < L, R > {
    private L left;
    private R right;

    // false is left
    // true is right
    private boolean side = false;

    public static < L, R > Either< L, R > left ( L value ) {
        return new Either<>( value, null, false );
    }

    public static < L, R > Either< L, R > right ( R value ) {
        return new Either<>( null, value, true );
    }

    private Either ( L left, R right, boolean side ) {
        this.left = left;
        this.right = right;
        this.side = side;
    }

    public boolean isLeft () {
        return !this.side;
    }

    public boolean isRight () {
        return this.side;
    }

    public L getLeft () {
        return this.left;
    }

    public R getRight () {
        return this.right;
    }
}