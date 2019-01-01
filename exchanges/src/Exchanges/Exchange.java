package Exchanges;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

interface ExchangeController {
    void auctionCreated ( Auction auction );

    void auctionClosed ( int company, List< AuctionBidding > biddings );

    void emissionCreated ( Emission emission );

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
    private Map< Integer, Auction >  auctions;
    private Map< Integer, Emission > emissions;
    private DirectoryClient          directory;
    private ScheduledExecutorService scheduler = new ScheduledThreadPoolExecutor( 1 );

    private long durationTime = 15;
    private TimeUnit durationUnits = TimeUnit.SECONDS;

    private ExchangeController controller;

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


    public boolean hasAuctionFor ( int company ) {
        return this.auctions.containsKey( company );
    }

    public void createAuction ( int company, int amount, double maxInterestRate ) throws ExchangeException {
        if ( this.hasAuctionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateAuction );
        }

        if ( this.hasEmissionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateEvents );
        }

        Auction auction = new Auction( company, amount, maxInterestRate );

        try {
            // The new Auction object is the one returned from the Directory after being created
            // And so already has the correct auction id
            auction = this.directory.createAuction( auction );
        } catch ( IOException exception ) {
            exception.printStackTrace();

            throw new ExchangeException( ExchangeExceptionType.DirectoryError );
        }


        this.scheduler.schedule( new ExchangeCloseRunnable( this.controller, company ), this.durationTime, this.durationUnits );

        this.auctions.put( company, auction );

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

        auction.bid( investor, amount, rate );
    }

    public void closeAuction ( int company ) throws ExchangeException {
        if ( !this.auctions.containsKey( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Auction auction = this.auctions.get( company );

        List< AuctionBidding > accepted = auction.close();

        this.directory.closeAuction( auction, accepted );

        this.auctions.remove( company );


        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.auctionClosed( company, accepted );
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
        Emission emission = this.directory.getLastEmission( company );

        if ( emission != null ) {
            if ( emission.isCompleted() ) {
                return emission.getInterestRate();
            } else {
                return emission.getInterestRate() * 1.1;
            }
        }

        Auction auction = this.directory.getLastAuction( company );

        if ( auction != null ) {
            List< AuctionBidding > biddings = this.directory.getAuctionBiddings( auction.getId() );

            if ( biddings != null ) {
                return biddings
                        .stream()
                        .mapToDouble( AuctionBidding::getInterestRate )
                        .max()
                        .orElse( -1 );
            } else {
                return -1;
            }
        }

        return -1;
    }


    public boolean hasEmissionFor ( int company ) {
        return this.emissions.containsKey( company );
    }

    public void createEmission ( int company, int amount ) throws ExchangeException {
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

        try {
            // The new Emission object is the one returned from the Directory after being created
            // And so already has the correct emission id
            emission = this.directory.createEmission( emission );
        } catch ( IOException exception ) {
            exception.printStackTrace();

            throw new ExchangeException( ExchangeExceptionType.DirectoryError );
        }

        this.scheduler.schedule( new ExchangeCloseRunnable( this.controller, company ), this.durationTime, this.durationUnits );

        this.emissions.put( company, emission );

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

        emission.subscribe( investor, amount );
    }

    public void closeEmission ( int company ) throws ExchangeException {
        if ( !this.hasEmissionFor( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Emission emission = this.emissions.get( company );

        List< EmissionSubscription > subscribed = emission.close();

        this.directory.closeEmission( emission, subscribed );

        this.emissions.remove( company );

        if ( this.controller != null ) {
            synchronized ( this.controller ) {
                this.controller.emissionClosed( company, subscribed );
            }
        }
    }
}
