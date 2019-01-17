package Exchanges;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.annotation.JsonIgnore;

import java.io.IOException;
import java.util.*;

class AuctionBidding implements Comparable< AuctionBidding > {
    private static long ai = 0;

    public static long increment () {
        return AuctionBidding.ai++;
    }

    private int    investor;
    private int    amount;
    private double interestRate;

    // When two biddings with the same interest rate are proposed, the oldest always has priority
    // And to know which one is the oldest, we use this long counter
    @JsonIgnore()
    private long order = AuctionBidding.increment();

    public AuctionBidding ( int investor, int amount, double interestRate ) {
        this.investor = investor;
        this.amount = amount;
        this.interestRate = interestRate;
    }

    public int getInvestor () {
        return this.investor;
    }

    public int getAmount () {
        return this.amount;
    }

    public double getInterestRate () {
        return this.interestRate;
    }

    public long getOrder () {
        return order;
    }

    @Override
    public int compareTo ( AuctionBidding auctionBidding ) {
        if ( auctionBidding.getInterestRate() == this.getInterestRate() ) {
            return Long.compare( auctionBidding.order, this.order );
        }

        return Double.compare( auctionBidding.getInterestRate(), this.interestRate );
    }

    public String toString () {
        return Long.toString( this.getOrder() );
    }
}

public class Auction {
    private int    id;
    private int    company;
    private int    amount;
    private double maxInterestRate;
    private boolean closed        = false;
    private int     biddingsTotal = 0;

    private Map< Integer, AuctionBidding > biddingsByInvestor = new HashMap<>();
    private TreeSet< AuctionBidding >      biddingsByRate     = new TreeSet<>();
    private List< AuctionBidding >         biddings           = null;

    private Auction () {}

    public Auction ( int id, int company, int amount, double maxInterestRate ) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.maxInterestRate = maxInterestRate;
    }

    public Auction ( int company, int amount, double maxInterestRate ) {
        this( -1, company, amount, maxInterestRate );
    }

    public int getId () {
        return this.id;
    }

    public void setId ( int id ) {
        this.id = id;
    }

    public int getCompany () {
        return company;
    }

    public int getAmount () {
        return amount;
    }

    public double getMaxInterestRate () {
        return maxInterestRate;
    }

    public int getBiddingsTotal () {
        return biddingsTotal;
    }

    public List< AuctionBidding > getBiddings () {
        return biddings;
    }

    public AuctionBidding getLowestBid () {
        return this.biddingsByRate.last();
    }

    public AuctionBidding getHighestBid () {
        return this.biddingsByRate.first();
    }

    public boolean isClosed () {
        return this.closed;
    }

    public Collection< AuctionBidding > bid ( int investor, int amount, double interestRate ) throws ExchangeException {
        return this.bid( new AuctionBidding( investor, amount, interestRate ) );
    }

    public Collection< AuctionBidding > bid ( AuctionBidding bidding ) throws ExchangeException {
        if ( bidding.getAmount() > this.amount * 10 || bidding.getAmount() <= 0 ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidAmount );
        }

        if ( bidding.getInterestRate() > this.maxInterestRate ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidInterestRate );
        }

        AuctionBidding previousBid = this.biddingsByInvestor.get( bidding.getInvestor() );

        // The assumed behaviour for when the same investor is bidding more than once on the same auction
        // Is for the last bid to replace the previous bid by the same investor
        if ( previousBid != null ) {
            if ( bidding.getAmount() < previousBid.getAmount() ) {
                throw new ExchangeException( ExchangeExceptionType.AntiCompetitiveBid );
            }

            if ( bidding.getInterestRate() > previousBid.getInterestRate() ) {
                throw new ExchangeException( ExchangeExceptionType.AntiCompetitiveBid );
            }

            this.biddingsByRate.remove( previousBid );

            this.biddingsTotal -= previousBid.getAmount();

            this.biddingsByInvestor.remove( bidding.getInvestor() );
        }

        this.biddingsByInvestor.put( bidding.getInvestor(), bidding );

        this.biddingsByRate.add( bidding );

        this.biddingsTotal += bidding.getAmount();

        // Since we created a new bidding, we have to be aware of the case that some biddings might have been invalidated:
        // that is, that some biddings with higher interest rates might no longer be in play for this auction
        // Son they can be safely removed
        Collection< AuctionBidding > invalidatedBiddings = new ArrayList<>();

        Iterator< AuctionBidding > iterator = this.biddingsByRate.iterator();

        AuctionBidding cursor;

        int targetAmount = this.amount * 10;

        // The TreeSet is ordered from biggest interest rate to lowest. So if we want to "prune" any invalidated
        // biddings, we can simply start from the beginning and remove them until the total amount isn't enough anymore
        while ( iterator.hasNext() ) {
            cursor = iterator.next();

            if ( this.biddingsTotal - cursor.getAmount() < targetAmount ) {
                break;
            }

            this.biddingsByInvestor.remove( cursor.getInvestor() );

            this.biddingsTotal -= cursor.getAmount();

            invalidatedBiddings.add( cursor );

            iterator.remove();
        }

        return invalidatedBiddings;
    }

    public boolean isSuccess () {
        return this.biddingsTotal >= this.amount * 10;
    }

    public boolean close () {
        if ( this.closed ) {
            return this.isSuccess();
        }

        this.closed = true;

        this.biddings = new ArrayList<>( this.biddingsByRate );

        this.biddingsByInvestor.clear();
        this.biddingsByRate.clear();

        return this.isSuccess();
    }

    public static Auction fromJSON ( String json ) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.readValue( json, Auction.class );
    }

    public String toJSON () throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.writeValueAsString( this );
    }

    public String toString () {
        try {
            return this.toJSON();
        } catch ( JsonProcessingException e ) {
            return super.toString();
        }
    }

    public static List< Auction > listFromJSON ( String json ) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.readValue( json, new TypeReference< List< Auction > >() { } );
    }
}
