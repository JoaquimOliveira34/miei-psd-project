package Exchanges;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

class AuctionBidding {
    private int    investor;
    private int    amount;
    private double interestRate;

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
}

public class Auction {
    private int    id;
    private int    company;
    private int    amount;
    private double maxInterestRate;
    private boolean closed = false;

    private Map< Integer, AuctionBidding > biddings = new HashMap<>();
    private List< AuctionBidding >         accepted = null;

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

    public void bid ( int investor, int amount, double interestRate ) throws ExchangeException {
        this.bid( new AuctionBidding( investor, amount, interestRate ) );
    }

    public void bid ( AuctionBidding bidding ) throws ExchangeException {
        if ( this.biddings.containsKey( bidding.getInvestor() ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateBidding );
        }

        if ( bidding.getAmount() * 10 > this.amount || bidding.getAmount() <= 0 ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidAmount );
        }

        if ( bidding.getInterestRate() > this.maxInterestRate ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidInterestRate );
        }

        this.biddings.put( bidding.getInvestor(), bidding );
    }

    public boolean isSuccess () {
        return this.biddings.values().stream().mapToInt( bidding -> bidding.getAmount() * 10 ).sum() >= this.amount;
    }

    public List< AuctionBidding > close () {
        if ( this.closed ) {
            return this.accepted;
        }

        this.closed = true;

        List< AuctionBidding > sortedBiddings = this.biddings
                .values()
                .stream()
                .sorted( Comparator.comparingDouble( AuctionBidding::getInterestRate ) )
                .collect( Collectors.toList() );

        int sum = 0;
        int i;

        for ( i = 0; i < sortedBiddings.size() && sum * 10 < this.amount; i++ ) {
            sum += sortedBiddings.get( i ).getAmount();
        }

        if ( sum * 10 < this.amount ) {
            return null;
        }

        return this.accepted = sortedBiddings.subList( 0, i );
    }

    public static Auction fromJSON ( String json ) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.readValue( json, Auction.class );
    }

    public String toJSON () throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.writeValueAsString( this );
    }
}
