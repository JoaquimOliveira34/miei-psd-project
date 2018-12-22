package Exchanges;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

class Bidding {
    private int investor;
    private int amount;
    private double interestRate;

    public Bidding ( int investor, int amount, double interestRate ) {
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
    private int company;
    private int amount;
    private double maxInterestRate;
    private boolean closed = false;

    private Map<Integer, Bidding> biddings;
    private List<Bidding> accepted = null;

    public Auction ( int company, int amount, double maxInterestRate ) {
        this.company = company;
        this.amount = amount;
        this.maxInterestRate = maxInterestRate;
    }

    public void bid ( int investor, int amount, double interestRate ) throws ExchangeException {
        this.bid( new Bidding( investor, amount, interestRate ) );
    }

    public void bid ( Bidding bidding ) throws ExchangeException {
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

    public List<Bidding> close () {
        if ( this.closed ) {
            return this.accepted;
        }

        this.closed = true;

        List<Bidding> sortedBiddings = this.biddings
            .values()
            .stream()
            .sorted( ( a,b ) -> Double.compare( a.getInterestRate(), b.getInterestRate() ) )
            .collect( Collectors.toList() );

        int sum = 0;
        int i = 0;
        
        for ( i = 0; i < sortedBiddings.size() && sum * 10 < this.amount; i++ ) {
            sum += sortedBiddings.get( i ).getAmount();
        }

        if ( sum * 10 < this.amount ) {
            return null;
        }

        return this.accepted = sortedBiddings.subList( 0, i );
    }
}
