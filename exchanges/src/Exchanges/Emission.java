package Exchanges;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class EmissionSubscription {
    private int investor;
    private int amount;

    public EmissionSubscription ( int investor, int amount ) {
        this.investor = investor;
        this.amount = amount;
    }

    public int getInvestor () {
        return investor;
    }

    public int getAmount () {
        return amount;
    }
}

public class Emission {
    private int    id;
    private int    company;
    private int    amount;
    private double interestRate;
    private Map< Integer, EmissionSubscription > subscriptions = new HashMap<>();
    private boolean completed;

    private Emission () {}

    public Emission ( int id, int company, int amount, double interestRate ) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.interestRate = interestRate;
    }

    public Emission ( int company, int amount, double interestRate ) {
        this( -1, company, amount, interestRate );
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

    public double getInterestRate () {
        return interestRate;
    }

    public int getSubscribedAmount () {
        return this.subscriptions.values().stream().mapToInt( EmissionSubscription::getAmount ).sum();
    }

    public boolean isCompleted () {
        return this.getSubscribedAmount() * 10 == this.amount;
    }

    public void subscribe ( int investor, int amount ) throws ExchangeException {
        this.subscribe( new EmissionSubscription( investor, amount ) );
    }

    public void subscribe ( EmissionSubscription subscription ) throws ExchangeException {
        if ( this.subscriptions.containsKey( subscription.getInvestor() ) ) {
            throw new ExchangeException( ExchangeExceptionType.DuplicateBidding );
        }

        if ( ( subscription.getAmount() + this.getSubscribedAmount() ) * 10 > this.amount || subscription.getAmount() <= 0 ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidAmount );
        }

        this.subscriptions.put( subscription.getInvestor(), subscription );
    }

    public List< EmissionSubscription > close () {
        return new ArrayList<>( this.subscriptions.values() );
    }

    public static Emission fromJSON ( String json ) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.readValue( json, Emission.class );
    }

    public String toJSON () throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.writeValueAsString( this );
    }

    public static List<Emission> listFromJSON ( String json ) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.readValue( json, new TypeReference<List<Emission>>(){} );
    }
}
