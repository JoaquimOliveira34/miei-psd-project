package rest.representations;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.List;

class EmissionSubscription {
    private int investor;
    private int amount;

    public EmissionSubscription () {}

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

    public void setInvestor ( int investor ) {
        this.investor = investor;
    }

    public void setAmount ( int amount ) {
        this.amount = amount;
    }
}

public class Emission {
    private int    id;
    private int    company;
    private int    amount;
    private double interestRate;
    private List<EmissionSubscription> subscriptions = null;

    public Emission () {}

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

    public List< EmissionSubscription > getSubscriptions () {
        return subscriptions;
    }

    public void setCompany ( int company ) {
        this.company = company;
    }

    public void setAmount ( int amount ) {
        this.amount = amount;
    }

    public void setInterestRate ( double interestRate ) {
        this.interestRate = interestRate;
    }

    public void setSubscriptions ( List< EmissionSubscription > subscriptions ) {
        this.subscriptions = subscriptions;
    }
}
