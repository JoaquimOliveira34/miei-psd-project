package rest.representations;

import java.util.List;
import java.util.Map;

class Bidding {
    private int investor;
    private int amount;
    private double interestRate;

    public Bidding () {}

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

    public void setInvestor ( int investor ) {
        this.investor = investor;
    }

    public void setAmount ( int amount ) {
        this.amount = amount;
    }

    public void setInterestRate ( double interestRate ) {
        this.interestRate = interestRate;
    }
}


public class Auction{
    private int Id;
    private int company;
    private int amount;
    private double maxInterestRate;
    private boolean closed = false;
    private Map<Integer, Bidding> biddings;
    private List<Bidding> accepted = null;

    public Auction () {}

    public Auction ( int company, int amount, double maxInterestRate ) {
        this.company = company;
        this.amount = amount;
        this.maxInterestRate = maxInterestRate;
    }

    public void setId ( int id ) {
        Id = id;
    }

    public int getId () {
        return Id;
    }

    public void setAmount ( int amount ) {
        this.amount = amount;
    }

    public int getAmount () {
        return amount;
    }

    public int getCompany () {
        return company;
    }

    public void setCompany ( int company ) {
        this.company = company;
    }

    public void setMaxInterestRate ( double maxInterestRate ) {
        this.maxInterestRate = maxInterestRate;
    }

    public double getMaxInterestRate () {
        return maxInterestRate;
    }

    public boolean isClosed () {
        return closed;
    }

    public void setClosed ( boolean closed ) {
        this.closed = closed;
    }

    public List< Bidding > getAccepted () {
        return accepted;
    }

    public void setAccepted ( List< Bidding > accepted ) {
        this.accepted = accepted;
    }

    public String toString(){
        return "Id: " + Id + ", Company: " + company + ", Amount: " + amount +", maxInterestRate: " + maxInterestRate +
               ", closed: " + closed + ",Biddings: " + biddings.toString() + "Accepted: " + accepted.toString();
    }
}
