package rest.resources;
import rest.representations.Saying;

import java.util.*;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/peerlending")
@Produces(MediaType.APPLICATION_JSON)
public class test {
    static class Company{
        private int Id;
        private String name;
        private String zone;
        private List<Integer> auctions;

        public Company(int id, String name, String zone){
            this.Id = id;
            this.name=name;
            this.zone=zone;
            this.auctions = new ArrayList<>();
        }

        public void addAuction(int auctionId){
            this.auctions.add(auctionId);
        }

        public String toString(){
            return "Name " + this.name + " , Zone " + this.zone +"\nAuctions Participated: " + this.auctions.toString();
        }

    }
    static class Investor{
        private int Id;
        private String name;
        private String zone;
        private List<Integer> auctionsBidded;

        public Investor(int id,String name, String zone){
            this.name=name;
            this.zone=zone;
            this.auctionsBidded = new ArrayList<>();
        }

        public void addBid(int auctionId){
            this.auctionsBidded.add(auctionId);
        }

        public String toString(){
            return "Name " + this.name + " , Zone " + this.zone +"\nAuctions Participated: " + this.auctionsBidded.toString();
        }

    }
    static class Bidding {
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


    static class Auction{
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

    private Map<String,Company> companies;
    private Map<String,Investor> investors;
    private Map<Integer,Auction> auctions;
    private long requestCounter;
    private final String templateCompanies = "Empresas registadas no sistema :\n%s";
    private final String templateAuctions = "Leiloes registados no sistema :\n%s";
    private final String templateInvestors = "Investidores registados no sistema :\n%s";

    public test(){
        this.companies = new HashMap<>();
        this.investors = new HashMap<>();
        this.auctions = new HashMap<>();
        this.requestCounter = 0;
        this.companies.put("test",new Company(1,"test","Braga"));
        this.investors.put("test",new Investor(1,"test","Braga"));

    }

    @GET
    @Path("/companies")
    public Saying getCompanies() {
        final String content = String.format(templateCompanies, this.companies.toString());
        long i;
        synchronized (this) { requestCounter++; i = requestCounter; }
        // demo only; if counter is resource state, GET should not increment it
        return new Saying(i, content);
    }

    @GET
    @Path("/company/{name}")
    public Saying getCompany(@PathParam("name") String name) {
        Company c = this.companies.get(name);
        final String content = String.format(templateCompanies, c.toString());
        String s = c.toString();
        long i;
        synchronized (this) { requestCounter++; i = requestCounter; }
        // demo only; if counter is resource state, GET should not increment it
        return new Saying(i, s);
    }

    @PUT
    @Path("/company/{names}")
    public Response putCompany(@FormParam("name") Set<String> names) { //testar...
        boolean error = false;
            Iterator i= names.iterator();
            String id= (String) i.next();
            String name= (String) i.next();
            String zone= (String) i.next();
            Company c = new Company(Integer.parseInt(id),name,zone);
            if(c==null)
                error = true;
            else this.companies.put(c.name, c);

        return Response.ok().build();
        //if (!error) return Response.ok().build();
        //else return Response.serverError().build(); //ver melhor
    }




    @GET
    @Path("/investors")
    public Saying getInvestors() {
        final String content = String.format(templateInvestors, this.investors.toString());
        long i;
        synchronized (this) { requestCounter++; i = requestCounter; }
        // demo only; if counter is resource state, GET should not increment it
        return new Saying(i, content);
    }

    @GET
    @Path("/investor/{name}")
    public Saying getInvestor(@PathParam("name") String name) {
        long i;
        synchronized (this) { requestCounter++; i = requestCounter; }
        if(this.investors.containsKey(name)){
        Investor inv = this.investors.get(name);
        final String content = String.format(templateInvestors, inv.toString());
        String s = inv.toString();
        // demo only; if counter is resource state, GET should not increment it
        return new Saying(i, content);}
        else throw new WebApplicationException(Response.Status.NOT_FOUND);
    }

    @PUT
    @Path("/investor/{names}")
    public Response putInvestor(@FormParam("name") Set<String> names) {
        boolean error = false;
        Iterator i= names.iterator();
        String id= (String) i.next();
        String name= (String) i.next();
        String zone= (String) i.next();
        Company c = new Company(Integer.parseInt(id),name,zone);
        if(c==null)
            error = true;
        else this.companies.put(c.name, c);

        return Response.ok().build();
        //if (!error) return Response.ok().build();
        //else return Response.serverError().build(); //ver melhor
    }
    @DELETE
    @Path("/company/{name}")
    public Response deleteCompany(@PathParam("name") String name) {
        this.companies.remove(name);
        return Response.ok().build();//verificar se n tem auction ativa
    }

    /*
    * Auctions
    */
    @GET
    @Path("/auctions")
    public List<Auction> getAuctions() {
        synchronized ( this ) {
            return  new ArrayList<>( this.auctions.values() );
        }
    }

    @POST
    @Path("/auctions")
    public Response postAuction( Auction auction ){
        synchronized ( this ) {
            int Id = this.auctions.size(); //ver concorrencia

            auction.setId(Id);

            this.auctions.put(Id, auction);

            return Response.ok(auction).build();
        }
    }


    @POST
    @Path("/auction/{id}")
//    @Consumes("application/json")
    public Response putAuction ( @PathParam( "id" ) int Id, Auction auction ){
        synchronized ( this ) {
            auction.setId( Id );

            this.auctions.put( Id, auction );

            return Response.ok(auction).build();
        }
    }
}
