package rest.resources;
import rest.representations.Saying;

import jdk.nashorn.internal.objects.annotations.Getter;

import com.google.common.base.Optional;

import java.lang.reflect.Constructor;
import java.util.*;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/peerlending")
@Produces(MediaType.APPLICATION_JSON)
public class test {
    class Company{
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
    class Investor{
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


    class Auction{
        private int Id;
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

        public String toString(){
            return "Id: " + Id + ", Company: " + company + ", Amount: " + amount +", maxInterestRate: " + maxInterestRate +
                    ", closed: " + closed + ",Biddings: " + biddings.toString() + "Accepted: " + accepted.toString();
        }

    }
    private static Map<String,Company> companies;
    private static Map<String,Investor> investors;
    private static Map<Integer,Auction> auctions;
    private static long requestCounter;
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

    @GET
    @Path("/auctions")
    public Saying getAuctions() {
        final String content = String.format(templateAuctions, this.auctions.toString());
        long i;
        synchronized (this) { requestCounter++; i = requestCounter; }
        // demo only; if counter is resource state, GET should not increment it
        return new Saying(i, content);
    }

    @POST
    @Path("/auctions")
    public Response postAuction(@FormParam("name") Set<String> names){
        int Id=this.auctions.size(); //ver concorrencia
        Iterator i= names.iterator();
        int company= Integer.parseInt((String) i.next());
        int amount= Integer.parseInt((String) i.next());
        double maxInterestRate = Double.parseDouble((String) i.next());
        this.auctions.put(Id, new Auction(company,amount,maxInterestRate));
        return Response.ok(Id).build();
    }
}
