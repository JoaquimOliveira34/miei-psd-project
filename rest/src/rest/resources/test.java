package rest.resources;
import rest.representations.Saying;
import rest.representations.Auction;
import rest.representations.Emission;

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

    private Map<String,Company> companies;
    private Map<String,Investor> investors;
    private Map<Integer,Auction> auctions;
    private Map<Integer,Emission> emissions;
    private long requestCounter;
    private final String templateCompanies = "Empresas registadas no sistema :\n%s";
    private final String templateAuctions = "Leiloes registados no sistema :\n%s";
    private final String templateInvestors = "Investidores registados no sistema :\n%s";

    public test(){
        this.companies = new HashMap<>();
        this.investors = new HashMap<>();
        this.auctions = new HashMap<>();
        this.emissions = new HashMap<>();
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


    @GET
    @Path("/auction/{id}")
    public Response putAuction ( @PathParam( "id" ) int Id ){
        synchronized ( this ) {
            Auction auction = this.auctions.get( Id );

            if ( auction == null ) {
                return Response.status( Response.Status.NOT_FOUND ).build();
            }

            return Response.ok( auction ).build();
        }
    }

    @POST
    @Path("/auction/{id}")
    public Response putAuction ( @PathParam( "id" ) int Id, Auction auction ){
        synchronized ( this ) {
            auction.setId( Id );

            this.auctions.put( Id, auction );

            return Response.ok(auction).build();
        }
    }

    /*
     * Emissions
     */
    @GET
    @Path("/emissions")
    public Response getEmissions() {
        synchronized ( this ) {
            return Response.ok( new ArrayList<>( this.emissions.values() ) ).build();
        }
    }

    @POST
    @Path("/emissions")
    public Response postEmission( Emission emission ){
        synchronized ( this ) {
            int Id = this.emissions.size();

            emission.setId(Id);

            this.emissions.put(Id, emission);

            return Response.ok(emission).build();
        }
    }


    @GET
    @Path("/emission/{id}")
    public Response putEmission ( @PathParam( "id" ) int Id ){
        synchronized ( this ) {
            Emission emission = this.emissions.get( Id );

            if ( emission == null ) {
                return Response.status( Response.Status.NOT_FOUND ).build();
            }

            return Response.ok( emission ).build();
        }
    }

    @POST
    @Path("/emission/{id}")
    public Response putEmission ( @PathParam( "id" ) int Id, Emission emission ){
        synchronized ( this ) {
            emission.setId( Id );

            this.emissions.put( Id, emission );

            return Response.ok( emission ).build();
        }
    }
}
