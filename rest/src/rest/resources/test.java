package rest.resources;
import rest.representations.*;
import sun.security.provider.certpath.OCSPResponse;

import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/peerlending")
@Produces(MediaType.APPLICATION_JSON)
public class test implements Serializable {


    private Map<String, Company> companies;
    private Map<String, Investor> investors;
    private Map<Integer,Auction> auctions;
    private Map<Integer,Emission> emissions;
    private AtomicInteger idCounter;
    private ObjectOutputStream osc;
    private ObjectOutputStream osi;
    private ObjectOutputStream osa;
    private ObjectOutputStream ose;
    private ObjectOutputStream osidc;

    private final String templateCompanies = "Empresas registadas no sistema :\n%s";
    private final String templateAuctions = "Leiloes registados no sistema :\n%s";
    private final String templateInvestors = "Investidores registados no sistema :\n%s";

    private Company getCompanyById ( int id ) {
        return this.companies.values().stream().filter( company -> company.getId() == id ).findFirst().orElse( null );
    }

    public test(){
        this.companies = new HashMap<>();
        this.investors = new HashMap<>();
        this.auctions = new HashMap<>();
        this.emissions = new HashMap<>();
        this.idCounter = new AtomicInteger(0);
        Company c1= new Company(this.idCounter.incrementAndGet(),"test","password","Braga");
        Company c2= new Company(this.idCounter.incrementAndGet(),"test","password","Braga");
        this.companies.put("test",c1);
        this.companies.put("test1",c2);
        this.investors.put("test",new Investor(this.idCounter.incrementAndGet(),"test","password","Braga"));
        try {
            loadCompanies();
            loadInvestors();
            loadAuctions();
            loadEmissions();
            initOS();

            System.out.println(this.auctions);
        }
        catch(Exception e){
            System.out.println(e.getMessage());
        }
        //writeCompanies();
    }
    public void initOS()throws IOException{
        //testar appends...
        this.osc = new ObjectOutputStream(new FileOutputStream("companies"));
        this.osi = new ObjectOutputStream(new FileOutputStream("investors"));
        this.osa = new ObjectOutputStream(new FileOutputStream("auctions"));
        this.ose = new ObjectOutputStream(new FileOutputStream("emissions"));
        this.osidc = new ObjectOutputStream(new FileOutputStream("idcounter"));
    }
    public void loadAuctions() throws IOException, ClassNotFoundException {
        ObjectInputStream is = new ObjectInputStream(new FileInputStream("auctions"));
        this.auctions = (HashMap) is.readObject();
        is.close();
    }
    public void loadCompanies() throws IOException, ClassNotFoundException {
        ObjectInputStream is = new ObjectInputStream(new FileInputStream("companies"));
        this.companies = (HashMap) is.readObject();
        System.out.println(this.companies.toString());
        is.close();
    }
    public void loadInvestors() throws IOException, ClassNotFoundException {
        ObjectInputStream is = new ObjectInputStream(new FileInputStream("investors"));
        this.investors = (HashMap) is.readObject();
        is.close();
    }
    public void loadEmissions() throws IOException, ClassNotFoundException {
        ObjectInputStream is = new ObjectInputStream(new FileInputStream("emissions"));
        this.emissions = (HashMap) is.readObject();
        is.close();
    }

    public void loadCounter() throws IOException, ClassNotFoundException {
        ObjectInputStream is = new ObjectInputStream(new FileInputStream("idcounter"));
        this.auctions = (HashMap<Integer,Auction>) is.readObject();
        is.close();
    }
    public void writeCompanies(){
        try {
            osc.writeObject(companies);
            osc.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void writeInvestors(){
        try {
            osi.writeObject(investors);
            osi.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void writeEmissions(){
        try {
            ose.writeObject(emissions);
            ose.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void writeAuctions(){
        try {
            osa.writeObject(auctions);
            osa.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void writeCounter(){
        try {
            osa.writeObject(idCounter.get());
            osa.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    @GET
    @Path("/users")
    public List<User> getUsersErlang(){
        List<User> users = new ArrayList<>();
        for (Investor i: investors.values())
            users.add(new User(i.getName(),i.getPassword(),i.getId(),true));
        for (Company c: companies.values())
            users.add(new User(c.getName(),c.getPassword(),c.getId(),false));
        return users;
    }
    @GET
    @Path("/companies")
    public List<Company> getCompanies() {
        /*
        final String content = String.format(templateCompanies, this.companies.toString());
        long i;
        synchronized (this) { requestCounter++; i = requestCounter; }
        // demo only; if counter is resource state, GET should not increment it
        return new Saying(i, content);*/
        synchronized ( this ) {
            return  new ArrayList<>( this.companies.values() );
        }
    }
    @POST
    @Path("/companies")
    public Response postCompany(Company company) {
        synchronized (this){
            if(this.companies.containsKey(company.getName()))
                return Response.status(Response.Status.PRECONDITION_FAILED).build();
            else {
                int Id = this.idCounter.incrementAndGet();
                company.setId(Id);
                this.companies.put(company.getName(),company);
                writeCompanies();
                writeCounter();
                return Response.ok(Id).build();
            }
        }
    }

    @PUT
    @Path("/company/{name}")
    public Response putCompany(@PathParam("name") String name, String password, String zone) {
        synchronized (this){
            Company c = new Company(this.idCounter.incrementAndGet(),name,password,zone);
            this.companies.put(name, c);
            writeCompanies();
            writeCounter();
            return Response.ok(c.getId()).build();
            /*
            if(this.companies.containsKey(company.getName())) {
                this.companies.put(company.getName(), company);
                return Response.status(Response.Status.OK).build();
            }
            else {
                int Id = this.idCounter.incrementAndGet();
                company.setId(Id);
                this.companies.put(name,company);
                return Response.ok(Id).build();
            }
            */
        }
    }

    @GET
    @Path("/company/{name}")
    public Response getCompany(@PathParam("name") String name) {
        Company c;
        synchronized (this) {
            c = this.companies.get(name);
        }
        if (c == null)
            return Response.status (Response.Status.NOT_FOUND).build();
        else return Response.ok(c).build();

    }





    @GET
    @Path("/investors")
    public List<Investor> getInvestors() {
        synchronized ( this ) {
            return  new ArrayList<>( this.investors.values() );
        }
    }

    @GET
    @Path("/investor/{name}")
    public Response getInvestor(@PathParam("name") String name) {
        long i;
        if(this.investors.containsKey(name)){
            Investor inv = this.investors.get(name);
            return Response.ok( inv ).build();
        }
        else return Response.status(Response.Status.NOT_FOUND).build();
    }

    @POST
    @Path("/investors")
    public Response postInvestor(Investor inv) {
        synchronized (this){
            if(this.investors.containsKey(inv.getName()))
                return Response.status(Response.Status.PRECONDITION_FAILED).build();
            else {
                int Id = this.idCounter.incrementAndGet();
                inv.setId(Id);
                this.investors.put(inv.getName(),inv);
                writeInvestors();
                writeCounter();
                return Response.ok(Id).build();
            }
        }
    }

    @PUT
    @Path("/investor/{name}")
    public Response putInvestor(@PathParam("name") String name, String password, String zone) {
        synchronized (this){
            Investor i = new Investor(this.idCounter.incrementAndGet(), name, password,zone);
            this.investors.put(name, i);
            writeInvestors();
            writeCounter();
            return Response.ok(i.getId()).build();
            /*
            if(this.investors.containsKey(inv.getName())) {
                this.investors.put(inv.getName(), inv);
                return Response.status(Response.Status.OK).build();
            }
            else {
                int Id = this.idCounter.incrementAndGet();
                inv.setId(Id);
                this.investors.put(inv.getName(),inv);
                return Response.ok(Id).build();
            }*/
        }
    }
    @DELETE
    @Path("/company/{name}")
    public Response deleteCompany(@PathParam("name") String name) {
        if (this.companies.containsKey(name)) {
            this.companies.remove(name);
            writeInvestors();
            return Response.ok(true).build();
        }
        else return Response.noContent().build();//verificar se n tem auction ativa
    }

    @DELETE
    @Path("/investor/{name}")
    public Response deleteInvestor(@PathParam("name") String name) {
        if (this.investors.containsKey(name)) {
            this.investors.remove(name);
            writeInvestors();
            return Response.ok().build();
        }
        else return Response.status(Response.Status.NOT_FOUND).build();
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

            Company company = this.getCompanyById( auction.getCompany() );

            if ( company == null ) {
                return Response.status( Response.Status.BAD_REQUEST ).build();
            }

            company.addAuction( Id );

            auction.setId(Id);

            this.auctions.put(Id, auction);
            writeAuctions();
            writeCompanies();
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
    /*
    @POST
    @Path("/auction/{id}")
    public Response putAuction ( @PathParam( "id" ) int Id, Auction auction ){
        synchronized ( this ) {
            auction.setId( Id );

            this.auctions.put( Id, auction );
            writeAuctions();

            return Response.ok(auction).build();
        }
    }
    */

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
            writeEmissions();

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
            writeEmissions();

            return Response.ok( emission ).build();
        }
    }
}
