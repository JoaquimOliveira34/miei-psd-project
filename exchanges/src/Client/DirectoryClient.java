package Client;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DirectoryClient {
    private int                    port;
    private String                 address;
    private OkHttpClient           client;
    private Map< String, Integer > companyIds;
    private Map< Integer, String > companyNames;

    public DirectoryClient ( String address, int port ) {
        this.address = address;
        this.port = port;
        this.client = new OkHttpClient();
        this.companyIds = new HashMap<>();
        this.companyNames = new HashMap<>();
    }

    protected String getResourceUrl ( String... segments ) {
        return String.format( "http://%s:%d/peerlending/%s", this.address, this.port, String.join( "/", segments ) );
    }

    protected < T > List< T > readListOf ( String json, TypeReference< List< T > > type ) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.readValue( json, type );
    }

    public List< Company > listCompanies () throws IOException {
        String url = this.getResourceUrl( "companies" );

        Request request = new Request.Builder()
                .url( url ).get().build();

        try ( Response response = client.newCall( request ).execute() ) {
            int code = response.code();

            if ( code < 200 || code > 299 ) {
                throw new IOException( "Cannot get company: HTTP " + code );
            }

            String string = response.body().string();

            return readListOf( string, new TypeReference< List< Company > >() { } );
        }
    }

    public List< Auction > listAuctions () throws IOException {
        String url = this.getResourceUrl( "auctions" );

        Request request = new Request.Builder()
                .url( url ).get().build();

        try ( Response response = client.newCall( request ).execute() ) {
            int code = response.code();

            if ( code < 200 || code > 299 ) {
                throw new IOException( "Cannot get company: HTTP " + code );
            }

            String string = response.body().string();

            return readListOf( string, new TypeReference< List< Auction > >() { } );
        }
    }

    public List< Emission > listEmissions () throws IOException {
        String url = this.getResourceUrl( "emissions" );

        Request request = new Request.Builder()
                .url( url ).get().build();

        try ( Response response = client.newCall( request ).execute() ) {
            int code = response.code();

            if ( code < 200 || code > 299 ) {
                throw new IOException( "Cannot get company: HTTP " + code );
            }

            String string = response.body().string();

            return readListOf( string, new TypeReference< List< Emission > >() { } );
        }
    }

    public Company getCompany ( String name ) throws IOException {
        String url = this.getResourceUrl( "company", name );

        Request request = new Request.Builder()
                .url( url ).get().build();

        try ( Response response = client.newCall( request ).execute() ) {
            int code = response.code();

            if ( code < 200 || code > 299 ) {
                throw new IOException( "Cannot get company: HTTP " + code );
            }

            String string = response.body().string();

            ObjectMapper mapper = new ObjectMapper();

            return mapper.readValue( string, Company.class );
        }
    }

    public int getCompanyId ( String name ) throws IOException {
        if ( this.companyIds.containsKey( name.toLowerCase() ) ) {
            return this.companyIds.get( name.toLowerCase() );
        }

        Company company = this.getCompany( name );

        this.companyIds.put( company.getName().toLowerCase(), company.getId() );
        this.companyNames.put( company.getId(), company.getName() );

        return company.getId();
    }

    public String getCompanyName ( Integer id ) {
        if ( this.companyNames.containsKey( id ) ) {
            return this.companyNames.get( id );
        }

        return Integer.toString( id );
    }


    // -- Entities
    @JsonIgnoreProperties( ignoreUnknown = true )
    public static class Company {
        private int Id;

        private String name;

        private String zone;

        public int getId () {
            return Id;
        }

        public void setId ( int id ) {
            Id = id;
        }

        public String getName () {
            return name;
        }

        public void setName ( String name ) {
            this.name = name;
        }

        public String getZone () {
            return zone;
        }

        public void setZone ( String zone ) {
            this.zone = zone;
        }
    }


    public static class AuctionBidding {
        private int    investor;
        private int    amount;
        private double interestRate;

        public AuctionBidding () {}

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

    @JsonIgnoreProperties( ignoreUnknown = true )
    public static class Auction {
        private int    id;
        private int    company;
        private int    amount;
        private double maxInterestRate;
        private boolean closed = false;

        private List< AuctionBidding > biddings = null;

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

        public boolean isClosed () {
            return this.closed;
        }

        public boolean isOpen () {
            return !this.isClosed();
        }

        public List< AuctionBidding > getBiddings () {
            return this.biddings;
        }
    }



    public static class EmissionSubscription {
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

    public static class Emission {
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

        public boolean isClosed () {
            return this.subscriptions != null && this.subscriptions.size() > 0;
        }

        public boolean isOpen () {
            return !this.isClosed();
        }
    }

}
