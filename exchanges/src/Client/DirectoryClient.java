package Client;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

import java.io.IOException;
import java.util.*;

public class DirectoryClient {
    private int          port;
    private String       address;
    private OkHttpClient client;

    public DirectoryClient ( String address, int port ) {
        this.address = address;
        this.port = port;
        this.client = new OkHttpClient();
    }

    protected String getResourceUrl ( String... segments ) {
        return String.format( "http://%s:%d/peerlending/%s", this.address, this.port, String.join( "/", segments ) );
    }

    protected <T> List<T> readListOf ( String json ) throws IOException {
        ObjectMapper mapper = new ObjectMapper();

        return mapper.readValue( json, new TypeReference<List<T>>(){} );
    }

    public List< Company > listCompanies () throws IOException {
        String url = this.getResourceUrl( "companies" );

        Request request = new Request.Builder()
                .url( url ).get().build();

        try ( Response response = client.newCall( request ).execute() ) {
            String string = response.body().string();

            return readListOf( string );
        }
    }

    public List< Auction > listAuctions () throws IOException {
        String url = this.getResourceUrl( "auctions" );

        Request request = new Request.Builder()
                .url( url ).get().build();

        try ( Response response = client.newCall( request ).execute() ) {
            String string = response.body().string();

            return readListOf( string );
        }
    }

    // -- Entities

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

    public static class Auction {
        private int    id;
        private int    company;
        private int    amount;
        private double maxInterestRate;
        private boolean closed = false;

        private List< AuctionBidding >         biddings = null;

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

        public List< AuctionBidding > getBiddings () {
            return this.biddings;
        }
    }

}
