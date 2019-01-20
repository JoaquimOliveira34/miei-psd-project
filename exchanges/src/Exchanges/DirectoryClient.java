package Exchanges;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.*;
import okhttp3.*;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;

/*
GET /empresas
	Lista todas as empresas
GET /empresa/<nome>
	Lista histórico de empresa com um dado nome
PUT /empresa/<nome>
	Atualiza informação sobre empresa
	Insere se esta não existe
DELETE /empresa/<nome>
	Remove cliente com papel de empresa
	Não permitido se este estiver com um leilão em curso. (Conflito 409)
GET /leiloes
	Lista leilões em curso
POST /leiloes
	Acrescenta novo leilão com determinado montante e taxa
	Retorna ID
POST /leilao/<id>
	Insere bid num determinado leilão através do identificador
	Não permitir se leilão não existir.
DELETE /leilao/<id>
	Apaga leilão com determinado identificador
	Não permitido se não estiver terminado o leilão(Conflito 409)
GET /emissoes
	Lista emissões em curso
DELETE /emissao/<id>
	Apaga emissão com determinado identificador
	Não permitido se não estiver terminado a emissão. (Conflito 409)
POST /emissoes
	Acrescenta novo leilão com determinado montante e taxa
	Retorna ID
 */

class JsonBuilder {
    public static final MediaType JSON = MediaType.parse( "application/json; charset=utf-8" );

    private ObjectMapper mapper = new ObjectMapper();
    private ObjectNode   root   = this.mapper.createObjectNode();

    public ObjectNode getObjectNode () {
        return root;
    }

    public JsonBuilder add ( String field, String value ) {
        this.root.set( field, TextNode.valueOf( value ) );

        return this;
    }

    public JsonBuilder add ( String field, int value ) {
        this.root.set( field, IntNode.valueOf( value ) );

        return this;
    }

    public JsonBuilder add ( String field, double value ) {
        this.root.set( field, DoubleNode.valueOf( value ) );

        return this;
    }

    public JsonBuilder add ( String field, boolean value ) {
        this.root.set( field, BooleanNode.valueOf( value ) );

        return this;
    }

    public JsonBuilder add ( String field, float value ) {
        this.root.set( field, FloatNode.valueOf( value ) );

        return this;
    }

    public JsonBuilder add ( String field, Collection< ? > values ) {
        if ( values != null ) {
            ArrayNode node = this.mapper.createArrayNode();

            values.forEach( value -> {
                if ( value instanceof JsonBuilder ) {
                    node.add( ( ( JsonBuilder ) value ).getObjectNode() );
                } else {
                    node.add( this.mapper.valueToTree( value ) );
                }
            } );

            this.root.set( field, node );
        } else {
            this.root.set( field, NullNode.getInstance() );
        }

        return this;
    }

    public JsonBuilder add ( String field, JsonBuilder value ) {
        this.root.set( field, value.getObjectNode() );

        return this;
    }

    public String toString () {
        ObjectMapper mapper = new ObjectMapper();

        try {
            return mapper.writeValueAsString( this.root );
        } catch ( JsonProcessingException e ) {
            return "{}";
        }
    }

    public RequestBody build () {
        // This causes the warning: unknown enum constant DeprecationLevel.ERROR
        return RequestBody.create( JSON, this.toString().getBytes() );
    }
}

public class DirectoryClient {
    private int                    port;
    private String                 address;
    private OkHttpClient           client;
    private ExecutorService        threadPool;

    public DirectoryClient ( String address, int port ) {
        this.address = address;
        this.port = port;
        this.client = new OkHttpClient();
        // New thread pool that starts out with 2 threads, maxs out at 16 threads before queuing tasks
        // Each unused thread is kept alive for two seconds before being discarded
        // And finally queued tasks are stored in an unbounded LinkedList
        this.threadPool = new ThreadPoolExecutor( 2, 16, 2, TimeUnit.SECONDS, new LinkedBlockingQueue<>() );
    }

    public boolean companyExists ( int id ) throws IOException {
        String url = this.getResourceUrl( "company", "id", Integer.toString( id ) );

        Request request = new Request.Builder()
                .url( url )
                .get()
                .build();

        try ( Response response = client.newCall( request ).execute() ) {
            int code = response.code();

            if ( code < 200 || code > 299 ) {
                return false;
            }

            return true;
        }
    }

    @FunctionalInterface
    interface IOSupplier < T > {
        T get () throws IOException;
    }

    @FunctionalInterface
    interface IORunnable {
        void run () throws IOException;
    }

    private CompletableFuture< Void > executeAsync ( IORunnable runnable ) {
        return this.executeAsync( () -> {
            runnable.run();

            return null;
        } );
    }

    private < T > CompletableFuture< T > executeAsync ( IOSupplier< T > supplier ) {
        CompletableFuture< T > future = new CompletableFuture<>();

        this.threadPool.execute( () -> {
            try {
                future.complete( supplier.get() );
            } catch ( Exception e ) {
                future.completeExceptionally( e );
            }
        } );

        return future;
    }

    protected String getResourceUrl ( String... segments ) {
        return String.format( "http://%s:%d/peerlending/%s", this.address, this.port, String.join( "/", segments ) );
    }

    public Auction createAuction ( int company, int amount, double maxInterestRate ) throws IOException {
        String url = this.getResourceUrl( "auctions" );

        RequestBody requestBody = new JsonBuilder()
                .add( "company", company )
                .add( "amount", amount )
                .add( "maxInterestRate", maxInterestRate )
                .build();

        Request request = new Request.Builder()
                .url( url )
                .post( requestBody )
                .build();

        try ( Response response = client.newCall( request ).execute() ) {
            int code = response.code();

            String string = response.body().string();

            if ( code < 200 || code > 299 ) {
                System.out.println( string + code );
            }

            return Auction.fromJSON( string );
        }
    }

    public Auction createAuction ( Auction auction ) throws IOException {
        return this.createAuction( auction.getCompany(), auction.getAmount(), auction.getMaxInterestRate() );
    }

    public void closeAuction ( Auction auction, List< AuctionBidding > biddings ) {
        String url = this.getResourceUrl( "auction", Integer.toString( auction.getId() ) );

        RequestBody requestBody = new JsonBuilder()
                .add( "company", auction.getCompany() )
                .add( "amount", auction.getAmount() )
                .add( "maxInterestRate", auction.getMaxInterestRate() )
                .add( "closed", true )
                .add( "biddings", biddings )
                .build();

        Request request = new Request.Builder()
                .url( url )
                .put( requestBody )
                .build();

        try {
            client.newCall( request ).execute();
        } catch ( IOException e ) {
            e.printStackTrace();
        }
    }


    public Emission createEmission ( int company, int amount, double interestRate ) throws IOException {
        String url = this.getResourceUrl( "emissions" );

        RequestBody requestBody = new JsonBuilder()
                .add( "company", company )
                .add( "amount", amount )
                .add( "interestRate", interestRate )
                .build();

        Request request = new Request.Builder()
                .url( url )
                .post( requestBody )
                .build();

        try ( Response response = client.newCall( request ).execute() ) {
            int code = response.code();

            String string = response.body().string();

            if ( code < 200 || code > 299 ) {
                System.out.println( string + code );
            }

            return Emission.fromJSON( string );
        }
    }

    public Emission createEmission ( Emission emission ) throws IOException {
        return this.createEmission( emission.getCompany(), emission.getAmount(), emission.getInterestRate() );
    }

    public void closeEmission ( Emission emission, List< EmissionSubscription > subscribed ) {
        String url = this.getResourceUrl( "emission", Integer.toString( emission.getId() ) );

        RequestBody requestBody = new JsonBuilder()
                .add( "company", emission.getCompany() )
                .add( "amount", emission.getAmount() )
                .add( "interestRate", emission.getInterestRate() )
                .add( "subscriptions", subscribed )
                .build();

        Request request = new Request.Builder()
                .url( url )
                .put( requestBody )
                .build();

        try {
            client.newCall( request ).execute();
        } catch ( IOException e ) {
            e.printStackTrace();
        }
    }


    // ASYNC METHODS
    public CompletableFuture< Auction > createAuctionAsync ( Auction auction ) {
        return this.createAuctionAsync( auction.getCompany(), auction.getAmount(), auction.getMaxInterestRate() );
    }

    public CompletableFuture< Auction > createAuctionAsync ( final int company, final int amount, final double maxInterestRate ) {
        return this.executeAsync( () -> this.createAuction( company, amount, maxInterestRate ) );
    }

    public CompletableFuture< Void > closeAuctionAsync ( Auction auction, List< AuctionBidding > biddings ) {
        return this.executeAsync( () -> this.closeAuction( auction, biddings ) );
    }

    public CompletableFuture< Emission > createEmissionAsync ( Emission emission ) {
        return this.createEmissionAsync( emission.getCompany(), emission.getAmount(), emission.getInterestRate() );
    }

    public CompletableFuture< Emission > createEmissionAsync ( final int company, final int amount, final double interestRate ) {
        return this.executeAsync( () -> this.createEmission( company, amount, interestRate ) );
    }

    public CompletableFuture< Void > closeEmissionAsync ( Emission emission, List< EmissionSubscription > subscribed ) {
        return this.executeAsync( () -> this.closeEmission( emission, subscribed ) );
    }
}
