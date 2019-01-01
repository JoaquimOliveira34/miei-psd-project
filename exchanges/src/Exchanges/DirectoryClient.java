package Exchanges;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.*;
import okhttp3.*;

import java.io.IOException;
import java.util.Collection;
import java.util.List;

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
        ArrayNode node = this.mapper.createArrayNode();

        values.forEach( value -> {
            if ( value instanceof JsonBuilder ) {
                node.add( ( ( JsonBuilder ) value ).getObjectNode() );
            } else {
                node.add( this.mapper.valueToTree( value ) );
            }
        } );

        this.root.set( field, node );

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
        // This causes de warning: unknown enum constant DeprecationLevel.ERROR
        return RequestBody.create( JSON, this.toString().getBytes() );
    }
}

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
        return String.format( "http://%s:%d/", this.address, this.port, String.join( "/", segments ) );
    }

    public Auction createAuction ( int company, int amount, double maxInterestRate ) throws IOException {
        String url = this.getResourceUrl( "leiloes" );

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
            return Auction.fromJSON( response.body().string() );
        }
    }

    public Auction createAuction ( Auction auction ) throws IOException {
        return this.createAuction( auction.getCompany(), auction.getAmount(), auction.getMaxInterestRate() );
    }

    public void closeAuction ( Auction auction, List< AuctionBidding > biddings ) {
        String url = this.getResourceUrl( "leilao", Integer.toString( auction.getId() ) );

        RequestBody requestBody = new JsonBuilder()
                .add( "biddings", biddings )
                .build();

        Request request = new Request.Builder()
                .url( url )
                .post( requestBody )
                .build();

        try {
            client.newCall( request ).execute();
        } catch ( IOException e ) {
            e.printStackTrace();
        }
    }


    public Emission createEmission ( int company, int amount, double interestRate ) throws IOException {
        String url = this.getResourceUrl( "emissoes" );

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
            return Emission.fromJSON( response.body().string() );
        }
    }

    public Emission createEmission ( Emission emission ) throws IOException {
        return this.createEmission( emission.getCompany(), emission.getAmount(), emission.getInterestRate() );
    }

    public void closeEmission ( Emission emission, List<EmissionSubscription> subscribed ) {
        // TODO
    }

    public Emission getLastEmission ( int company ) {
        // TODO
        return null;
    }

    public Auction getLastAuction ( int company ) {
        // TODO
        return null;
    }

    public List<AuctionBidding> getAuctionBiddings ( int id ) {
        // TODO
        return null;
    }
}
