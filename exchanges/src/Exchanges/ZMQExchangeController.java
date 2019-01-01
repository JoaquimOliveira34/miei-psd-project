package Exchanges;

import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.util.List;
import java.util.Timer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledThreadPoolExecutor;


public class ZMQExchangeController implements ExchangeController {
    private Exchange    exchange;
    private ZMQ.Context context;
    private ZMQ.Socket  socket;
    private ZMQ.Socket  publisher;
    private int         port;

    public ZMQExchangeController ( int port ) {
        this.exchange = new Exchange();
        this.context = ZMQ.context( 1 );
        this.socket = context.socket( ZMQ.REP );
        this.publisher = context.socket( ZMQ.PUB );
        this.port = port;

        this.exchange.setController( this );
    }

    @Override
    public void auctionCreated ( Auction auction ) {
        // TODO
    }

    @Override
    public void auctionClosed ( int company, List< AuctionBidding > biddings ) {
        // TODO
    }

    @Override
    public void emissionCreated ( Emission emission ) {
        // TODO
    }

    @Override
    public void emissionClosed ( int company, List< EmissionSubscription > subscriptions ) {
        // TODO
    }

    public void onCompanyMessage ( Protos.MsgCompany message ) throws ExchangeException {
        if ( message.getType() == Protos.MsgCompany.Type.AUCTION ) {
            this.exchange.createAuction( message.getCompanyId(), message.getAmount(), ( double ) message.getRate() );
        } else if ( message.getType() == Protos.MsgCompany.Type.FIXEDRATE ) {
            this.exchange.createEmission( message.getCompanyId(), message.getAmount() );
        }
    }

    public void onInvestorMessage ( Protos.MsgInvestor message ) throws ExchangeException {
        int company  = message.getCompany();
        int investor = message.getInvestorId();
        int amount   = message.getAmount();

        // Each company can only have at most one auction or one emission running at each moment in time
        // So we check which one is running right now and act accordingly
        if ( this.exchange.hasAuctionFor( company ) ) {
            this.exchange.bidAuction( company, investor, amount, message.getRate() );
        } else if ( this.exchange.hasEmissionFor( company ) ) {
            this.exchange.subscribeEmission( company, investor, amount );
        } else {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }
    }

    public void sendSuccess () {
        this.socket.send( Protos.ServerResponse.newBuilder().setResponse( true ).build().toByteArray() );
    }

    public void sendError ( String error ) {
        socket.send(
                Protos.ServerResponse.newBuilder()
                        .setResponse( false )
                        .setError( error )
                        .build().toByteArray()
        );
    }

    public void run () {
        this.socket.bind( "tcp://*:" + Integer.toString( port ) );

        while ( true ) {
            byte[] bytes = socket.recv();

            try {
                Protos.MsgExchange message = Protos.MsgExchange.parseFrom( bytes );

                // Can receive two types of messages from the same socket: decide which one it is and call the
                // appropriate method
                if ( message.getType() == Protos.MsgExchange.Type.COMPANY ) {
                    this.onCompanyMessage( message.getCompany() );
                } else if ( message.getType() == Protos.MsgExchange.Type.INVESTOR ) {
                    this.onInvestorMessage( message.getInvestor() );
                }

                this.sendSuccess();
            } catch ( InvalidProtocolBufferException e ) {
                e.printStackTrace();

                // There was an error with the parsing of the message. Send a response to the socket saying so
                this.sendError( ExchangeExceptionType.InvalidMessage.toString() );
            } catch ( ExchangeException e ) {
                this.sendError( e.getMessage() );
            }
        }
    }
}
