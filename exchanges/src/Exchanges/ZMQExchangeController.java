package Exchanges;

import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.util.List;


public class ZMQExchangeController implements ExchangeController {
    private Exchange    exchange;
    private ZMQ.Context context;
    private ZMQ.Socket  replySocket;
    private ZMQ.Socket  publisher;
    private ZMQ.Socket  schedulerSocket;
    private ZMQ.Socket  schedulerSocketPush;
    private int         replyPort;
    private int         publisherPort;

    public ZMQExchangeController ( int replyPort, int publisherPort, DirectoryClient directory ) {
        this.context = ZMQ.context( 1 );

        // Socket that receives requests from the erlang frontend and sends replies
        this.replySocket = context.socket( ZMQ.REP );

        // Socket responsible for publishing notifications
        this.publisher = context.socket( ZMQ.PUB );

        // Socket that receives timeout notifications to close auctions/emissions
        this.schedulerSocket = context.socket( ZMQ.PULL );

        // Socket that notifies the exchange controller's event loop when
        // an auction or emission should be closed
        this.schedulerSocketPush = context.socket( ZMQ.PUSH );


        this.replyPort = replyPort;

        this.publisherPort = publisherPort;


        this.replySocket.bind( "tcp://*:" + Integer.toString( replyPort ) );

        this.schedulerSocket.bind( "inproc://scheduler" );

        this.schedulerSocketPush.connect( "inproc://scheduler" );

        this.publisher.bind( "tcp://*:" + Integer.toString( publisherPort ) );


        this.exchange = new Exchange( directory, this );
    }

    private void publish ( int company, String message ) {
        this.publisher.send( Protos.Notification.newBuilder().setCompany( company ).setMessage( message ).build().toByteArray() );
    }

    private void publish ( int company, String format, Object ... args ) {
        this.publish( company, String.format( format, args ) );
    }

    @Override
    public void auctionCreated ( Auction auction ) {
        this.publish( auction.getCompany(), "Auction created with ammount %d and max interest rate %d.", auction.getAmount(), auction.getMaxInterestRate() );
    }

    @Override
    public void auctionClosed ( int company, List< AuctionBidding > biddings ) {
        if ( biddings != null && biddings.size() > 0 ) {
            this.publish( company, "Auction closed with enough biddings." );
        } else {
            this.publish( company, "Auction closed without enough biddings." );
        }
    }

    @Override
    public void emissionCreated ( Emission emission ) {
        this.publish( emission.getCompany(), "Emission created with ammount %d and fixed interest rate %d.", emission.getAmount(), emission.getInterestRate() );
    }

    @Override
    public void emissionClosed ( int company, List< EmissionSubscription > subscriptions ) {
        this.publish( company, "Emission closed with %d collected.", subscriptions.stream().mapToInt( EmissionSubscription::getAmount ).sum() );
    }

    @Override
    public void scheduleClose ( int company ) {
        this.schedulerSocketPush.send( intToByteArray( company ) );
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
        this.replySocket.send( Protos.ServerResponse.newBuilder().setResponse( true ).build().toByteArray() );
    }

    public void sendError ( String error ) {
        replySocket.send(
                Protos.ServerResponse.newBuilder()
                        .setResponse( false )
                        .setError( error )
                        .build().toByteArray()
        );
    }

    public void run () {
        // Here we create a poller that allows us to listen to more than one socket at a time
        // The first socket receives messages from the erlang frontend
        // The second one notifies us when an auction/emission is ready to be closed
        ZMQ.Poller poller = this.context.poller( 1 );

        poller.register( this.replySocket, ZMQ.Poller.POLLIN );
        poller.register( this.schedulerSocket, ZMQ.Poller.POLLIN );

        System.out.printf( ">> Exchange listening on port %d (notifications on port %d)\n", this.replyPort, this.publisherPort );

        while ( true ) {
            poller.poll();

            if ( poller.pollin( 0 ) ) {
                byte[] bytes = replySocket.recv( ZMQ.DONTWAIT );

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

            // We check if the 1-indexed socket (schedulerSocket) has any incoming messages
            // If so, we read the company id and close the auction
            if ( poller.pollin( 1 ) ) {
                byte[] bytes = replySocket.recv( ZMQ.DONTWAIT );

                int company = byteArrayToInt( bytes );

                try {
                    if ( this.exchange.hasAuctionFor( company ) ) {
                        this.exchange.closeAuction( company );
                    } else if ( this.exchange.hasEmissionFor( company ) ) {
                        this.exchange.closeEmission( company );
                    }
                } catch ( ExchangeException e ) {
                    e.printStackTrace();
                }
            }
        }
    }


    // Helper methods to convert from int to byte[] and vice-versa
    // Taken from https://stackoverflow.com/questions/5399798/byte-array-and-int-conversion-in-java/5399829#5399829
    public static int byteArrayToInt ( byte[] b ) {
        return b[ 3 ] & 0xFF |
               ( b[ 2 ] & 0xFF ) << 8 |
               ( b[ 1 ] & 0xFF ) << 16 |
               ( b[ 0 ] & 0xFF ) << 24;
    }

    public static byte[] intToByteArray ( int a ) {
        return new byte[] {
                ( byte ) ( ( a >> 24 ) & 0xFF ),
                ( byte ) ( ( a >> 16 ) & 0xFF ),
                ( byte ) ( ( a >> 8 ) & 0xFF ),
                ( byte ) ( a & 0xFF )
        };
    }
}
