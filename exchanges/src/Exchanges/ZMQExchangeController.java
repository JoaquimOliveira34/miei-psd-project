package Exchanges;

import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.util.Arrays;
import java.util.List;


public class ZMQExchangeController implements ExchangeController {
    private Exchange    exchange;
    private ZMQ.Context context;
    private ZMQ.Socket  pullSocket;
    private ZMQ.Socket  pushSocket;
    private ZMQ.Socket  publisher;
    private ZMQ.Socket  schedulerSocket;
    private ZMQ.Socket  schedulerSocketPush;
    private int         pullPort;
    private int         pushPort;
    private int         publisherPort;

    public ZMQExchangeController ( int pullPort, int pushPort, int publisherPort, DirectoryClient directory ) {
        this.context = ZMQ.context( 1 );

        // Socket that receives requests from the erlang frontend
        this.pullSocket = context.socket( ZMQ.PULL );

        // Socket that sends messages to the erlang frontend
        this.pushSocket = context.socket( ZMQ.PUSH );

        // Socket responsible for publishing notifications
        this.publisher = context.socket( ZMQ.PUB );

        // Socket that receives timeout notifications to close auctions/emissions
        this.schedulerSocket = context.socket( ZMQ.PULL );

        // Socket that notifies the exchange controller's event loop when
        // an auction or emission should be closed
        this.schedulerSocketPush = context.socket( ZMQ.PUSH );


        this.pullPort = pullPort;

        this.pushPort = pushPort;

        this.publisherPort = publisherPort;

        
        this.pullSocket.bind( "tcp://localhost:" + Integer.toString( pullPort ) );

        this.pushSocket.connect( "tcp://localhost:" + Integer.toString( pushPort ) );

        this.publisher.bind( "tcp://localhost:" + Integer.toString( publisherPort ) );

        this.schedulerSocket.bind( "inproc://scheduler" );

        this.schedulerSocketPush.connect( "inproc://scheduler" );


        this.exchange = new Exchange( directory, this );
    }

    public Exchange getExchange () {
        return this.exchange;
    }

    private void debug ( String action, String message ) {
        System.out.println( action + "------------" );
        System.out.println( message );
    }

    private void publish ( int company, String message ) {
        String string = String.format( "<comp:%d> %s", company, message );

        this.debug( "Publishing", string );

        this.publisher.send( string );
    }

    private void publish ( int company, String format, Object... args ) {
        this.publish( company, String.format( format, args ) );
    }

    @Override
    public void auctionCreated ( Auction auction ) {
        this.sendReply( auction.getCompany(), "Auction created successfully." );

        this.publish( auction.getCompany(), "Auction created for company <comp:%d> with ammount %d and max interest rate %f.", auction.getCompany(), auction.getAmount(), auction.getMaxInterestRate() );
    }

    @Override
    public void auctionBid ( Auction auction, AuctionBidding bidding ) {
        this.publish( auction.getCompany(), "Auction for company <comp:%d> new bid with ammount %d and rate %f.", auction.getCompany(), bidding.getAmount(), bidding.getInterestRate() );
    }

    @Override
    public void auctionBiddingInvalidated ( int company, int investor, AuctionBidding lowest ) {
        this.sendReply( investor, String.format( "Your bidding for company <comp:%d> was invalidated, lowest is now %d, %f.", company, lowest.getAmount(), lowest.getInterestRate() ) );
    }

    @Override
    public void auctionClosed ( int company, boolean success, List< AuctionBidding > biddings ) {
        if ( success ) {
            this.sendReply( company, "Auction closed with enough biddings." );
            this.publish( company, "Auction closed with enough biddings." );

            for ( AuctionBidding bidding : biddings ) {
                this.sendReply( bidding.getInvestor(), String.format( "Auction closed successfully at company <comp:%d>. Your bidding of %d at %f was approved.", company, bidding.getAmount(), bidding.getInterestRate() ) );
            }
        } else {
            this.sendReply( company, "Auction closed without enough biddings." );
            this.publish( company, "Auction closed without enough biddings." );

            for ( AuctionBidding bidding : biddings ) {
                this.sendReply( bidding.getInvestor(), String.format( "Auction closed unsuccessfully at company <comp:%d>. Your bidding of %d at %f was invalidated.", company, bidding.getAmount(), bidding.getInterestRate() ) );
            }
        }
    }

    @Override
    public void emissionCreated ( Emission emission ) {
        this.sendReply( emission.getCompany(), "Emission created successfully." );
        this.publish( emission.getCompany(), "Emission created for company <comp:%d> with ammount %d and fixed interest rate %f.", emission.getCompany(), emission.getAmount(), emission.getInterestRate() );
    }

    @Override
    public void emissionSubscribed ( Emission emission, EmissionSubscription subscription ) {
        this.publish( emission.getCompany(), "Emission for company <comp:%d> new subscription with ammount %d.", emission.getCompany(), subscription.getAmount() );
    }

    @Override
    public void emissionClosed ( int company, List< EmissionSubscription > subscriptions ) {
        this.publish( company, "Emission closed for company <comp:%d> with %d collected.", company, subscriptions.stream().mapToInt( EmissionSubscription::getAmount ).sum() );
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
            this.exchange.bidAuction( company, investor, amount, ( double ) message.getRate() );
        } else if ( this.exchange.hasEmissionFor( company ) ) {
            this.exchange.subscribeEmission( company, investor, amount );
        } else {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }
    }

    public void sendReply ( int user, String response ) {
        Protos.ServerResponse msg = Protos.ServerResponse.newBuilder()
                .setUserId( user )
                .setResponse( response )
                .build();

        this.debug( "Outgoing", msg.toString() );

        this.pushSocket.send( msg.toByteArray() );
    }

    public void sendError ( int user, String error ) {
        Protos.ServerResponse msg = Protos.ServerResponse.newBuilder()
                .setUserId( user )
                .setError( error )
                .build();

        System.out.println( msg );

        this.pushSocket.send( msg.toByteArray() );
    }

    public void run () {
        // Here we create a poller that allows us to listen to more than one socket at a time
        // The first socket receives messages from the erlang frontend
        // The second one notifies us when an auction/emission is ready to be closed
        ZMQ.Poller poller = this.context.poller( 2 );

        poller.register( this.pullSocket, ZMQ.Poller.POLLIN );
        poller.register( this.schedulerSocket, ZMQ.Poller.POLLIN );

        System.out.printf( ">> Exchange pulling on port %d, pushing on port %d (notifications on port %d)\n", this.pullPort, this.pushPort, this.publisherPort );

        int id = 0;

        while ( true ) {
            poller.poll();

            if ( poller.pollin( 0 ) ) {
                byte[] bytes = pullSocket.recv( ZMQ.DONTWAIT );

                try {
                    Protos.MsgExchange message = Protos.MsgExchange.parseFrom( bytes );

                    this.debug( "Incoming", message.toString() );

                    // Can receive two types of messages from the same socket: decide which one it is and call the
                    // appropriate method
                    if ( message.getType() == Protos.MsgExchange.Type.COMPANY ) {
                        id = message.getCompany().getCompanyId();

                        this.onCompanyMessage( message.getCompany() );
                    } else if ( message.getType() == Protos.MsgExchange.Type.INVESTOR ) {
                        id = message.getInvestor().getInvestorId();

                        this.onInvestorMessage( message.getInvestor() );
                    } else {
                        id = -1;
                    }
                } catch ( InvalidProtocolBufferException e ) {
                    e.printStackTrace();

                    // There was an error with the parsing of the message. Send a response to the socket saying so
                    this.sendError( id, ExchangeExceptionType.InvalidMessage.toString() );
                } catch ( ExchangeException e ) {
                    this.sendError( id, e.getMessage() );
                }
            }

            // We check if the 1-indexed socket (schedulerSocket) has any incoming messages
            // If so, we read the company id and close the auction
            if ( poller.pollin( 1 ) ) {
                byte[] bytes = schedulerSocket.recv( ZMQ.DONTWAIT );

                int company = byteArrayToInt( bytes );

                this.debug( "Incoming Close", Integer.toString( company ) );

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
