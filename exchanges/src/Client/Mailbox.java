package Client;

import Exchanges.Protos;
import org.zeromq.ZMQ;

import java.io.DataInputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Mailbox {
    private final ZMQ.Context context;

    // Native JAVA socket to communicate with erlang
    private final Socket messagesSocket;

    // Sub socket that receives any messages this client has subscribed for
    private final ZMQ.Socket notificationsSocket;

    // Receives commands from the client (like shutdown, subscribe, unsubscribe, etc...)
    private final ZMQ.Socket commandsSocketReader;

    // Receives commands from the client (like shutdown, subscribe, unsubscribe, etc...)
    private final ZMQ.Socket commandsSocketWriter;

    private final BlockingQueue< Protos.ServerResponse > responses = new LinkedBlockingQueue<>();

    // Unbounded queue that holds any received, yet to be read, messages
    private final ConcurrentLinkedQueue<String> messages = new ConcurrentLinkedQueue<>();

    // Unbounded queue that holds any received, yet to be read, notifications
    private final ConcurrentLinkedQueue< String > notifications = new ConcurrentLinkedQueue<>();

    private boolean waitingResponse = true;

    private Runnable onReceiveHandler = null;

    private boolean listening = false;

    public Mailbox ( ZMQ.Context context, Socket socket, int proxyPort ) {
        this.context = context;

        this.messagesSocket = socket;

        this.notificationsSocket = context.socket( ZMQ.SUB );

        this.commandsSocketReader = context.socket( ZMQ.PAIR );

        this.commandsSocketWriter = context.socket( ZMQ.PAIR );

        this.notificationsSocket.connect( "tcp://localhost:" + proxyPort );

        this.commandsSocketReader.bind( "inproc://mailbox-" + this.hashCode() );

        this.commandsSocketWriter.connect( "inproc://mailbox-" + this.hashCode() );
    }

    public void setOnReceive ( Runnable onReceive ) {
        synchronized ( this ) {
            this.onReceiveHandler = onReceive;
        }
    }

    public Runnable getOnReceive () {
        synchronized ( this ) {
            return onReceiveHandler;
        }
    }

    public void setWaitingResponse ( boolean waitingResponse ) {
        synchronized ( this ) {
            this.waitingResponse = waitingResponse;
        }
    }

    public boolean isWaitingResponse () {
        synchronized ( this ) {
            return waitingResponse;
        }
    }

    public int getResponsesCount () {
        return this.responses.size();
    }

    // Since responses is a LinkedBlockingQueue, the 'take()' method blocks if the queue is empty (and thus has no response)
    // until one response is added to the queue
    public Protos.ServerResponse readResponse () {
        try {
            return this.responses.take();
        } catch ( InterruptedException e ) {
            return null;
        }
    }

    public List<Protos.ServerResponse> readAllResponses () {
        List< Protos.ServerResponse > list = new ArrayList<>();

        Protos.ServerResponse message;

        // Poll does not block. Returns the last item, or null when the queue is empty
        while ( ( message = this.responses.poll() ) != null ) {
            list.add( message );
        }

        return list;
    }

    public int getMessageCount () {
        return this.messages.size();
    }

    public List< String > readMessages () {
        List< String > list = new ArrayList<>();

        String message;

        // Poll does not block. Returns the last item, or null when the queue is empty
        while ( ( message = this.messages.poll() ) != null ) {
            list.add( message );
        }

        return list;
    }

    public int getNotificationsCount () {
        return this.notifications.size();
    }

    public List< String > readNotifications () {
        List< String > list = new ArrayList<>();

        String notification;

        // Poll does not block. Returns the last item, or null when the queue is empty
        while ( ( notification = this.notifications.poll() ) != null ) {
            list.add( notification );
        }


        return list;
    }

    public void subscribe ( int topic ) {
        this.subscribe( Integer.toString( topic ) );
    }

    public void subscribe ( String topic ) {
        this.commandsSocketWriter.send( "sub " + topic );
    }

    public void unsubscribe ( int topic ) {
        this.unsubscribe( Integer.toString( topic ) );
    }

    public void unsubscribe ( String topic ) {
        this.commandsSocketWriter.send( "unsub " + topic );
    }

    // Runs in the auxiliary thread; beware of parallelism
    private void onReceive () {
        Runnable handler = this.getOnReceive();

        if ( handler != null ) {
            handler.run();
        }
    }

    // Runs in the auxiliary thread; beware of parallelism
    private void onMessage ( Protos.ServerResponse message ) {
        if ( this.isWaitingResponse() ) {
            this.responses.add( message );
        } else {
            if ( message.getError() != null && !message.getError().equals( "" ) ) {
                this.messages.add( "[error] " + message.getError() );
            } else {
                this.messages.add( message.getResponse() );
            }

            this.onReceive();
        }
    }

    // Runs in the auxiliary thread; beware of parallelism
    private void onNotification ( String notification ) {
        this.notifications.add( notification );

        this.onReceive();
    }

    // Runs in the auxiliary thread; beware of parallelism
    private boolean onCommand ( String command ) {
        if ( command.equals( "close" ) ) {
            return false;
        }

        if ( command.startsWith( "sub " ) ) {
            this.notificationsSocket.subscribe( command.substring( "sub ".length() ) );
        } else if ( command.startsWith( "unsub " ) ) {
            this.notificationsSocket.unsubscribe( command.substring( "unsub ".length() ) );
        } else {
            System.err.println( "Mailbox received unknown command: " + command );
        }

        return true;
    }

    // Runs in the auxiliary thread; beware of parallelism
    private void zmqEventLoop () {
        ZMQ.Poller poller = this.context.poller( 2 );

        poller.register( this.notificationsSocket );

        poller.register( this.commandsSocketReader );

        boolean running = true;

        while ( running ) {
            poller.poll();

            if ( poller.pollin( 0 ) ) {
                byte[] bytes = this.notificationsSocket.recv( ZMQ.DONTWAIT );

                if ( bytes != null ) {
                    this.onNotification( new String( bytes ) );
                }
            }


            if ( poller.pollin( 1 ) ) {
                byte[] bytes = this.commandsSocketReader.recv( ZMQ.DONTWAIT );

                if ( bytes != null ) {
                    running = this.onCommand( new String( bytes ) );
                }
            }
        }

        poller.close();
    }

    // Runs in the auxiliary thread; beware of parallelism
    private void netEventLoop () {
        try {
            DataInputStream stream = new DataInputStream( this.messagesSocket.getInputStream() );


            final byte[] b1 = new byte[ 5 ];

            boolean open = true;

            while ( open ) {
                // get size of message
                stream.readFully( b1 );
                int msgSize = Protos.IntMessage.parseFrom( b1 ).getValue();

                //get message
                byte[] b2 = new byte[ msgSize ];
                stream.readFully( b2 );

                this.onMessage( Protos.ServerResponse.parseFrom( b2 ) );
            }
        } catch ( Exception e ) {
            e.printStackTrace();
        }
    }

    public void start () {
        if ( this.listening ) return;

        this.listening = true;

        Thread zmqThread = new Thread( this::zmqEventLoop );

        Thread netThread = new Thread( this::netEventLoop );

        zmqThread.start();

        netThread.start();
    }

    private void close () {
        if ( !this.listening ) return;

        this.listening = false;

        this.commandsSocketWriter.send( "close" );

        // TODO enable close
        // this.commandsSocketWriter.close();
    }
}
