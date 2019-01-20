package Proxy;
import org.zeromq.ZMQ;

public class Broker {

    public static void main(String[] args) {

        if( args.length < 2){
            args = new String[2];
            args[0] = "9901";
            args[1] = "9902";
        }

        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket pubs = context.socket(ZMQ.XSUB);
        ZMQ.Socket subs = context.socket(ZMQ.XPUB);
        pubs.bind("tcp://localhost:"+args[0]);
        subs.bind("tcp://localhost:"+args[1]);

        for ( int i = 2; i < args.length; i++ ) {
            pubs.connect( "tcp://localhost:" + args[ i ] );
        }

        new Proxy(context, pubs, subs).poll();
    }
}
