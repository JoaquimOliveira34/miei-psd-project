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
        pubs.bind("tcp://*:"+args[0]);
        subs.bind("tcp://*:"+args[1]);
        new Proxy(context, pubs, subs).poll();
    }
}
