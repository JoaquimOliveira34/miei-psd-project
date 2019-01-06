package Proxy;
import org.zeromq.ZMQ;

class Proxy {
    private ZMQ.Poller items;
    private ZMQ.Socket s1, s2;

    Proxy(ZMQ.Context context, ZMQ.Socket s1, ZMQ.Socket s2) {
        this.s1 = s1;
        this.s2 = s2;
        items = context.poller(2);
        items.register(s1, ZMQ.Poller.POLLIN);
        items.register(s2, ZMQ.Poller.POLLIN);
    }

    void poll() {
        while (!Thread.currentThread().isInterrupted()) {
            items.poll();
            ZMQ.Socket from, to;

            if (items.pollin(0)) {
                from = s1; to = s2;
            } else {
                from = s2; to = s1;
            }

            while (true) {
                byte[] m = from.recv();
                if (from.hasReceiveMore()) {
                    to.sendMore(m);
                } else {
                    to.send(m);
                    break;
                }
            }
        }
    }

}

