package Exchanges;

public class Main {
    // Usage: <replyPort> <publisherPort> <directoryAddress> <directoryPort>
    public static void main ( String[] args ) {
        DirectoryClient directory = new DirectoryClient( args[ 2 ], Integer.parseInt( args[ 3 ] ) );

        ZMQExchangeController controller = new ZMQExchangeController( Integer.parseInt( args[ 0 ] ), Integer.parseInt( args[ 1 ] ), directory );

        controller.run();
    }
}
