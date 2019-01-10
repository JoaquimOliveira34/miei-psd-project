package Exchanges;

public class Main {
    // Usage: <pullPort> <pushPort> <publisherPort> <directoryAddress> <directoryPort>
    public static void main ( String[] args ) {
        DirectoryClient directory = new DirectoryClient( args[ 3 ], Integer.parseInt( args[ 4 ] ) );


        ZMQExchangeController controller = new ZMQExchangeController( Integer.parseInt( args[ 0 ] ), Integer.parseInt( args[ 1 ] ), Integer.parseInt( args[ 2 ] ), directory );

        controller.run();
    }
}
