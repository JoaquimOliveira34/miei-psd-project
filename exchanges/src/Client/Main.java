package Client;

import java.io.IOException;

public class Main {

    public static void main( String[] args ) throws IOException {
        /*
            Receive from args
                args[0] - Address  where client will connect to erlang server
                args[1} - Address where client will connect to Proxy server ( notifications )


        if( args.length < 2 ){
            System.out.println("Numero de argumentos invalido");
            return ;
        }
         */


        new Client( 12345,  1111 );
    }
}
