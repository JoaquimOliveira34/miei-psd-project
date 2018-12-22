package Exchanges;

public class ExchangeException extends Exception {
    public ExchangeException ( ExchangeExceptionType error ) {
        super( error.toString() );
    }
}
