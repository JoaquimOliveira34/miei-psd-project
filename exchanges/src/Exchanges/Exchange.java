package Exchanges;

import java.util.List;
import java.util.Map;

interface ExchangeController {
    void auctionClosed ( int company, List<Bidding> biddings );
}

public class Exchange {
    private Map<Integer, Auction> auctions;
    private List<Emission> emissions;

    private ExchangeController controller;

    public void bid ( int company, int investor, int amount, double rate ) throws ExchangeException {
        if ( !this.auctions.containsKey( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Auction auction = this.auctions.get( company );

        auction.bid( investor, amount, rate );
    }

    public void closeAuction ( int company ) {
        if ( !this.auctions.containsKey( company ) ) {
            throw new ExchangeException( ExchangeExceptionType.InvalidCompany );
        }

        Auction auction = this.auctions.get( company );

        List<Bidding> accepted = auction.close();

        this.auctions.remove( company );

        if ( this.controller != null ) {
            this.controller.auctionClosed( company, accepted );
        }
    }
}
