package Exchanges;

public enum ExchangeExceptionType {
    InvalidMessage,

    InvalidCompany,
    InvalidInterestRate,
    InvalidAmount,

    AntiCompetitiveBid,

    DuplicateAuction,

    DuplicateEmission,

    // When there is already an auction and tries to create an emission
    // Or vice-versa
    DuplicateEvents,

    DirectoryError,
};
