package Exchanges;

public enum ExchangeExceptionType {
    InvalidMessage,

    InvalidCompany,
    InvalidInterestRate,
    InvalidAmount,

    DuplicateBidding,
    DuplicateAuction,

    DuplicateEmission,
    DuplicateSubscription,

    // When there is already an auction and tries to create an emission
    // Or vice-versa
    DuplicateEvents,

    DirectoryError,
};
