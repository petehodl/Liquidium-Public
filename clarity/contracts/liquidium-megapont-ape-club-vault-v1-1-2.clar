
;;;; liquidium-megapont-ape-club-vault-v1-1-2
;;;; nft: megapont-ape-club-nft
;;;; network: testnet
;;;; Manages loans and auctions for megapont ape club nft assets as collateral
;;;; Offical website that calls the public functions can be found here:
;;;; https://liquidium.finance/

;;;; Constant definitions

;;; Standard principal that deployed contract
;; (define-constant DEPLOYER_ACCOUNT tx-sender) 
(define-constant DEPLOYER_ACCOUNT 'ST2M5284BRBR3AVZXR2GD17AFZ6VKJ2ERH123XMVD) ;; TODO: only for testing


;;; Error codes
(define-constant ERR_AUCTION_INACTIVE (err u1000))
(define-constant ERR_LOAN_INACTIVE (err u1001))

(define-constant ERR_AUCTION_NOT_FOUND (err u2000))
(define-constant ERR_LOAN_NOT_FOUND (err u2001))

(define-constant ERR_AMOUNT_INVALID (err u3000))
(define-constant ERR_TERM_INVALID (err u3001))
(define-constant ERR_VALUE_INVALID (err u3002))
(define-constant ERR_ACCOUNT_INVALID (err u3003))

(define-constant ERR_ASSET_TRANSFER_FAILED (err u4000))
(define-constant ERR_STX_TRANSFER_FAILED (err u4001))

(define-constant ERR_AUCTION_ENDED (err u5000))
(define-constant ERR_LOAN_EXPIRED (err u5001))

(define-constant ERR_MAX_ACTIVE_LOANS_REACHED (err u6000))

(define-constant ERR_ASSET_NOT_OWNED (err u7000))

(define-constant ERR_NOT_ALLOWED (err u9999))

;;;; Data variable definitions

;;; Standard principal that calls public function (maintenance)
(define-data-var maintenanceAccount principal DEPLOYER_ACCOUNT)

;; uint values used in calculations and expressions within private and public functions
(define-data-var loanToAssetRatio uint u2500) ;; as basis points ;; 2500 bp = 25 percent
(define-data-var loanLiquidationThreshhold uint u3500) ;; as basis points ;; 3500 bp = 35 percent
(define-data-var loanFeeRate uint u50) ;; as basis points ;; 50 bp = 0.5 percent
;; (define-data-var loanInterestPeriod uint u52560) ;; as blocks ;; 52560 ~= 365 days
(define-data-var loanInterestPeriod uint u4380) ;; TODO: this is correct one? ;; as blocks ;; 4380 ~= 30 days
;; (define-data-var loanTermLengthMin uint u1008) ;; as blocks ;; 1008 ~= 7 days
(define-data-var loanTermLengthMax uint u13104) ;; as blocks ;; 13104 ~= 91 days
;; (define-data-var auctionDuration uint u144) ;; as blocks ;; 144 ~= 24 hours

(define-data-var loanTermLengthMin uint u5) ;; TODO: only for testing
(define-data-var auctionDuration uint u5) ;; TODO: only for testing

(define-data-var loanTermInterestRates (list 20 {termLengthMin: uint, interestRate: uint})
    (list
        ;; {termLengthMin: u1008, interestRate: u500}
        {termLengthMin: u0, interestRate: u500} ;; TODO: consider keeping this
        {termLengthMin: u1152, interestRate: u600}
        {termLengthMin: u2160, interestRate: u700}
        {termLengthMin: u4464, interestRate: u800}
        {termLengthMin: u8784, interestRate: u900}
    )
)

;; uint values updated in public function (maintenance)
(define-data-var lastMaintenanceCallBlock uint block-height)

;; uint value updated in public function (set-asset-floor-value)
(define-data-var assetFloor uint u1000000) ;; as microstacks ;; 1 microstx = 0.000001 stx

;; uint value calculated in public function (set-asset-floor-value)
(define-data-var loanAmountMax uint u1000000)

;; uint value calculated in public function (set-asset-floor-value)
(define-data-var loanLiquidationValue uint u1000000)

;; uint value incremented by 1 and updated in private function (liquidate-loan)
(define-data-var lastAuctionId uint u0)

;; uint value incremented by 1 and updated in public function (new-loan)
(define-data-var lastLoanId uint u0)

;; uint value updated in private function (liquidate-loan)
;; uint value updated in private function (close-auction)
(define-data-var tempUint uint u0)

;; list of uint values added in private function (liquidate-loan)
;; list of uint values removed in private function (close-auction)
(define-data-var activeAuctionIds (list 2500 uint) (list ))

;; list of uint values added in private function (new-loan)
;; list of uint values removed in private function (liquidate-loan)
(define-data-var activeLoanIds (list 2500 uint) (list ))

;;;; Map definitions

(define-map Borrower
    principal
    (list 5 uint)
)

(define-map BorrowerByLoan
    uint
    principal
)

(define-map Loan
    uint
    {
        id: uint,
        assetId: uint,
        debtBalance: uint,
        termInterestRate: uint,
        termEndAt: uint,
    }
)

(define-map Auction
    uint
    {
        id: uint,
        assetId: uint,
        reserveAmount: uint,
        lastBidAmount: uint,
        lastBidderAccount: (optional principal),
        endAt: (optional uint),
    }
)

(define-map Admin
    principal
    bool
)

;;;; Private function definitions

(define-private (not-tempUint (id uint))
    (not (is-eq id (var-get tempUint)))
)

(define-private (get-auction (auctionId uint))
    (map-get? Auction auctionId)
)

(define-private (get-loan (loanId uint))
    (map-get? Loan loanId)
)

(define-private (term-interest-rate (termInterestRate {termLengthMin: uint, interestRate: uint}) (interestRate uint))
    (begin
        (asserts! (>= (var-get tempUint) (get termLengthMin termInterestRate))
            interestRate
        )
        (get interestRate termInterestRate)
    )
)

(define-private (uint-list-slice (uintList (list 2500 uint)) (start uint))
    (get accumulator (fold uint-list-slice-iterator uintList {accumulator: (list ), index: u0, start: start}))
)

(define-private (uint-list-slice-iterator (value uint) (state {accumulator: (list 10 uint), index: uint, start: uint}))
    (let
        (
            (start
                (get start state)
            )
            (index
                (get index state)
            )
            (accumulator
                (get accumulator state)
            )
        )
        {
            start:
                start,
            accumulator:
                (if (>= index start)
                    (unwrap! (as-max-len? (append accumulator value) u10) state)
                    accumulator
                ),
            index:
                (+ index u1)
        }
    )
)

(define-private (close-auction (auctionId uint) (count uint))
    (let
        (
            (auction
                (unwrap! (map-get? Auction auctionId) count)
            )
            (auctionAssetId
                (get assetId auction)
            )
            (auctionLastBidderAccount
                (unwrap! (get lastBidderAccount auction) count)
            )
            (auctionEndAt
                (unwrap! (get endAt auction) count)
            )
            (eventCount
                (+ count u1)
            )
        )
        (asserts! (> block-height auctionEndAt)
            count
        )
        (asserts! (is-ok (as-contract (contract-call? .megapont-ape-club-nft transfer auctionAssetId tx-sender auctionLastBidderAccount)))
            count
        )
        (var-set tempUint auctionId)
        (var-set activeAuctionIds (filter not-tempUint (var-get activeAuctionIds)))
        (print
            {
                eventName: "close-auction",
                eventCount: eventCount,
                auctionId: auctionId,
                assetId: auctionAssetId,
                lastBidderAccount: auctionLastBidderAccount,
                endAt: auctionEndAt
            }
        )
        eventCount
    )
)

(define-private (liquidate-loan (loanId uint) (count uint))
    (let
        (
            (loan
                (unwrap! (map-get? Loan loanId) count)
            )
            (loanAssetId
                (get assetId loan)
            )
            (borrowerAccount
                (unwrap! (map-get? BorrowerByLoan loanId) count)
            )
            (loanDebtBalance
                (get debtBalance loan)
            )
            (loanTermEndAt
                (get termEndAt loan)
            )
            (borrowerActiveLoanIds
                (unwrap! (map-get? Borrower borrowerAccount) count)
            )
            (auctionId
                (begin
                    (+ (var-get lastAuctionId) u1)
                )
            )
            (eventCount
                (+ count u1)
            )
        )
        (asserts! (or (> block-height loanTermEndAt) (>= loanDebtBalance (var-get loanLiquidationValue)))
            count
        )
        (begin
            (var-set tempUint loanId)
            (var-set activeLoanIds (filter not-tempUint (var-get activeLoanIds)))
            (map-set Borrower borrowerAccount (filter not-tempUint borrowerActiveLoanIds))
            (var-set activeAuctionIds (unwrap! (as-max-len? (concat (list auctionId) (var-get activeAuctionIds)) u2500) count))
            (var-set lastAuctionId auctionId)
            (map-insert Auction
                auctionId
                {
                    id: auctionId,
                    assetId: loanAssetId,
                    reserveAmount: loanDebtBalance,
                    lastBidAmount: u0,
                    lastBidderAccount: none,
                    endAt: none,
                }
            )
            (print
                {
                    eventName: "liquidate-loan",
                    eventCount: eventCount,
                    loanId: loanId,
                    assetId: loanAssetId,
                    debtBalance: loanDebtBalance,
                    termEndAt: loanTermEndAt,
                    loanLiquidationValue: (var-get loanLiquidationValue),
                    auctionId: auctionId
                }
            )
            eventCount
        )
    )
)

(define-private (compound-loan (loanId uint) (count uint))
    (let
        (
            (loan
                (unwrap! (map-get? Loan loanId) count)
            )
            (loanAssetId
                (get assetId loan)
            )
            (loanDebtBalance
                (get debtBalance loan)
            )
            (loanTermInterestRate
                (get termInterestRate loan)
            )
            (loanTermEndAt
                (get termEndAt loan)
            )
            (compoundingInterval
                (- block-height (var-get lastMaintenanceCallBlock))
            )
            (interestAmountPerPeriod
                (/ (* loanDebtBalance loanTermInterestRate) u10000)
            )
            (interestAmountPerBlock
                    (/ interestAmountPerPeriod (var-get loanInterestPeriod))
            )
            (compoundedInterestAmount
                (* interestAmountPerBlock compoundingInterval)
            )
            (compoundedDebtBalance
                (+ loanDebtBalance compoundedInterestAmount)
            )
            (eventCount
                (+ count u1)
            )
        )
        (map-set Loan
            loanId
            {
                id: loanId,
                assetId: loanAssetId,
                debtBalance: compoundedDebtBalance,
                termInterestRate: loanTermInterestRate,
                termEndAt: loanTermEndAt,
            }
        )
        (print
            {
                eventName: "compound-loan",
                eventCount: eventCount,
                loanId: loanId,
                assetId: loanAssetId,
                debtBalance: loanDebtBalance,
                termEndAt: loanTermEndAt,
                compoundedInterestAmount: compoundedInterestAmount,
                compoundedDebtBalance: compoundedDebtBalance
            }
        )
        eventCount
    )
)

;;;; Read only function definitions

(define-read-only (get-active-auctions (start uint))
    (map get-auction (uint-list-slice (var-get activeAuctionIds) start))
)

(define-read-only (get-active-loans (start uint))
    (map get-loan (uint-list-slice (var-get activeLoanIds) start))
)

(define-read-only (get-borrower-active-loans (account principal))
    (let
        (
            (borrowerActiveLoanIds
                (default-to (list ) (map-get? Borrower account))
            )
        )
        (map get-loan borrowerActiveLoanIds)
    )
)

(define-read-only (get-data-values-1)
    (let
        (
            (values
                {
                    loanToAssetRatio: (var-get loanToAssetRatio),
                    loanLiquidationThreshhold: (var-get loanLiquidationThreshhold),
                    loanFeeRate: (var-get loanFeeRate),
                    loanInterestPeriod: (var-get loanInterestPeriod),
                    loanTermLengthMin: (var-get loanTermLengthMin),
                    loanTermLengthMax: (var-get loanTermLengthMax),
                }
            )
        )
        values
    )
)

(define-read-only (get-data-values-2)
  (let
        (
            (values
                {
                    auctionDuration: (var-get auctionDuration),
                    loanTermInterestRates: (var-get loanTermInterestRates),
                    assetFloor: (var-get assetFloor),
                    loanAmountMax: (var-get loanAmountMax),
                    loanLiquidationValue: (var-get loanLiquidationValue)
                }
            )
        )
        values
    )
)

(define-read-only (get-data-values-3)
  (let
        (
            (values
                {
                    loanCount: (len (var-get activeLoanIds)),
                    auctionCount: (len (var-get activeAuctionIds))
                }
            )
        )
        values
    )
)

;;;; Public function definitions

(define-public (auction-bid (auctionId uint) (amount uint))
    (begin
        (asserts! (is-some (index-of (var-get activeAuctionIds) auctionId))
            ERR_AUCTION_INACTIVE
        )
        (let
            (
                (auction
                    (unwrap! (map-get? Auction auctionId) ERR_AUCTION_NOT_FOUND) ;; should never catch if auctionId is active
                )
                (auctionAssetId
                    (get assetId auction)
                )
                (auctionReserveAmount
                    (get reserveAmount auction)
                )
                (auctionLastBidAmount
                    (get lastBidAmount auction)
                )
                (auctionLastBidderAccount
                    (get lastBidderAccount auction)
                )
                (auctionEndAt
                    (match (get endAt auction)
                        endAt
                            (begin
                                (asserts! (< block-height endAt) ERR_AUCTION_ENDED)
                                endAt
                            )
                        (+ block-height (var-get auctionDuration))
                    )
                )
                (bidderAccount
                    tx-sender
                )
            )
            (asserts! (and (>= amount auctionReserveAmount) (> amount auctionLastBidAmount))
                ERR_AMOUNT_INVALID
            )
            (asserts! (not (is-eq bidderAccount (as-contract tx-sender)))
                ERR_ACCOUNT_INVALID
            )
            (map-set Auction
                auctionId
                {
                    id: auctionId,
                    assetId: auctionAssetId,
                    reserveAmount: auctionReserveAmount,
                    lastBidAmount: amount,
                    lastBidderAccount: (some bidderAccount),
                    endAt: (some auctionEndAt),
                }
            )
            (asserts! (>= (stx-get-balance bidderAccount) amount)
                ERR_STX_TRANSFER_FAILED
            )
            (asserts! (is-ok (stx-transfer? amount bidderAccount (as-contract tx-sender)))
                ERR_STX_TRANSFER_FAILED
            )
            (match auctionLastBidderAccount
                recipient
                    (begin
                        (asserts! (>= (stx-get-balance (as-contract tx-sender)) auctionLastBidAmount)
                            ERR_STX_TRANSFER_FAILED
                        )
                        (asserts! (is-ok (as-contract (stx-transfer? auctionLastBidAmount tx-sender recipient)))
                            ERR_STX_TRANSFER_FAILED
                        )
                        (ok true)
                    )
                (ok true)
            )
        )
    )
)

(define-public (pay-loan (loanId uint) (amount uint))
    (begin
        (asserts! (is-some (index-of (var-get activeLoanIds) loanId))
            ERR_LOAN_INACTIVE
        )
        (asserts! (is-some (index-of (default-to (list ) (map-get? Borrower tx-sender)) loanId))
            ERR_LOAN_INACTIVE
        )
        (asserts! (> amount u0)
            ERR_AMOUNT_INVALID
        )
        (let
            (
                (loan
                    (unwrap! (map-get? Loan loanId) ERR_LOAN_NOT_FOUND) ;; should never catch if valid loanId
                )
                (loanAssetId
                    (get assetId loan)
                )
                (loanDebtBalance
                    (get debtBalance loan)
                )
                (loanTermInterestRate
                    (get termInterestRate loan)
                )
                (loanTermEndAt
                    (get termEndAt loan)
                )
                (adjustedPaymentAmount
                    (if (>= amount loanDebtBalance)
                        loanDebtBalance
                        amount
                    )
                )
                (newLoanDebtBalance
                    (- loanDebtBalance adjustedPaymentAmount)
                )
                (paymentRefundAmount
                    (if (is-eq newLoanDebtBalance u0)
                        (- amount loanDebtBalance)
                        u0
                    )
                )
                (borrowerAccount
                    tx-sender
                )
                (borrowerActiveLoanIds
                    (default-to (list ) (map-get? Borrower borrowerAccount))
                )
            )
            (asserts! (> loanTermEndAt block-height) ERR_LOAN_EXPIRED) ;; TODO: test
            (asserts! (is-some (index-of (default-to (list ) (map-get? Borrower borrowerAccount)) loanId))
                ERR_NOT_ALLOWED
            )
            (map-set Loan
                loanId
                {
                    id: loanId,
                    assetId: loanAssetId,
                    debtBalance: newLoanDebtBalance,
                    termInterestRate: loanTermInterestRate,
                    termEndAt: loanTermEndAt,
                }
            )
            (asserts! (>= (stx-get-balance borrowerAccount) amount)
                ERR_STX_TRANSFER_FAILED
            )
            (asserts! (is-ok (stx-transfer? amount borrowerAccount (as-contract tx-sender)))
                ERR_STX_TRANSFER_FAILED
            )

            ;; TO-DO - Refactor
            (if (is-eq newLoanDebtBalance u0)
                (begin
                    (var-set tempUint loanId)
                    (var-set activeLoanIds (filter not-tempUint (var-get activeLoanIds)))
                    (map-set Borrower borrowerAccount (filter not-tempUint borrowerActiveLoanIds))
                    (try! (as-contract (contract-call? .megapont-ape-club-nft transfer loanAssetId tx-sender borrowerAccount)))
                    (if (> paymentRefundAmount u0)
                        (begin
                            (asserts! (>= paymentRefundAmount (stx-get-balance borrowerAccount))
                               ERR_STX_TRANSFER_FAILED
                            )
                            (asserts! (is-ok (stx-transfer? paymentRefundAmount borrowerAccount (as-contract tx-sender)))
                                ERR_STX_TRANSFER_FAILED
                            )
                            (ok true)
                        )
                        (ok true)
                    )
                )
                (ok true) ;; never used
            )
        )
    )
)

(define-public (new-loan (assetId uint) (amount uint) (termLength uint))
    (begin
        (asserts! (is-eq (ok (some tx-sender)) (contract-call? .megapont-ape-club-nft get-owner assetId))
            ERR_ASSET_NOT_OWNED
        )
        (asserts! (<= amount (var-get loanAmountMax))
            ERR_AMOUNT_INVALID
        )
        (asserts!
            (and
                (>= termLength (var-get loanTermLengthMin))
                (<= termLength (var-get loanTermLengthMax))
            )
            ERR_TERM_INVALID
        )
        (let
            (
                (loanId
                    (begin
                        (var-set lastLoanId (+ (var-get lastLoanId) u1))
                        (var-get lastLoanId)
                    )
                )
                (loanTermEndAt
                    (+ block-height termLength)
                )
                (loanTermInterestRate
                    (begin
                        (var-set tempUint termLength)
                        (fold term-interest-rate (var-get loanTermInterestRates) u0)
                    )
                )
                (loanFeeAmount
                    (/ (* amount (var-get loanFeeRate)) u10000)
                )
                (loanWithdrawalAmount
                    (- amount loanFeeAmount)
                )
                (borrowerAccount
                    tx-sender
                )
                (borrowerActiveLoanIds
                    (default-to (list ) (map-get? Borrower borrowerAccount))
                )
            )
            (asserts! (>= (stx-get-balance (as-contract tx-sender)) loanWithdrawalAmount)
                ERR_STX_TRANSFER_FAILED
            )
            ;; if error then fail
            (asserts! (is-ok (as-contract (stx-transfer? loanWithdrawalAmount tx-sender borrowerAccount)))
                ERR_STX_TRANSFER_FAILED
            )
            ;; if error then fail
            (asserts! (is-ok (contract-call? .megapont-ape-club-nft transfer assetId borrowerAccount (as-contract tx-sender)))
                ERR_ASSET_TRANSFER_FAILED
            )
            (map-set Borrower borrowerAccount (unwrap! (as-max-len? (concat (list loanId) borrowerActiveLoanIds) u5) ERR_MAX_ACTIVE_LOANS_REACHED))
            (var-set activeLoanIds (unwrap! (as-max-len? (concat (list loanId) (var-get activeLoanIds)) u2500) ERR_MAX_ACTIVE_LOANS_REACHED))
            (map-insert Loan
                loanId
                {
                    id: loanId,
                    assetId: assetId,
                    debtBalance: amount,
                    termInterestRate: loanTermInterestRate,
                    termEndAt: loanTermEndAt,
                }
            )
            (map-insert BorrowerByLoan
                loanId
                borrowerAccount
            )
            (ok true)
        )
    )
)

;;; Admin functions

(define-read-only (is-admin (account principal))
    (or
        (is-eq (var-get maintenanceAccount) tx-sender)
        (default-to false (map-get? Admin account))
    )
)

(define-public (set-admin (account principal) (allowed bool))
    (begin
        (asserts!
            (and
                (is-admin tx-sender)
                (not (is-eq account tx-sender))
            )
            ERR_NOT_ALLOWED
        )
        ;; #[allow(unchecked_data)]
        (ok (map-set Admin account allowed))
    )
)

(define-public (run-maintenance)
    (begin
        (fold close-auction (var-get activeAuctionIds) u0)
        (fold liquidate-loan (var-get activeLoanIds) u0)
        (fold compound-loan (var-get activeLoanIds) u0)
        (var-set lastMaintenanceCallBlock block-height)
        (ok true)
    )
)

(define-public (set-assetFloor (amount uint))
    (begin
        (asserts! (is-admin tx-sender)
            ERR_NOT_ALLOWED
        )
        (asserts! (> amount u0)
            ERR_AMOUNT_INVALID
        )
        (var-set assetFloor amount)
        (var-set loanAmountMax (/ (* (var-get loanToAssetRatio) amount) u10000))
        (var-set loanLiquidationValue (/ (* (var-get loanLiquidationThreshhold) amount) u10000))
        (ok true)
    )
)

(define-public (fund-vault (amount uint))
    (begin
        (asserts! (> amount u0)
            ERR_AMOUNT_INVALID
        )
        (asserts! (>= (stx-get-balance tx-sender) amount)
            ERR_STX_TRANSFER_FAILED
        )
        (asserts! (is-ok (stx-transfer? amount tx-sender (as-contract tx-sender)))
            ERR_STX_TRANSFER_FAILED
        )
        (ok true)
    )
)

(define-public (drain-vault (amount uint))
    (begin
        (asserts! (is-admin tx-sender)
            ERR_NOT_ALLOWED
        )
        (asserts! (and (> amount u0) (>= (stx-get-balance (as-contract tx-sender)) amount))
            ERR_AMOUNT_INVALID
        )
        (asserts! (is-ok (as-contract (stx-transfer? amount tx-sender DEPLOYER_ACCOUNT)))
            ERR_STX_TRANSFER_FAILED
        )
        (ok true)
    )
)
