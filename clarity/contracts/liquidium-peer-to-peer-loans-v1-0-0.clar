;; liquidium-peer-to-peer-loans-v1-0-0

;; traits
(use-trait sip009-nft-trait .sip009-nft-trait.sip009-nft-trait)

;; constants
(define-constant DEPLOYER tx-sender)

;; error codes
(define-constant ERR-UNAUTHORIZED (err u1))
(define-constant ERR-EXPIRY-IN-PAST (err u100))
(define-constant ERR-PRINCIPAL-ZERO (err u101))
(define-constant ERR-UNKNOWN-LISTING (err u102))
(define-constant ERR-NFT-ASSET-MISMATCH (err u103))
(define-constant ERR-BORROWER-LENDER-EQUAL (err u104))
(define-constant ERR-UNINTENDED-LENDER (err u105))
(define-constant ERR-LISTING-EXPIRED (err u106))
(define-constant ERR-PAYMENT-ASSET-MISMATCH (err u107))
(define-constant ERR-UNKNOWN-LOAN (err u108))
(define-constant ERR-MAX-ACTIVE-LOANS-REACHED (err u109))
(define-constant ERR-ON-ACTIVE-LOAN-IDS-UPDATE (err u110))
(define-constant ERR-INSUFFICIENT-FUNDS (err u111))
(define-constant ERR-CONTRACT-INSUFFICIENT-FUNDS (err u112))
(define-constant ERR-LOAN-NOT-EXPIRED (err u113))
(define-constant ERR-LOAN-EXPIRED (err u114))
(define-constant ERR-PAYOFF-LESS-THAN-PRINCIPAL (err u115))
(define-constant ERR-MAX-OFFERS-REACHED (err u116))
(define-constant ERR-NO-OFFERS (err u117))
(define-constant ERR-OFFER-INDEX-OUT-OF-RANGE (err u118))
(define-constant ERR-UNKNOWN-OFFER (err u119))

;; vars
(define-data-var listings-count uint u0)
(define-data-var loans-count uint u0)
(define-data-var temp-uint uint u0)
(define-data-var active-loan-ids (list 5000 uint) (list ))

(define-data-var interest-fee uint u500) ;; 5%
(define-data-var listing-fee uint u200) ;; 2% ;; TODO: consider if this should be a constant. if stays var, needs to be stored in listing

;; maps
(define-map listings
  uint
	{
		borrower: principal,
		lender: (optional principal),
		token-id: uint,
		nft-asset-contract: principal,
		principal: uint, ;; TODO: need an optional object representing borrowers initial request
    payoff: uint,
		expiry: uint,
		payment-asset-contract: (optional principal),
    offers: (optional (list 20 {
      lender: principal,
      principal: uint,
      payoff: uint,
      expiry: uint,
      payment-asset-contract: (optional principal),
    }))
	}
)

(define-map loans 
  uint
  {
    borrower: principal,
    lender: principal,
    principal: uint,
    payoff: uint,
    token-id: uint,
    nft-asset-contract: principal,
    expiry: uint,
    payment-asset-contract: (optional principal)
  }
)

;; read only functions
(define-read-only (get-loan (loan-id uint))
  (map-get? loans loan-id)
)

(define-read-only (get-listing (listing-id uint))
	(map-get? listings listing-id)
)

;; private functions
(define-private (not-temp-uint (id uint))
  (if (is-eq id (var-get temp-uint)) false true)
)

(define-private (transfer-nft
  (token-contract <sip009-nft-trait>)
  (token-id uint)
  (sender principal)
  (recipient principal)
)
  (contract-call? token-contract transfer token-id sender recipient)
)

;; public functions
(define-public (list-asset
  (nft-asset-contract <sip009-nft-trait>)
  (listing-info {
    principal: uint,
    payoff: uint,
    lender: (optional principal),
    token-id: uint,
    expiry: uint,
    payment-asset-contract: (optional principal)
  })
)
	(let (
    (listing-id (var-get listings-count))
    (principal (get principal listing-info))
    (fee (/ (* (var-get listing-fee) principal) u10000)) ;; TODO: need to figure out how to handle fees
    (lender (get lender listing-info))
  )
		(asserts! (> (get expiry listing-info) block-height) ERR-EXPIRY-IN-PAST)
		(asserts! (> principal u0) ERR-PRINCIPAL-ZERO)
    (asserts! (<= fee (stx-get-balance tx-sender)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (not (and (is-some lender) (is-eq tx-sender (unwrap-panic lender)))) ERR-BORROWER-LENDER-EQUAL)
    (asserts! (> (get payoff listing-info) principal) ERR-PAYOFF-LESS-THAN-PRINCIPAL)

    (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
		(try! (transfer-nft nft-asset-contract (get token-id listing-info) tx-sender (as-contract tx-sender)))
		(map-set listings listing-id (merge {borrower: tx-sender, nft-asset-contract: (contract-of nft-asset-contract), offers: none} listing-info))
		(var-set listings-count (+ listing-id u1))
		(ok listing-id)
	)
)

(define-public (cancel-listing
  (listing-id uint)
  (nft-asset-contract <sip009-nft-trait>)
)
	(let (
    (listing (unwrap! (map-get? listings listing-id) ERR-UNKNOWN-LISTING))
    (borrower (get borrower listing))
  )
		(asserts! (is-eq borrower tx-sender) ERR-UNAUTHORIZED)
		(asserts! (is-eq (get nft-asset-contract listing) (contract-of nft-asset-contract)) ERR-NFT-ASSET-MISMATCH)
    
		(map-delete listings listing-id)
    (try! (stx-transfer? (/ (* (var-get listing-fee) (get principal listing)) u10000) (as-contract tx-sender) tx-sender))
		(as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender borrower))
	)
)

(define-public (fulfil-listing-stx
  (listing-id uint)
  (nft-asset-contract <sip009-nft-trait>)
  (payment-asset-contract (optional principal))
)
	(let (
    (listing (unwrap! (map-get? listings listing-id) ERR-UNKNOWN-LISTING))
    (lender tx-sender)
    (borrower (get borrower listing))
    (loan-id (var-get loans-count))
  )
    (asserts! (not (is-eq borrower tx-sender)) ERR-BORROWER-LENDER-EQUAL)
		(asserts! (match (get lender listing) intended-lender (is-eq intended-lender lender) true) ERR-UNINTENDED-LENDER)
		(asserts! (< block-height (get expiry listing)) ERR-LISTING-EXPIRED)
		(asserts! (is-eq (get nft-asset-contract listing) (contract-of nft-asset-contract)) ERR-NFT-ASSET-MISMATCH)
		(asserts! (is-eq (get payment-asset-contract listing) payment-asset-contract) ERR-PAYMENT-ASSET-MISMATCH)

		(try! (stx-transfer? (get principal listing) lender borrower))
    (var-set active-loan-ids (unwrap! (as-max-len? (append (var-get active-loan-ids) loan-id) u5000) ERR-MAX-ACTIVE-LOANS-REACHED))
    (map-set loans loan-id {
      borrower: borrower,
      lender: lender,
      principal: (get principal listing),
      payoff: (get payoff listing),
      token-id: (get token-id listing),
      nft-asset-contract: (get nft-asset-contract listing),
      expiry: (get expiry listing),
      payment-asset-contract: (get payment-asset-contract listing)
    })
		(map-delete listings listing-id)
    (var-set loans-count (+ u1 loan-id))
		(ok loan-id)
	)
)

(define-public (make-offer
  (listing-id uint)
  (offer-info {
    principal: uint,
    payoff: uint,
    expiry: uint,
    payment-asset-contract: (optional principal),
  })
)
  (let (
    (listing (unwrap! (map-get? listings listing-id) ERR-UNKNOWN-LISTING))
    (offers (default-to (list ) (get offers listing)))
    (principal (get principal offer-info))
    (payoff (get payoff offer-info))
  )
    (asserts! (> (get expiry offer-info) block-height) ERR-EXPIRY-IN-PAST)
    (asserts! (> (get principal offer-info) u0) ERR-PRINCIPAL-ZERO)
    (asserts! (> payoff principal) ERR-PAYOFF-LESS-THAN-PRINCIPAL)
    (asserts! (<= principal (stx-get-balance tx-sender)) ERR-INSUFFICIENT-FUNDS)

    (try! (stx-transfer? principal tx-sender (as-contract tx-sender)))
    (map-set listings listing-id {
      borrower: (get borrower listing),
      lender: (get lender listing),
      token-id: (get token-id listing),
      nft-asset-contract: (get nft-asset-contract listing),
      principal: (get principal listing),
      payoff: (get payoff listing),
      expiry: (get expiry listing),
      payment-asset-contract: (get payment-asset-contract listing),
      offers: (some (unwrap! (as-max-len? (append offers (merge offer-info {lender: tx-sender})) u20) ERR-MAX-OFFERS-REACHED))
    })
    (ok listing-id)
  )
)

(define-public (accept-offer
  (listing-id uint)
  (offer-index uint)
)
  (let (
    (listing (unwrap! (map-get? listings listing-id) ERR-UNKNOWN-LISTING))
    (offers (unwrap! (get offers listing) ERR-NO-OFFERS))
    (offer (unwrap! (element-at offers offer-index) ERR-UNKNOWN-OFFER)) ;; TODO: handle all the panics damnit
    (principal (get principal offer))
    (borrower (get borrower listing))
    (fee (/ (* (var-get listing-fee) principal) u10000))
    (loan-id (var-get loans-count))
  )
    (asserts! (is-eq tx-sender borrower) ERR-UNAUTHORIZED)
    (asserts! (< block-height (get expiry listing)) ERR-LISTING-EXPIRED)
    (asserts! (< offer-index (len offers)) ERR-OFFER-INDEX-OUT-OF-RANGE)
    (asserts! (<= fee (stx-get-balance tx-sender)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (<= principal (stx-get-balance (as-contract tx-sender))) ERR-CONTRACT-INSUFFICIENT-FUNDS)

    (try! (stx-transfer? fee tx-sender DEPLOYER))
    (try! (as-contract (stx-transfer? principal tx-sender borrower)))
    (var-set active-loan-ids (unwrap! (as-max-len? (append (var-get active-loan-ids) loan-id) u5000) ERR-MAX-ACTIVE-LOANS-REACHED))
    (map-set loans loan-id {
      borrower: borrower,
      lender: (get lender offer),
      principal: principal,
      payoff: (get payoff offer),
      token-id: (get token-id listing),
      nft-asset-contract: (get nft-asset-contract listing),
      expiry: (get expiry offer),
      payment-asset-contract: (get payment-asset-contract offer)
    })
		(map-delete listings listing-id)
    (var-set loans-count (+ u1 loan-id))
		(ok loan-id)
  )
)

(define-public (repay-loan
  (loan-id uint)
  (nft-asset-contract <sip009-nft-trait>)
)
  (let (
    (loan (unwrap! (map-get? loans loan-id) ERR-UNKNOWN-LOAN))
    (repayer tx-sender)
    (lender (get lender loan))
    (fee (/ (* (- (get payoff loan) (get principal loan)) (var-get interest-fee)) u10000))
  )
    (asserts! (is-eq repayer (get borrower loan)) ERR-UNAUTHORIZED) ;; TODO: or should anyone be allowed to repay?
    (asserts! (is-eq (get nft-asset-contract loan) (contract-of nft-asset-contract)) ERR-NFT-ASSET-MISMATCH)
    (asserts! (>= (stx-get-balance tx-sender) (get payoff loan)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (<= block-height (get expiry loan)) ERR-LOAN-EXPIRED)

    (try! (stx-transfer? fee repayer DEPLOYER))
    (try! (stx-transfer? (- (get payoff loan) fee) repayer lender))
    (try! (as-contract (transfer-nft nft-asset-contract (get token-id loan) tx-sender repayer)))
    (map-delete loans loan-id)
    (ok loan-id)
  )
)

(define-public (foreclose-loan
  (loan-id uint)
  (nft-asset-contract <sip009-nft-trait>)
)
  (let (
    (closer tx-sender)
    (loan (unwrap! (map-get? loans loan-id) ERR-UNKNOWN-LOAN))
  )
    (asserts! (is-eq closer (get lender loan)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get nft-asset-contract loan) (contract-of nft-asset-contract)) ERR-NFT-ASSET-MISMATCH)
    (asserts! (>= block-height (get expiry loan)) ERR-LOAN-NOT-EXPIRED)

    (try! (as-contract (transfer-nft nft-asset-contract (get token-id loan) tx-sender closer)))
    (var-set active-loan-ids (unwrap! (as-max-len? (filter not-temp-uint (var-get active-loan-ids)) u2500) ERR-ON-ACTIVE-LOAN-IDS-UPDATE))    
    (map-delete loans loan-id)
    (ok loan-id)
  )
)
