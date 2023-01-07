
;; liquidium-megapont-p2pool-loans-v1-0

;; admins
(define-constant DEPLOYER-ACCOUNT tx-sender)
(define-constant LIQUIDIUM-ACCOUNT 'SPYHY9MV6S08YJQVW0R400ADXZBBJ0GM096BMY34.liquidium-profits)

;; error codes
(define-constant err-unauthorized (err u1000))
(define-constant err-invalid-input (err u1001))
(define-constant err-incorrect-cycle-term (err u1002))
(define-constant err-appending-to-list (err u1003))
(define-constant err-insufficient-funds (err u1004))

(define-constant err-max-capital-reached (err u2000))

(define-constant err-loan-not-found (err u3000))
(define-constant err-loan-not-active (err u3001))
(define-constant err-invalid-term-length (err u3002))
(define-constant err-invalid-amount (err u3003))
(define-constant err-on-asset-transfer (err u3004))
(define-constant err-asset-not-owned (err u3005))
(define-constant err-max-user-active-loans (err u3006))
(define-constant err-max-vault-active-loans (err u3007))
(define-constant err-on-fee-transfer (err u3008))
(define-constant err-on-stx-transfer (err u3009))

;; constants

;; data vars
(define-data-var asset-value uint u0)
(define-data-var vault-lending-liquidity uint u0) ;; current lending liquidity

(define-data-var auction-count uint u0) ;; total auctions ever
(define-data-var auction-duration uint u144) ;; ~1 day in blocks

(define-data-var loan-count uint u0) ;; total loans ever
(define-data-var loan-fee uint u50) ;; 0.5% as basis points
(define-data-var loan-term-min uint u1152) ;; ~1 week in blocks
;; TODO: solve problem to allow for people to take loans longer than cycle term
;; (define-data-var loan-term-max uint u12432) ;; ~3 months in blocks
(define-data-var loan-ltv-max uint u4000) ;; 40% as basis points
(define-data-var loan-ltv-liquidation-threshold uint u5000) ;; 50% as basis points
(define-data-var loan-liquidation-value uint u0)
(define-data-var loan-max-value uint u0)
(define-data-var loan-interest-rates (list 5 (tuple (interestRate uint) (termLengthMin uint)))
  (list
    {termLengthMin: u0, interestRate: u500}
    {termLengthMin: u1152, interestRate: u600}
    {termLengthMin: u2160, interestRate: u700}
    {termLengthMin: u3168, interestRate: u800}
    {termLengthMin: u4176, interestRate: u900}
))

(define-data-var cycle-start-block uint block-height)
(define-data-var cycle-lend-term uint u4176) ;; ~1 month in blocks
(define-data-var cycle-raise-term uint u1152) ;; ~1 week in blocks
(define-data-var cycle-length uint (+ (var-get cycle-lend-term) (var-get cycle-raise-term)))
(define-data-var cycle-max-capital uint u10000000000) ;; max capital lenders can deposit
(define-data-var cycle-max-lend-percent uint u7500) ;; 50% as basis points

;; dummy principal
(define-data-var temp-uint uint u0)
(define-data-var temp-principal principal 'SPYHY9MV6S08YJQVW0R400ADXZBBJ0GM096BMY34)

;; lists
(define-data-var active-lenders (list 5000 principal) (list ))
(define-data-var active-loans (list 1500 uint) (list ))
(define-data-var active-auctions (list 1500 uint) (list ))

;; maps
(define-map admins principal bool)

(define-map lenders principal {
  locked-amount: uint,
  compound: bool,
})

(define-map loans uint {
  asset-id: uint,
  debt: uint,
  principal: uint,
  interest: uint,
  interest-rate: uint,
  start: uint,
  end: uint,
  borrower: principal,
})

(define-map auctions uint { 
  id: uint,
  asset-id: uint,
  reserve: uint,
  start: uint,
  end: (optional uint),
  last-bid: (optional uint),
  last-bidder: (optional principal),
})

(define-map borrowers principal (list 50 uint))

;; private functions
(define-private (not-temp-uint (id uint))
  (not (is-eq id (var-get temp-uint)))
)

(define-private (not-temp-principal (id principal))
  (not (is-eq id (var-get temp-principal)))
)

(define-private (term-interest-rate (termInterestRate {termLengthMin: uint, interestRate: uint}) (interestRate uint))
  (begin
    (asserts! (>= (var-get temp-uint) (get termLengthMin termInterestRate)) interestRate)
    (get interestRate termInterestRate)
  )
)

(define-private (get-term-interest-rate (term-length uint))
  (begin 
    (var-set temp-uint term-length)
    (fold term-interest-rate (var-get loan-interest-rates) u0)
  )
)

(define-private (uint-list-slice (uint-list (list 1500 uint)) (start uint))
  (get accumulator (fold uint-list-slice-iterator uint-list {accumulator: (list ), index: u0, start: start}))
)

(define-private (uint-list-slice-iterator (value uint) (state {accumulator: (list 10 uint), index: uint, start: uint}))
  (let (
      (start (get start state))
      (index (get index state))
      (accumulator (get accumulator state) )
    )
    {
      start: start,
      accumulator: (if (>= index start)
        (unwrap! (as-max-len? (append accumulator value) u10) state) 
        accumulator
      ),
      index: (+ index u1)
    }
  )
)

(define-private (principal-list-slice (principal-list (list 5000 principal)) (start uint))
  (get accumulator (fold principal-list-slice-iterator principal-list {accumulator: (list ), index: u0, start: start}))
)

(define-private (principal-list-slice-iterator (value principal) (state {accumulator: (list 10 principal), index: uint, start: uint}))
  (let (
      (start (get start state))
      (index (get index state))
      (accumulator (get accumulator state) )
    )
    {
      start: start,
      accumulator: (if (>= index start)
        (unwrap! (as-max-len? (append accumulator value) u10) state) 
        accumulator
      ),
      index: (+ index u1)
    }
  )
)

(define-private (liquidate-loan (loan-id uint) (count uint))
  (let (
      (loan (unwrap! (map-get? loans loan-id) count))
      (borrower (get borrower loan))
      (auction-id (+ (var-get auction-count) u1))
    )
    (asserts! (or
      (>= block-height (get end loan))
      (>= (get debt loan) (var-get loan-liquidation-value))
    ) count)
    (var-set temp-uint loan-id)
    (var-set active-loans (unwrap! (as-max-len? (filter not-temp-uint (var-get active-loans)) u1500) count))
    (map-set borrowers borrower (unwrap! (as-max-len? (filter not-temp-uint (default-to (list ) (map-get? borrowers borrower))) u50) count))
    (var-set active-auctions (unwrap! (as-max-len? (append (var-get active-auctions) loan-id) u1500) count)) ;; TODO: test stuff like this when auctions max out, but loans need to liquidate (or is even concern? 1500/2500 MEGs in auction?)
    (+ count u1)
  )
)

;; read only functions
(define-read-only (get-lender (lender principal))
  (map-get? lenders lender)
)

(define-read-only (get-loan (id uint))
  (map-get? loans id)
)

(define-read-only (get-auction (id uint))
  (map-get? auctions id)
)

(define-read-only (get-active-auctions (start uint))
  (map get-auction (uint-list-slice (var-get active-auctions) start))
)

(define-read-only (get-active-loans (start uint))
  (map get-loan (uint-list-slice (var-get active-loans) start))
)

(define-read-only (get-active-lenders (start uint))
  (map get-lender (principal-list-slice (var-get active-lenders) start))
)

(define-read-only (get-borrower-active-loans (account principal))
  (map get-loan (default-to (list ) (map-get? borrowers account)))
)

;; lender functions
(define-public (deposit-capital (amount uint) (compound bool))
  (let (
      (current-lending-liquidity (var-get vault-lending-liquidity))
      (current-locked-amount (match (map-get? lenders tx-sender) existing-lender
        (get locked-amount existing-lender)
        u0
      ))
    )
    (asserts! (< block-height (+ (var-get cycle-start-block) (var-get cycle-raise-term))) err-incorrect-cycle-term)
    (asserts! (>= (var-get cycle-max-capital) (+ current-lending-liquidity amount)) err-max-capital-reached)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    (asserts! (not (is-eq amount u0)) err-invalid-amount)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set vault-lending-liquidity (+ current-lending-liquidity amount))
    (map-set lenders tx-sender {
      locked-amount: (+ current-locked-amount amount),
      compound: compound,
    })
    (asserts! (is-some (index-of (var-get active-lenders) tx-sender))
      (ok (var-set active-lenders (unwrap! (as-max-len? (append (var-get active-lenders) tx-sender) u5000) err-appending-to-list)))
    )
    (ok true)
  )
)

(define-public (withdrawal-capital (amount uint))
  (let (
      (current-locked-amount (match (map-get? lenders tx-sender) existing-lender
        (get locked-amount existing-lender)
        u0
      ))
      (withdrawal-amount (if (>= amount current-locked-amount) current-locked-amount amount))
    )
    (asserts! (is-some (map-get? lenders tx-sender)) err-unauthorized)
    (asserts! (< block-height (+ (var-get cycle-start-block) (var-get cycle-raise-term))) err-incorrect-cycle-term)
    (asserts! (not (is-eq amount u0)) err-invalid-amount)
    
    (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
    (var-set vault-lending-liquidity (- (var-get vault-lending-liquidity) amount))
    (map-set lenders tx-sender {
      locked-amount: (- current-locked-amount amount),
      compound: (get compound (unwrap! (map-get? lenders tx-sender) err-unauthorized)),
    })
    (var-set temp-principal tx-sender)
    (asserts! (> (- current-locked-amount amount) u0)
      (ok (var-set active-lenders (filter not-temp-principal (var-get active-lenders))))
    )
    (ok true)
  )
)

;; loan functions
(define-public (new-loan (amount uint) (asset-id uint) (term-length uint))
  (let (
      (borrower tx-sender)
      (borrower-active-loan-ids (default-to (list ) (map-get? borrowers borrower)))
      (loan-id (+ u1 (var-get loan-count)))
      (fee-amount (/ (* amount (var-get loan-fee)) u10000))
      (interest-rate (get-term-interest-rate term-length))
      (interest (* amount interest-rate))
      (available-liquidity (var-get vault-lending-liquidity))
    )
    (asserts! (and (not (is-eq amount u0)) (<= amount (var-get loan-max-value))) err-invalid-amount)
    (asserts! (is-some (map-get? lenders borrower)) err-unauthorized)
    (asserts! (>= block-height (+ (var-get cycle-start-block) (var-get cycle-raise-term))) err-incorrect-cycle-term)
    (asserts! (and 
      (>= term-length (var-get loan-term-min))
      (<= (+ block-height term-length) (+ (var-get cycle-start-block) (var-get cycle-length)))
    ) err-invalid-term-length)
    (asserts! (is-eq (ok (some borrower)) (contract-call? .megapont-ape-club-nft get-owner asset-id)) err-asset-not-owned)
    (asserts! (>= (stx-get-balance tx-sender) (+ amount fee-amount)) err-insufficient-funds)

    (unwrap! (contract-call? .megapont-ape-club-nft transfer asset-id borrower (as-contract tx-sender)) err-on-asset-transfer)
    (unwrap! (stx-transfer? fee-amount borrower LIQUIDIUM-ACCOUNT) err-on-fee-transfer)
    (unwrap! (as-contract (stx-transfer? amount tx-sender borrower)) err-on-stx-transfer)
    (var-set vault-lending-liquidity (- available-liquidity amount))
    (var-set active-loans (unwrap! (as-max-len? (concat (list loan-id) (var-get active-loans)) u1000) err-max-vault-active-loans))
    (map-set borrowers borrower (unwrap! (as-max-len? (concat (list loan-id) borrower-active-loan-ids) u50) err-max-user-active-loans))
    (map-set loans loan-id {
      asset-id: asset-id,
      debt: (+ amount interest),
      principal: amount,
      interest: interest,
      interest-rate: interest-rate,
      start: block-height,
      end: term-length,
      borrower: borrower
    })
    (ok true)
  )
)

(define-public (pay-loan (amount uint) (loan-id uint))
  (let (
      (loan (unwrap! (map-get? loans loan-id) err-loan-not-found))
      (loan-debt (get debt loan))
      (loan-payment-amount (if (>= amount loan-debt) loan-debt amount))
      (loan-updated-debt (- loan-debt loan-payment-amount))
      (loan-principal (get principal loan))
      (loan-asset-id (get asset-id loan))
      (borrower (get borrower loan))
      (borrower-active-loans (default-to (list ) (map-get? borrowers borrower)))
    )
    (asserts! (is-eq tx-sender borrower) err-unauthorized)
    (asserts! (is-some (index-of (var-get active-loans) loan-id)) err-loan-not-active)
    (asserts! (>= (stx-get-balance tx-sender) loan-payment-amount) err-insufficient-funds)
    
    (map-set loans loan-id {
      asset-id: loan-asset-id,
      debt: loan-updated-debt,
      principal: loan-principal,
      interest: (get interest loan),
      interest-rate: (get interest-rate loan),
      start: (get start loan),
      end: (get end loan),
      borrower: borrower
    })
    (if (> loan-debt loan-principal) ;; if interest is still due
      (if (>= (- loan-debt loan-principal) loan-payment-amount) ;; if interest due is greater than payment amount
        (unwrap! (stx-transfer? loan-payment-amount borrower LIQUIDIUM-ACCOUNT) err-on-stx-transfer) ;; then pay part of interest due with all loan-payment-amount to LIQM ACCOUNT
        (begin
          (unwrap! (stx-transfer? (- loan-debt loan-principal) borrower LIQUIDIUM-ACCOUNT) err-on-stx-transfer) ;; else send rest of interest due to LIQM ACCOUNT
          (unwrap! (stx-transfer? (- loan-payment-amount (- loan-debt loan-principal)) borrower (as-contract tx-sender)) err-on-stx-transfer)
          (var-set vault-lending-liquidity (+ (var-get vault-lending-liquidity) (- loan-payment-amount (- loan-debt loan-principal))))
      ))
      (begin
        (unwrap! (stx-transfer? loan-payment-amount borrower (as-contract tx-sender)) err-on-stx-transfer) ;; else no interest due, so send all back to contract
        (var-set vault-lending-liquidity (+ (var-get vault-lending-liquidity) loan-payment-amount))
    ))
    (asserts! (is-eq loan-updated-debt u0) (ok true))
    (var-set temp-uint loan-id)
    (var-set active-loans (filter not-temp-uint (var-get active-loans)))
    (map-set borrowers borrower (filter not-temp-uint borrower-active-loans))
    (unwrap! (as-contract (contract-call? .megapont-ape-club-nft transfer loan-asset-id tx-sender borrower)) err-on-asset-transfer)
    (ok true)
    ;; TODO: double check this change ^^^
  )
)

;; auction functions

;; admin functions
(define-read-only (is-admin (account principal))
  (or 
    (is-eq tx-sender DEPLOYER-ACCOUNT)
    (default-to false (map-get? admins account))
  )
)

(define-public (set-admin (account principal) (value bool))
  (begin
    (asserts! (is-eq tx-sender DEPLOYER-ACCOUNT) err-unauthorized)
    (ok (map-set admins account value))
  )
)

(define-public (set-loan-fee (value uint))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (asserts! (not (is-eq value u0)) err-invalid-input)
    (ok (var-set loan-fee value))
  )
)

;; TODO: add back  after solution found
;; (define-public (set-loan-term-max (value uint))
;;   (begin
;;     (asserts! (is-admin tx-sender) err-unauthorized)
;;     (asserts! (not (is-eq value u0)) err-invalid-input)
;;     (ok (var-set loan-term-max value))
;;   )
;; )

(define-public (set-loan-term-min (value uint))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (asserts! (not (is-eq value u0)) err-invalid-input)
    (ok (var-set loan-term-min value))
  )
)

(define-public (set-loan-ltv-max (value uint))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (asserts! (not (is-eq value u0)) err-invalid-input)
    (ok (var-set loan-ltv-max value))
  )
)

(define-public (set-loan-ltv-liquidation-threshold (value uint))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (asserts! (not (is-eq value u0)) err-invalid-input)
    (ok (var-set loan-ltv-liquidation-threshold value))
  )
)

(define-public (set-cycle-lend-term (value uint))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (asserts! (not (is-eq value u0)) err-invalid-input)
    (ok (var-set cycle-lend-term value))
  )
)

(define-public (set-cycle-raise-term (value uint))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (asserts! (not (is-eq value u0)) err-invalid-input)
    (ok (var-set cycle-raise-term value))
  )
)

(define-public (set-cycle-max-capital (value uint))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (asserts! (not (is-eq value u0)) err-invalid-input)
    (ok (var-set cycle-max-capital value))
  )
)

(define-public (set-loan-interest-rates (rates (list 5 (tuple (interestRate uint) (termLengthMin uint)))))
  (begin
    (asserts! (is-admin tx-sender) err-unauthorized)
    (ok (var-set loan-interest-rates rates))
  )
)
