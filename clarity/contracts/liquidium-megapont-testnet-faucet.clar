
;; liquidium-megapont-testnet-faucet
;; https://liquidium.finance/

;; error codes
(define-constant ERR_NO_MEGAPONTS (err u1001))
(define-constant ERR_ASSET_NOT_OWNED (err u1002))
(define-constant ERR_ASSET_LIMIT_REACHED (err u1003))
(define-constant ERR_ASSET_TRANSFER_FAILED (err u1004))

;; data maps and vars
(define-data-var maxMint uint u3)
(define-data-var megapontIndex uint u0)

(define-map balances principal uint)
(define-map megaponts uint {assetId: uint})

;; private functions
(define-private (get-last-megapont)
  (map-get? megaponts (- (var-get megapontIndex) u1))
)

;; read only functions
(define-read-only (get-index) (var-get megapontIndex))

;; public functions
(define-public (deposit-megapont (assetId uint))
  (let
    (
      (index (var-get megapontIndex))
    ) 
    (map-set megaponts index {assetId: assetId})
    (var-set megapontIndex (+ index u1))
    (asserts! (is-eq (ok (some tx-sender)) (contract-call? .megapont-ape-club-nft get-owner assetId))
      ERR_ASSET_NOT_OWNED
    )
    (asserts! (not (is-err (contract-call? .megapont-ape-club-nft transfer assetId tx-sender (as-contract tx-sender))))
      ERR_ASSET_TRANSFER_FAILED
    )
    (ok true)
  )
)

(define-public (withdrawl-megapont)
  (let
    (
      (index (var-get megapontIndex))
      (current-balance
        (default-to u0 (map-get? balances tx-sender))
      )
      (tester tx-sender)
    )
    (asserts! (not (is-eq index u0)) ERR_NO_MEGAPONTS)
    (asserts! (<= current-balance u3) ERR_ASSET_LIMIT_REACHED)
    (asserts! (is-eq (ok (some (as-contract tx-sender))) (contract-call? .megapont-ape-club-nft get-owner
      (get assetId (unwrap! (get-last-megapont) ERR_NO_MEGAPONTS))
    ))
      ERR_ASSET_NOT_OWNED
    )
    (asserts! (not (is-err (as-contract (contract-call? .megapont-ape-club-nft transfer
      (get assetId (unwrap! (get-last-megapont) ERR_NO_MEGAPONTS))
    tx-sender tester))))
      ERR_ASSET_TRANSFER_FAILED
    )
    (map-set balances tester (+ current-balance u1))
    (var-set megapontIndex (- index u1))
    (ok true)
  )
)
