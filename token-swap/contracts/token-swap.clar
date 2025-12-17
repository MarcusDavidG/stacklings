;; Token Swap - AMM/DEX
;; Automated Market Maker for decentralized token swaps with liquidity pools

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-liquidity (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-slippage-too-high (err u103))
(define-constant err-pool-exists (err u104))
(define-constant err-pool-not-found (err u105))

;; Data Variables
(define-data-var platform-fee-percent uint u30) ;; 0.3%
(define-data-var total-pools-created uint u0)

;; Data Maps
(define-map liquidity-pools
  { pool-id: uint }
  {
    token-a: principal,
    token-b: principal,
    reserve-a: uint,
    reserve-b: uint,
    total-shares: uint,
    creator: principal,
    created-at: uint
  }
)

(define-map liquidity-providers
  { pool-id: uint, provider: principal }
  { shares: uint, deposited-at: uint }
)

(define-map pool-by-tokens
  { token-a: principal, token-b: principal }
  { pool-id: uint }
)

;; Read-Only Functions
(define-read-only (get-pool (pool-id uint))
  (map-get? liquidity-pools { pool-id: pool-id })
)

(define-read-only (get-provider-shares (pool-id uint) (provider principal))
  (default-to 
    { shares: u0, deposited-at: u0 }
    (map-get? liquidity-providers { pool-id: pool-id, provider: provider })
  )
)

(define-read-only (get-pool-id-by-tokens (token-a principal) (token-b principal))
  (map-get? pool-by-tokens { token-a: token-a, token-b: token-b })
)

(define-read-only (calculate-swap-output (amount-in uint) (reserve-in uint) (reserve-out uint))
  (let (
    (amount-in-with-fee (- amount-in (/ (* amount-in (var-get platform-fee-percent)) u10000)))
    (numerator (* amount-in-with-fee reserve-out))
    (denominator (+ reserve-in amount-in-with-fee))
  )
    (ok (/ numerator denominator))
  )
)

(define-read-only (get-platform-fee)
  (ok (var-get platform-fee-percent))
)

(define-read-only (get-total-pools)
  (ok (var-get total-pools-created))
)

;; Public Functions
(define-public (create-pool (token-a principal) (token-b principal) (amount-a uint) (amount-b uint))
  (let (
    (pool-id (+ (var-get total-pools-created) u1))
    (initial-shares (+ amount-a amount-b))
  )
    (asserts! (> amount-a u0) err-invalid-amount)
    (asserts! (> amount-b u0) err-invalid-amount)
    (asserts! (is-none (get-pool-id-by-tokens token-a token-b)) err-pool-exists)
    
    (map-set liquidity-pools
      { pool-id: pool-id }
      {
        token-a: token-a,
        token-b: token-b,
        reserve-a: amount-a,
        reserve-b: amount-b,
        total-shares: initial-shares,
        creator: tx-sender,
        created-at: block-height
      }
    )
    
    (map-set pool-by-tokens
      { token-a: token-a, token-b: token-b }
      { pool-id: pool-id }
    )
    
    (map-set liquidity-providers
      { pool-id: pool-id, provider: tx-sender }
      { shares: initial-shares, deposited-at: block-height }
    )
    
    (var-set total-pools-created pool-id)
    (ok pool-id)
  )
)

(define-public (add-liquidity (pool-id uint) (amount-a uint) (amount-b uint))
  (let (
    (pool (unwrap! (get-pool pool-id) err-pool-not-found))
    (shares-to-mint (/ (* amount-a (get total-shares pool)) (get reserve-a pool)))
    (provider-info (get-provider-shares pool-id tx-sender))
  )
    (asserts! (> amount-a u0) err-invalid-amount)
    (asserts! (> amount-b u0) err-invalid-amount)
    
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool {
        reserve-a: (+ (get reserve-a pool) amount-a),
        reserve-b: (+ (get reserve-b pool) amount-b),
        total-shares: (+ (get total-shares pool) shares-to-mint)
      })
    )
    
    (map-set liquidity-providers
      { pool-id: pool-id, provider: tx-sender }
      { 
        shares: (+ (get shares provider-info) shares-to-mint),
        deposited-at: block-height
      }
    )
    
    (ok shares-to-mint)
  )
)

(define-public (swap-a-for-b (pool-id uint) (amount-in uint) (min-amount-out uint))
  (let (
    (pool (unwrap! (get-pool pool-id) err-pool-not-found))
    (amount-out (unwrap! (calculate-swap-output amount-in (get reserve-a pool) (get reserve-b pool)) err-insufficient-liquidity))
  )
    (asserts! (> amount-in u0) err-invalid-amount)
    (asserts! (>= amount-out min-amount-out) err-slippage-too-high)
    
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool {
        reserve-a: (+ (get reserve-a pool) amount-in),
        reserve-b: (- (get reserve-b pool) amount-out)
      })
    )
    
    (ok amount-out)
  )
)

(define-public (swap-b-for-a (pool-id uint) (amount-in uint) (min-amount-out uint))
  (let (
    (pool (unwrap! (get-pool pool-id) err-pool-not-found))
    (amount-out (unwrap! (calculate-swap-output amount-in (get reserve-b pool) (get reserve-a pool)) err-insufficient-liquidity))
  )
    (asserts! (> amount-in u0) err-invalid-amount)
    (asserts! (>= amount-out min-amount-out) err-slippage-too-high)
    
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool {
        reserve-b: (+ (get reserve-b pool) amount-in),
        reserve-a: (- (get reserve-a pool) amount-out)
      })
    )
    
    (ok amount-out)
  )
)

(define-public (remove-liquidity (pool-id uint) (shares uint))
  (let (
    (pool (unwrap! (get-pool pool-id) err-pool-not-found))
    (provider-info (get-provider-shares pool-id tx-sender))
    (amount-a (/ (* shares (get reserve-a pool)) (get total-shares pool)))
    (amount-b (/ (* shares (get reserve-b pool)) (get total-shares pool)))
  )
    (asserts! (> shares u0) err-invalid-amount)
    (asserts! (>= (get shares provider-info) shares) err-insufficient-liquidity)
    
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool {
        reserve-a: (- (get reserve-a pool) amount-a),
        reserve-b: (- (get reserve-b pool) amount-b),
        total-shares: (- (get total-shares pool) shares)
      })
    )
    
    (map-set liquidity-providers
      { pool-id: pool-id, provider: tx-sender }
      { 
        shares: (- (get shares provider-info) shares),
        deposited-at: (get deposited-at provider-info)
      }
    )
    
    (ok { amount-a: amount-a, amount-b: amount-b })
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set platform-fee-percent new-fee)
    (ok true)
  )
)
