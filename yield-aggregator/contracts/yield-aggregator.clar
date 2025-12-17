;; Yield Aggregator - Auto-Compounding Yield Optimizer
;; Automatically compound yields from multiple DeFi protocols

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-strategy-inactive (err u105))

;; Data Variables
(define-data-var strategy-nonce uint u0)
(define-data-var total-value-locked uint u0)
(define-data-var performance-fee-percent uint u200) ;; 2%

;; Strategy Status
(define-constant status-active u1)
(define-constant status-paused u2)
(define-constant status-deprecated u3)

;; Data Maps
(define-map strategies
  { strategy-id: uint }
  {
    name: (string-ascii 64),
    protocol: principal,
    apy: uint,
    tvl: uint,
    status: uint,
    last-harvest: uint,
    total-harvested: uint,
    manager: principal
  }
)

(define-map user-positions
  { strategy-id: uint, user: principal }
  {
    deposited: uint,
    shares: uint,
    last-compound: uint,
    total-earned: uint
  }
)

(define-map strategy-performance
  { strategy-id: uint, epoch: uint }
  {
    start-tvl: uint,
    end-tvl: uint,
    yield-earned: uint,
    apy: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    total-deposited: uint,
    total-withdrawn: uint,
    total-earned: uint,
    active-strategies: uint
  }
)

;; Read-Only Functions
(define-read-only (get-strategy (strategy-id uint))
  (map-get? strategies { strategy-id: strategy-id })
)

(define-read-only (get-user-position (strategy-id uint) (user principal))
  (default-to
    { deposited: u0, shares: u0, last-compound: u0, total-earned: u0 }
    (map-get? user-positions { strategy-id: strategy-id, user: user })
  )
)

(define-read-only (get-strategy-performance (strategy-id uint) (epoch uint))
  (map-get? strategy-performance { strategy-id: strategy-id, epoch: epoch })
)

(define-read-only (get-user-stats (user principal))
  (default-to
    { total-deposited: u0, total-withdrawn: u0, total-earned: u0, active-strategies: u0 }
    (map-get? user-stats { user: user })
  )
)

(define-read-only (calculate-shares (strategy-id uint) (amount uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) err-not-found))
    (tvl (get tvl strategy))
  )
    (if (is-eq tvl u0)
      (ok amount)
      (ok (/ (* amount u1000000) tvl))
    )
  )
)

(define-read-only (get-total-value-locked)
  (ok (var-get total-value-locked))
)

;; Public Functions
(define-public (create-strategy 
    (name (string-ascii 64))
    (protocol principal)
    (initial-apy uint)
  )
  (let (
    (strategy-id (+ (var-get strategy-nonce) u1))
  )
    (map-set strategies
      { strategy-id: strategy-id }
      {
        name: name,
        protocol: protocol,
        apy: initial-apy,
        tvl: u0,
        status: status-active,
        last-harvest: block-height,
        total-harvested: u0,
        manager: tx-sender
      }
    )
    
    (var-set strategy-nonce strategy-id)
    (ok strategy-id)
  )
)

(define-public (deposit (strategy-id uint) (amount uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) err-not-found))
    (user-pos (get-user-position strategy-id tx-sender))
    (user-info (get-user-stats tx-sender))
    (shares (unwrap! (calculate-shares strategy-id amount) err-invalid-amount))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-eq (get status strategy) status-active) err-strategy-inactive)
    
    (map-set user-positions
      { strategy-id: strategy-id, user: tx-sender }
      {
        deposited: (+ (get deposited user-pos) amount),
        shares: (+ (get shares user-pos) shares),
        last-compound: block-height,
        total-earned: (get total-earned user-pos)
      }
    )
    
    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy {
        tvl: (+ (get tvl strategy) amount)
      })
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge user-info {
        total-deposited: (+ (get total-deposited user-info) amount),
        active-strategies: (if (is-eq (get shares user-pos) u0)
          (+ (get active-strategies user-info) u1)
          (get active-strategies user-info)
        )
      })
    )
    
    (var-set total-value-locked (+ (var-get total-value-locked) amount))
    (ok shares)
  )
)

(define-public (withdraw (strategy-id uint) (shares uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) err-not-found))
    (user-pos (get-user-position strategy-id tx-sender))
    (user-info (get-user-stats tx-sender))
    (withdrawal-amount (/ (* shares (get tvl strategy)) u1000000))
  )
    (asserts! (> shares u0) err-invalid-amount)
    (asserts! (>= (get shares user-pos) shares) err-insufficient-balance)
    
    (map-set user-positions
      { strategy-id: strategy-id, user: tx-sender }
      {
        deposited: (get deposited user-pos),
        shares: (- (get shares user-pos) shares),
        last-compound: (get last-compound user-pos),
        total-earned: (get total-earned user-pos)
      }
    )
    
    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy {
        tvl: (- (get tvl strategy) withdrawal-amount)
      })
    )
    
    (map-set user-stats
      { user: tx-sender }
      (merge user-info {
        total-withdrawn: (+ (get total-withdrawn user-info) withdrawal-amount)
      })
    )
    
    (var-set total-value-locked (- (var-get total-value-locked) withdrawal-amount))
    (ok withdrawal-amount)
  )
)

(define-public (harvest (strategy-id uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) err-not-found))
    (time-elapsed (- block-height (get last-harvest strategy)))
    (yield-earned (/ (* (get tvl strategy) (get apy strategy) time-elapsed) u52560000))
    (performance-fee (/ (* yield-earned (var-get performance-fee-percent)) u10000))
    (net-yield (- yield-earned performance-fee))
  )
    (asserts! (is-eq (get status strategy) status-active) err-strategy-inactive)
    
    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy {
        tvl: (+ (get tvl strategy) net-yield),
        last-harvest: block-height,
        total-harvested: (+ (get total-harvested strategy) yield-earned)
      })
    )
    
    (var-set total-value-locked (+ (var-get total-value-locked) net-yield))
    (ok net-yield)
  )
)

(define-public (compound (strategy-id uint))
  (let (
    (user-pos (get-user-position strategy-id tx-sender))
    (user-info (get-user-stats tx-sender))
  )
    (try! (harvest strategy-id))
    
    (map-set user-positions
      { strategy-id: strategy-id, user: tx-sender }
      (merge user-pos {
        last-compound: block-height
      })
    )
    
    (ok true)
  )
)

(define-public (update-strategy-status (strategy-id uint) (new-status uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get manager strategy)) err-unauthorized)
    
    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy { status: new-status })
    )
    
    (ok true)
  )
)

(define-public (set-performance-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set performance-fee-percent new-fee)
    (ok true)
  )
)
