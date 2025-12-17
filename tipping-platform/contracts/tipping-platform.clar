;; Tipping Platform - Content Creator Tipping System
;; Support content creators with crypto tips and subscriptions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-self-tip (err u103))
(define-constant err-already-subscribed (err u104))

;; Data Variables
(define-data-var platform-fee-percent uint u250) ;; 2.5%
(define-data-var tip-nonce uint u0)

;; Data Maps
(define-map creator-profiles
  { creator: principal }
  {
    display-name: (string-ascii 64),
    bio: (string-ascii 256),
    total-received: uint,
    tip-count: uint,
    subscriber-count: uint,
    created-at: uint
  }
)

(define-map tips
  { tip-id: uint }
  {
    from: principal,
    to: principal,
    amount: uint,
    message: (optional (string-ascii 256)),
    timestamp: uint
  }
)

(define-map subscriptions
  { subscriber: principal, creator: principal }
  {
    amount: uint,
    started-at: uint,
    last-payment: uint,
    active: bool
  }
)

(define-map supporter-stats
  { supporter: principal }
  {
    total-tipped: uint,
    tip-count: uint,
    subscriptions: uint
  }
)

;; Read-Only Functions
(define-read-only (get-creator-profile (creator principal))
  (map-get? creator-profiles { creator: creator })
)

(define-read-only (get-tip (tip-id uint))
  (map-get? tips { tip-id: tip-id })
)

(define-read-only (get-subscription (subscriber principal) (creator principal))
  (map-get? subscriptions { subscriber: subscriber, creator: creator })
)

(define-read-only (get-supporter-stats (supporter principal))
  (default-to
    { total-tipped: u0, tip-count: u0, subscriptions: u0 }
    (map-get? supporter-stats { supporter: supporter })
  )
)

(define-read-only (is-subscribed (subscriber principal) (creator principal))
  (match (get-subscription subscriber creator)
    sub (get active sub)
    false
  )
)

(define-read-only (get-platform-fee)
  (ok (var-get platform-fee-percent))
)

;; Public Functions
(define-public (create-profile (display-name (string-ascii 64)) (bio (string-ascii 256)))
  (begin
    (map-set creator-profiles
      { creator: tx-sender }
      {
        display-name: display-name,
        bio: bio,
        total-received: u0,
        tip-count: u0,
        subscriber-count: u0,
        created-at: block-height
      }
    )
    
    (ok true)
  )
)

(define-public (update-profile (display-name (string-ascii 64)) (bio (string-ascii 256)))
  (let (
    (profile (unwrap! (get-creator-profile tx-sender) err-not-found))
  )
    (map-set creator-profiles
      { creator: tx-sender }
      (merge profile {
        display-name: display-name,
        bio: bio
      })
    )
    
    (ok true)
  )
)

(define-public (send-tip (creator principal) (amount uint) (message (optional (string-ascii 256))))
  (let (
    (tip-id (+ (var-get tip-nonce) u1))
    (platform-fee (/ (* amount (var-get platform-fee-percent)) u10000))
    (creator-amount (- amount platform-fee))
    (creator-profile (unwrap! (get-creator-profile creator) err-not-found))
    (supporter-info (get-supporter-stats tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq tx-sender creator)) err-self-tip)
    
    (map-set tips
      { tip-id: tip-id }
      {
        from: tx-sender,
        to: creator,
        amount: amount,
        message: message,
        timestamp: block-height
      }
    )
    
    (map-set creator-profiles
      { creator: creator }
      (merge creator-profile {
        total-received: (+ (get total-received creator-profile) creator-amount),
        tip-count: (+ (get tip-count creator-profile) u1)
      })
    )
    
    (map-set supporter-stats
      { supporter: tx-sender }
      {
        total-tipped: (+ (get total-tipped supporter-info) amount),
        tip-count: (+ (get tip-count supporter-info) u1),
        subscriptions: (get subscriptions supporter-info)
      }
    )
    
    (var-set tip-nonce tip-id)
    (ok tip-id)
  )
)

(define-public (subscribe (creator principal) (amount uint))
  (let (
    (creator-profile (unwrap! (get-creator-profile creator) err-not-found))
    (supporter-info (get-supporter-stats tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq tx-sender creator)) err-self-tip)
    (asserts! (not (is-subscribed tx-sender creator)) err-already-subscribed)
    
    (map-set subscriptions
      { subscriber: tx-sender, creator: creator }
      {
        amount: amount,
        started-at: block-height,
        last-payment: block-height,
        active: true
      }
    )
    
    (map-set creator-profiles
      { creator: creator }
      (merge creator-profile {
        subscriber-count: (+ (get subscriber-count creator-profile) u1)
      })
    )
    
    (map-set supporter-stats
      { supporter: tx-sender }
      (merge supporter-info {
        subscriptions: (+ (get subscriptions supporter-info) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (unsubscribe (creator principal))
  (let (
    (subscription (unwrap! (get-subscription tx-sender creator) err-not-found))
    (creator-profile (unwrap! (get-creator-profile creator) err-not-found))
    (supporter-info (get-supporter-stats tx-sender))
  )
    (asserts! (get active subscription) err-not-found)
    
    (map-set subscriptions
      { subscriber: tx-sender, creator: creator }
      (merge subscription { active: false })
    )
    
    (map-set creator-profiles
      { creator: creator }
      (merge creator-profile {
        subscriber-count: (- (get subscriber-count creator-profile) u1)
      })
    )
    
    (map-set supporter-stats
      { supporter: tx-sender }
      (merge supporter-info {
        subscriptions: (- (get subscriptions supporter-info) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set platform-fee-percent new-fee)
    (ok true)
  )
)
