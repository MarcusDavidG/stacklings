;; Token Vesting - Vesting schedules with cliff periods
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u102))
(define-constant err-not-vested (err u103))

(define-data-var next-schedule-id uint u1)

(define-map vesting-schedules
  uint
  {
    beneficiary: principal,
    total-amount: uint,
    released-amount: uint,
    start-block: uint,
    cliff-duration: uint,
    vesting-duration: uint,
    revocable: bool,
    revoked: bool
  }
)

(define-read-only (get-schedule (schedule-id uint))
  (map-get? vesting-schedules schedule-id)
)

(define-read-only (calculate-vested-amount (schedule-id uint))
  (match (map-get? vesting-schedules schedule-id)
    schedule
      (let
        ((elapsed (- stacks-block-height (get start-block schedule))))
        (if (< elapsed (get cliff-duration schedule))
          (ok u0)
          (if (>= elapsed (get vesting-duration schedule))
            (ok (get total-amount schedule))
            (ok (/ (* (get total-amount schedule) elapsed) (get vesting-duration schedule)))
          )
        )
      )
    (ok u0)
  )
)

(define-public (create-vesting (beneficiary principal) (amount uint) (cliff uint) (duration uint))
  (let ((schedule-id (var-get next-schedule-id)))
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (try! (stx-transfer-memo? amount tx-sender (as-contract tx-sender) 0x766573696e672063726561746520))
    (map-set vesting-schedules schedule-id {
      beneficiary: beneficiary,
      total-amount: amount,
      released-amount: u0,
      start-block: stacks-block-height,
      cliff-duration: cliff,
      vesting-duration: duration,
      revocable: true,
      revoked: false
    })
    (var-set next-schedule-id (+ schedule-id u1))
    (ok schedule-id)
  )
)

(define-public (release (schedule-id uint))
  (match (map-get? vesting-schedules schedule-id)
    schedule
      (let
        (
          (vested (unwrap! (calculate-vested-amount schedule-id) err-not-vested))
          (releasable (- vested (get released-amount schedule)))
        )
        (asserts! (is-eq tx-sender (get beneficiary schedule)) err-unauthorized)
        (asserts! (not (get revoked schedule)) err-unauthorized)
        (asserts! (> releasable u0) err-not-vested)
        (try! (as-contract (stx-transfer-memo? releasable tx-sender (get beneficiary schedule) 0x766573696e672072656c65617365)))
        (map-set vesting-schedules schedule-id (merge schedule { released-amount: vested }))
        (ok releasable)
      )
    (err u101)
  )
)
