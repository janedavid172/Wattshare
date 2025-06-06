(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_DEVICE_NOT_FOUND (err u103))
(define-constant ERR_DEVICE_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_PRICE (err u105))
(define-constant ERR_INSUFFICIENT_TOKENS (err u106))
(define-constant ERR_SELF_PURCHASE (err u107))

(define-fungible-token watt-token)

(define-data-var total-energy-tokenized uint u0)
(define-data-var platform-fee-rate uint u250)

(define-map devices
  { device-id: (string-ascii 50) }
  {
    owner: principal,
    energy-capacity: uint,
    energy-available: uint,
    price-per-watt: uint,
    is-active: bool,
    total-generated: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    total-energy-sold: uint,
    total-energy-bought: uint,
    devices-count: uint,
    reputation-score: uint
  }
)

(define-map energy-transactions
  { tx-id: uint }
  {
    seller: principal,
    buyer: principal,
    device-id: (string-ascii 50),
    energy-amount: uint,
    price-paid: uint,
    timestamp: uint
  }
)

(define-data-var next-tx-id uint u1)

(define-public (register-device (device-id (string-ascii 50)) (energy-capacity uint) (price-per-watt uint))
  (let ((device-exists (is-some (map-get? devices { device-id: device-id }))))
    (asserts! (not device-exists) ERR_DEVICE_ALREADY_EXISTS)
    (asserts! (> energy-capacity u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-watt u0) ERR_INVALID_PRICE)
    (map-set devices
      { device-id: device-id }
      {
        owner: tx-sender,
        energy-capacity: energy-capacity,
        energy-available: u0,
        price-per-watt: price-per-watt,
        is-active: true,
        total-generated: u0
      }
    )
    (update-user-profile tx-sender u0 u0 u1 u0)
    (ok device-id)
  )
)

(define-public (tokenize-energy (device-id (string-ascii 50)) (energy-amount uint))
  (let ((device-data (unwrap! (map-get? devices { device-id: device-id }) ERR_DEVICE_NOT_FOUND)))
    (asserts! (is-eq (get owner device-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> energy-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active device-data) ERR_DEVICE_NOT_FOUND)
    (let ((new-available (+ (get energy-available device-data) energy-amount))
          (new-total-generated (+ (get total-generated device-data) energy-amount)))
      (asserts! (<= new-available (get energy-capacity device-data)) ERR_INVALID_AMOUNT)
      (try! (ft-mint? watt-token energy-amount tx-sender))
      (map-set devices
        { device-id: device-id }
        (merge device-data {
          energy-available: new-available,
          total-generated: new-total-generated
        })
      )
      (var-set total-energy-tokenized (+ (var-get total-energy-tokenized) energy-amount))
      (ok energy-amount)
    )
  )
)

(define-public (list-energy-for-sale (device-id (string-ascii 50)) (energy-amount uint) (price-per-watt uint))
  (let ((device-data (unwrap! (map-get? devices { device-id: device-id }) ERR_DEVICE_NOT_FOUND)))
    (asserts! (is-eq (get owner device-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> energy-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-watt u0) ERR_INVALID_PRICE)
    (asserts! (>= (get energy-available device-data) energy-amount) ERR_INSUFFICIENT_BALANCE)
    (map-set devices
      { device-id: device-id }
      (merge device-data {
        price-per-watt: price-per-watt
      })
    )
    (ok true)
  )
)

(define-public (purchase-energy (device-id (string-ascii 50)) (energy-amount uint))
  (let ((device-data (unwrap! (map-get? devices { device-id: device-id }) ERR_DEVICE_NOT_FOUND))
        (total-cost (* energy-amount (get price-per-watt device-data)))
        (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
        (seller-payment (- total-cost platform-fee)))
    (asserts! (not (is-eq tx-sender (get owner device-data))) ERR_SELF_PURCHASE)
    (asserts! (> energy-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get energy-available device-data) energy-amount) ERR_INSUFFICIENT_TOKENS)
    (asserts! (get is-active device-data) ERR_DEVICE_NOT_FOUND)
    (try! (stx-transfer? total-cost tx-sender (get owner device-data)))
    (try! (ft-transfer? watt-token energy-amount (get owner device-data) tx-sender))
    (map-set devices
      { device-id: device-id }
      (merge device-data {
        energy-available: (- (get energy-available device-data) energy-amount)
      })
    )
    (record-transaction (get owner device-data) tx-sender device-id energy-amount total-cost)
    (update-user-profile (get owner device-data) energy-amount u0 u0 u10)
    (update-user-profile tx-sender u0 energy-amount u0 u5)
    (ok energy-amount)
  )
)

(define-public (deactivate-device (device-id (string-ascii 50)))
  (let ((device-data (unwrap! (map-get? devices { device-id: device-id }) ERR_DEVICE_NOT_FOUND)))
    (asserts! (is-eq (get owner device-data) tx-sender) ERR_UNAUTHORIZED)
    (map-set devices
      { device-id: device-id }
      (merge device-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (activate-device (device-id (string-ascii 50)))
  (let ((device-data (unwrap! (map-get? devices { device-id: device-id }) ERR_DEVICE_NOT_FOUND)))
    (asserts! (is-eq (get owner device-data) tx-sender) ERR_UNAUTHORIZED)
    (map-set devices
      { device-id: device-id }
      (merge device-data { is-active: true })
    )
    (ok true)
  )
)

(define-public (update-device-price (device-id (string-ascii 50)) (new-price uint))
  (let ((device-data (unwrap! (map-get? devices { device-id: device-id }) ERR_DEVICE_NOT_FOUND)))
    (asserts! (is-eq (get owner device-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_PRICE)
    (map-set devices
      { device-id: device-id }
      (merge device-data { price-per-watt: new-price })
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR_INVALID_AMOUNT)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-read-only (get-device-info (device-id (string-ascii 50)))
  (map-get? devices { device-id: device-id })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? energy-transactions { tx-id: tx-id })
)

(define-read-only (get-watt-balance (user principal))
  (ft-get-balance watt-token user)
)

(define-read-only (get-total-supply)
  (ft-get-supply watt-token)
)

(define-read-only (get-platform-stats)
  {
    total-energy-tokenized: (var-get total-energy-tokenized),
    platform-fee-rate: (var-get platform-fee-rate),
    total-transactions: (- (var-get next-tx-id) u1)
  }
)

(define-read-only (calculate-purchase-cost (device-id (string-ascii 50)) (energy-amount uint))
  (match (map-get? devices { device-id: device-id })
    device-data
    (let ((total-cost (* energy-amount (get price-per-watt device-data)))
          (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000)))
      (ok {
        total-cost: total-cost,
        platform-fee: platform-fee,
        seller-receives: (- total-cost platform-fee)
      })
    )
    ERR_DEVICE_NOT_FOUND
  )
)

(define-private (record-transaction (seller principal) (buyer principal) (device-id (string-ascii 50)) (energy-amount uint) (price-paid uint))
  (let ((tx-id (var-get next-tx-id)))
    (map-set energy-transactions
      { tx-id: tx-id }
      {
        seller: seller,
        buyer: buyer,
        device-id: device-id,
        energy-amount: energy-amount,
        price-paid: price-paid,
        timestamp: stacks-block-height
      }
    )
    (var-set next-tx-id (+ tx-id u1))
    tx-id
  )
)

(define-private (update-user-profile (user principal) (energy-sold uint) (energy-bought uint) (devices-added uint) (reputation-points uint))
  (let ((current-profile (default-to
    { total-energy-sold: u0, total-energy-bought: u0, devices-count: u0, reputation-score: u0 }
    (map-get? user-profiles { user: user }))))
    (map-set user-profiles
      { user: user }
      {
        total-energy-sold: (+ (get total-energy-sold current-profile) energy-sold),
        total-energy-bought: (+ (get total-energy-bought current-profile) energy-bought),
        devices-count: (+ (get devices-count current-profile) devices-added),
        reputation-score: (+ (get reputation-score current-profile) reputation-points)
      }
    )
  )
)