(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_DEVICE_NOT_FOUND (err u103))
(define-constant ERR_DEVICE_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_PRICE (err u105))
(define-constant ERR_INSUFFICIENT_TOKENS (err u106))
(define-constant ERR_SELF_PURCHASE (err u107))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u108))
(define-constant ERR_SUBSCRIPTION_ALREADY_EXISTS (err u109))
(define-constant ERR_SUBSCRIPTION_INACTIVE (err u110))
(define-constant ERR_SUBSCRIPTION_EXECUTION_FAILED (err u111))
(define-constant ERR_INVALID_INTERVAL (err u112))
(define-constant ERR_INSUFFICIENT_SUBSCRIPTION_BALANCE (err u113))

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
(define-data-var next-subscription-id uint u1)

(define-map subscriptions
  { subscription-id: uint }
  {
    subscriber: principal,
    device-id: (string-ascii 50),
    energy-amount: uint,
    interval-blocks: uint,
    max-price-per-watt: uint,
    prepaid-balance: uint,
    is-active: bool,
    created-at: uint,
    last-execution: uint,
    next-execution: uint,
    total-executions: uint
  }
)

(define-map user-subscriptions
  { user: principal, device-id: (string-ascii 50) }
  { subscription-id: uint }
)

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

(define-public (create-subscription (device-id (string-ascii 50)) (energy-amount uint) (interval-blocks uint) (max-price-per-watt uint) (prepaid-amount uint))
  (let ((device-data (unwrap! (map-get? devices { device-id: device-id }) ERR_DEVICE_NOT_FOUND))
        (subscription-id (var-get next-subscription-id))
        (existing-sub (map-get? user-subscriptions { user: tx-sender, device-id: device-id })))
    (asserts! (is-none existing-sub) ERR_SUBSCRIPTION_ALREADY_EXISTS)
    (asserts! (not (is-eq tx-sender (get owner device-data))) ERR_SELF_PURCHASE)
    (asserts! (> energy-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> interval-blocks u0) ERR_INVALID_INTERVAL)
    (asserts! (> max-price-per-watt u0) ERR_INVALID_PRICE)
    (asserts! (> prepaid-amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? prepaid-amount tx-sender (as-contract tx-sender)))
    (map-set subscriptions
      { subscription-id: subscription-id }
      {
        subscriber: tx-sender,
        device-id: device-id,
        energy-amount: energy-amount,
        interval-blocks: interval-blocks,
        max-price-per-watt: max-price-per-watt,
        prepaid-balance: prepaid-amount,
        is-active: true,
        created-at: stacks-block-height,
        last-execution: u0,
        next-execution: (+ stacks-block-height interval-blocks),
        total-executions: u0
      }
    )
    (map-set user-subscriptions
      { user: tx-sender, device-id: device-id }
      { subscription-id: subscription-id }
    )
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

(define-public (execute-subscription (subscription-id uint))
  (let ((subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
        (device-data (unwrap! (map-get? devices { device-id: (get device-id subscription-data) }) ERR_DEVICE_NOT_FOUND)))
    (asserts! (get is-active subscription-data) ERR_SUBSCRIPTION_INACTIVE)
    (asserts! (get is-active device-data) ERR_DEVICE_NOT_FOUND)
    (asserts! (>= stacks-block-height (get next-execution subscription-data)) ERR_SUBSCRIPTION_EXECUTION_FAILED)
    (asserts! (<= (get price-per-watt device-data) (get max-price-per-watt subscription-data)) ERR_INVALID_PRICE)
    (asserts! (>= (get energy-available device-data) (get energy-amount subscription-data)) ERR_INSUFFICIENT_TOKENS)
    (let ((total-cost (* (get energy-amount subscription-data) (get price-per-watt device-data)))
          (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
          (seller-payment (- total-cost platform-fee)))
      (asserts! (>= (get prepaid-balance subscription-data) total-cost) ERR_INSUFFICIENT_SUBSCRIPTION_BALANCE)
      (try! (as-contract (stx-transfer? seller-payment (as-contract tx-sender) (get owner device-data))))
      (try! (ft-transfer? watt-token (get energy-amount subscription-data) (get owner device-data) (get subscriber subscription-data)))
      (map-set devices
        { device-id: (get device-id subscription-data) }
        (merge device-data {
          energy-available: (- (get energy-available device-data) (get energy-amount subscription-data))
        })
      )
      (map-set subscriptions
        { subscription-id: subscription-id }
        (merge subscription-data {
          prepaid-balance: (- (get prepaid-balance subscription-data) total-cost),
          last-execution: stacks-block-height,
          next-execution: (+ stacks-block-height (get interval-blocks subscription-data)),
          total-executions: (+ (get total-executions subscription-data) u1)
        })
      )
      (record-transaction (get owner device-data) (get subscriber subscription-data) (get device-id subscription-data) (get energy-amount subscription-data) total-cost)
      (update-user-profile (get owner device-data) (get energy-amount subscription-data) u0 u0 u10)
      (update-user-profile (get subscriber subscription-data) u0 (get energy-amount subscription-data) u0 u5)
      (ok (get energy-amount subscription-data))
    )
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let ((subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get subscriber subscription-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription-data) ERR_SUBSCRIPTION_INACTIVE)
    (let ((refund-amount (get prepaid-balance subscription-data)))
      (begin
        (if (> refund-amount u0)
          (unwrap-panic (as-contract (stx-transfer? refund-amount (as-contract tx-sender) (get subscriber subscription-data))))
          true
        )
        (map-set subscriptions
          { subscription-id: subscription-id }
          (merge subscription-data {
            is-active: false,
            prepaid-balance: u0
          })
        )
        (map-delete user-subscriptions { user: (get subscriber subscription-data), device-id: (get device-id subscription-data) })
        (ok refund-amount)
      )
    )
  )
)

(define-public (top-up-subscription (subscription-id uint) (amount uint))
  (let ((subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get subscriber subscription-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription-data) ERR_SUBSCRIPTION_INACTIVE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription-data {
        prepaid-balance: (+ (get prepaid-balance subscription-data) amount)
      })
    )
    (ok (+ (get prepaid-balance subscription-data) amount))
  )
)

(define-public (update-subscription-max-price (subscription-id uint) (new-max-price uint))
  (let ((subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get subscriber subscription-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription-data) ERR_SUBSCRIPTION_INACTIVE)
    (asserts! (> new-max-price u0) ERR_INVALID_PRICE)
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription-data {
        max-price-per-watt: new-max-price
      })
    )
    (ok true)
  )
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-user-subscription (user principal) (device-id (string-ascii 50)))
  (match (map-get? user-subscriptions { user: user, device-id: device-id })
    subscription-ref (map-get? subscriptions { subscription-id: (get subscription-id subscription-ref) })
    none
  )
)

(define-read-only (can-execute-subscription (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription-data
    (and
      (get is-active subscription-data)
      (>= stacks-block-height (get next-execution subscription-data))
      (> (get prepaid-balance subscription-data) u0)
    )
    false
  )
)

(define-read-only (get-subscription-stats (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription-data
    (ok {
      total-executions: (get total-executions subscription-data),
      prepaid-balance: (get prepaid-balance subscription-data),
      next-execution-block: (get next-execution subscription-data),
      blocks-until-execution: (if (>= stacks-block-height (get next-execution subscription-data))
        u0
        (- (get next-execution subscription-data) stacks-block-height)
      ),
      is-ready: (>= stacks-block-height (get next-execution subscription-data))
    })
    ERR_SUBSCRIPTION_NOT_FOUND
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