;; Energy Escrow - Secure energy trading with escrow protection
;; Enables buyers to deposit funds and sellers to fulfill orders with automatic release

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_INVALID_AMOUNT (err u201))
(define-constant ERR_ESCROW_NOT_FOUND (err u202))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u203))
(define-constant ERR_INSUFFICIENT_BALANCE (err u204))
(define-constant ERR_ESCROW_COMPLETED (err u205))
(define-constant ERR_ESCROW_EXPIRED (err u206))
(define-constant ERR_DELIVERY_NOT_CONFIRMED (err u207))
(define-constant ERR_DISPUTE_PERIOD_ACTIVE (err u208))
(define-constant ERR_NOT_PARTICIPANT (err u209))
(define-constant ERR_DEVICE_NOT_FOUND (err u210))
(define-constant ERR_SELF_PURCHASE (err u211))
(define-constant ERR_INSUFFICIENT_TOKENS (err u212))

;; Escrow status constants
(define-constant STATUS_PENDING u1)
(define-constant STATUS_FULFILLED u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_DISPUTED u4)
(define-constant STATUS_CANCELLED u5)

;; Time constants
(define-constant DELIVERY_CONFIRMATION_BLOCKS u144) ;; ~24 hours
(define-constant DISPUTE_RESOLUTION_BLOCKS u1008)  ;; ~1 week
(define-constant ESCROW_EXPIRY_BLOCKS u5040)       ;; ~5 weeks

;; Contract state
(define-data-var next-escrow-id uint u1)
(define-data-var total-escrows-created uint u0)
(define-data-var total-escrow-volume uint u0)

;; Escrow orders
(define-map escrow-orders
    uint
    {
        buyer: principal,
        seller: principal,
        device-id: (string-ascii 50),
        energy-amount: uint,
        price-per-watt: uint,
        total-cost: uint,
        deposited-amount: uint,
        status: uint,
        created-at: uint,
        delivery-deadline: uint,
        fulfilled-at: uint,
        delivery-confirmed: bool,
        dispute-deadline: uint
    }
)

;; Escrow participants tracking
(define-map user-escrow-stats
    principal
    {
        total-buy-escrows: uint,
        total-sell-escrows: uint,
        completed-escrows: uint,
        disputed-escrows: uint,
        total-volume: uint
    }
)

;; Delivery confirmations
(define-map delivery-confirmations
    uint
    {
        confirmed-by: principal,
        confirmation-timestamp: uint,
        energy-delivered: uint,
        delivery-notes: (string-ascii 128)
    }
)

;; Check if device exists in main contract
(define-private (device-exists (device-id (string-ascii 50)))
    (is-some (contract-call? .Wattshare get-device-info device-id))
)

;; Get device owner from main contract
(define-private (get-device-owner (device-id (string-ascii 50)))
    (match (contract-call? .Wattshare get-device-info device-id)
        device-data (some (get owner device-data))
        none
    )
)

;; Create energy escrow order
(define-public (create-escrow-order (device-id (string-ascii 50)) (energy-amount uint) (price-per-watt uint) (delivery-deadline-blocks uint))
    (let (
        (escrow-id (var-get next-escrow-id))
        (buyer tx-sender)
        (seller (unwrap! (get-device-owner device-id) ERR_DEVICE_NOT_FOUND))
        (total-cost (* energy-amount price-per-watt))
        (current-block stacks-block-height)
        (delivery-deadline (+ current-block delivery-deadline-blocks))
        (user-stats (default-to { total-buy-escrows: u0, total-sell-escrows: u0, completed-escrows: u0, disputed-escrows: u0, total-volume: u0 } (map-get? user-escrow-stats buyer)))
    )
        ;; Validate inputs
        (asserts! (device-exists device-id) ERR_DEVICE_NOT_FOUND)
        (asserts! (not (is-eq buyer seller)) ERR_SELF_PURCHASE)
        (asserts! (> energy-amount u0) ERR_INVALID_AMOUNT)
        (asserts! (> price-per-watt u0) ERR_INVALID_AMOUNT)
        (asserts! (> delivery-deadline-blocks u0) ERR_INVALID_AMOUNT)
        (asserts! (<= delivery-deadline-blocks ESCROW_EXPIRY_BLOCKS) ERR_INVALID_AMOUNT)
        
        ;; Transfer funds to escrow
        (try! (stx-transfer? total-cost buyer (as-contract tx-sender)))
        
        ;; Create escrow order
        (map-set escrow-orders escrow-id {
            buyer: buyer,
            seller: seller,
            device-id: device-id,
            energy-amount: energy-amount,
            price-per-watt: price-per-watt,
            total-cost: total-cost,
            deposited-amount: total-cost,
            status: STATUS_PENDING,
            created-at: current-block,
            delivery-deadline: delivery-deadline,
            fulfilled-at: u0,
            delivery-confirmed: false,
            dispute-deadline: u0
        })
        
        ;; Update user stats
        (map-set user-escrow-stats buyer {
            total-buy-escrows: (+ (get total-buy-escrows user-stats) u1),
            total-sell-escrows: (get total-sell-escrows user-stats),
            completed-escrows: (get completed-escrows user-stats),
            disputed-escrows: (get disputed-escrows user-stats),
            total-volume: (+ (get total-volume user-stats) total-cost)
        })
        
        ;; Update contract stats
        (var-set next-escrow-id (+ escrow-id u1))
        (var-set total-escrows-created (+ (var-get total-escrows-created) u1))
        (var-set total-escrow-volume (+ (var-get total-escrow-volume) total-cost))
        
        (ok escrow-id)
    )
)

;; Fulfill escrow order (seller delivers energy)
(define-public (fulfill-escrow-order (escrow-id uint))
    (let (
        (escrow-data (unwrap! (map-get? escrow-orders escrow-id) ERR_ESCROW_NOT_FOUND))
        (seller tx-sender)
        (current-block stacks-block-height)
    )
        ;; Validate fulfillment
        (asserts! (is-eq seller (get seller escrow-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status escrow-data) STATUS_PENDING) ERR_ESCROW_COMPLETED)
        (asserts! (<= current-block (get delivery-deadline escrow-data)) ERR_ESCROW_EXPIRED)
        
        ;; Check if seller has enough energy available via main contract
        (let ((device-data (unwrap! (contract-call? .Wattshare get-device-info (get device-id escrow-data)) ERR_DEVICE_NOT_FOUND)))
            (asserts! (>= (get energy-available device-data) (get energy-amount escrow-data)) ERR_INSUFFICIENT_TOKENS)
        )
        
        ;; Execute energy transfer via main contract
        (try! (contract-call? .Wattshare purchase-energy (get device-id escrow-data) (get energy-amount escrow-data)))
        
        ;; Update escrow status
        (map-set escrow-orders escrow-id (merge escrow-data {
            status: STATUS_FULFILLED,
            fulfilled-at: current-block,
            dispute-deadline: (+ current-block DISPUTE_RESOLUTION_BLOCKS)
        }))
        
        (ok true)
    )
)

;; Confirm delivery (buyer confirms receipt)
(define-public (confirm-delivery (escrow-id uint) (delivery-notes (string-ascii 128)))
    (let (
        (escrow-data (unwrap! (map-get? escrow-orders escrow-id) ERR_ESCROW_NOT_FOUND))
        (buyer tx-sender)
        (current-block stacks-block-height)
        (seller-stats (default-to { total-buy-escrows: u0, total-sell-escrows: u0, completed-escrows: u0, disputed-escrows: u0, total-volume: u0 } (map-get? user-escrow-stats (get seller escrow-data))))
        (buyer-stats (default-to { total-buy-escrows: u0, total-sell-escrows: u0, completed-escrows: u0, disputed-escrows: u0, total-volume: u0 } (map-get? user-escrow-stats buyer)))
    )
        ;; Validate confirmation
        (asserts! (is-eq buyer (get buyer escrow-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status escrow-data) STATUS_FULFILLED) ERR_DELIVERY_NOT_CONFIRMED)
        
        ;; Record delivery confirmation
        (map-set delivery-confirmations escrow-id {
            confirmed-by: buyer,
            confirmation-timestamp: current-block,
            energy-delivered: (get energy-amount escrow-data),
            delivery-notes: delivery-notes
        })
        
        ;; Release funds to seller
        (try! (as-contract (stx-transfer? (get deposited-amount escrow-data) (as-contract tx-sender) (get seller escrow-data))))
        
        ;; Update escrow status
        (map-set escrow-orders escrow-id (merge escrow-data {
            status: STATUS_COMPLETED,
            delivery-confirmed: true
        }))
        
        ;; Update user stats
        (map-set user-escrow-stats (get seller escrow-data) {
            total-buy-escrows: (get total-buy-escrows seller-stats),
            total-sell-escrows: (+ (get total-sell-escrows seller-stats) u1),
            completed-escrows: (+ (get completed-escrows seller-stats) u1),
            disputed-escrows: (get disputed-escrows seller-stats),
            total-volume: (+ (get total-volume seller-stats) (get total-cost escrow-data))
        })
        
        (map-set user-escrow-stats buyer {
            total-buy-escrows: (get total-buy-escrows buyer-stats),
            total-sell-escrows: (get total-sell-escrows buyer-stats),
            completed-escrows: (+ (get completed-escrows buyer-stats) u1),
            disputed-escrows: (get disputed-escrows buyer-stats),
            total-volume: (get total-volume buyer-stats)
        })
        
        (ok true)
    )
)

;; Cancel escrow order (before fulfillment)
(define-public (cancel-escrow-order (escrow-id uint))
    (let (
        (escrow-data (unwrap! (map-get? escrow-orders escrow-id) ERR_ESCROW_NOT_FOUND))
        (buyer (get buyer escrow-data))
    )
        ;; Only buyer can cancel before fulfillment
        (asserts! (is-eq tx-sender buyer) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status escrow-data) STATUS_PENDING) ERR_ESCROW_COMPLETED)
        
        ;; Refund buyer
        (try! (as-contract (stx-transfer? (get deposited-amount escrow-data) (as-contract tx-sender) buyer)))
        
        ;; Update escrow status
        (map-set escrow-orders escrow-id (merge escrow-data {
            status: STATUS_CANCELLED
        }))
        
        (ok (get deposited-amount escrow-data))
    )
)

;; Auto-release funds after dispute period expires
(define-public (auto-release-escrow (escrow-id uint))
    (let (
        (escrow-data (unwrap! (map-get? escrow-orders escrow-id) ERR_ESCROW_NOT_FOUND))
        (current-block stacks-block-height)
        (seller-stats (default-to { total-buy-escrows: u0, total-sell-escrows: u0, completed-escrows: u0, disputed-escrows: u0, total-volume: u0 } (map-get? user-escrow-stats (get seller escrow-data))))
    )
        ;; Validate auto-release conditions
        (asserts! (is-eq (get status escrow-data) STATUS_FULFILLED) ERR_DELIVERY_NOT_CONFIRMED)
        (asserts! (>= current-block (get dispute-deadline escrow-data)) ERR_DISPUTE_PERIOD_ACTIVE)
        
        ;; Release funds to seller
        (try! (as-contract (stx-transfer? (get deposited-amount escrow-data) (as-contract tx-sender) (get seller escrow-data))))
        
        ;; Update escrow status
        (map-set escrow-orders escrow-id (merge escrow-data {
            status: STATUS_COMPLETED
        }))
        
        ;; Update seller stats
        (map-set user-escrow-stats (get seller escrow-data) {
            total-buy-escrows: (get total-buy-escrows seller-stats),
            total-sell-escrows: (+ (get total-sell-escrows seller-stats) u1),
            completed-escrows: (+ (get completed-escrows seller-stats) u1),
            disputed-escrows: (get disputed-escrows seller-stats),
            total-volume: (+ (get total-volume seller-stats) (get total-cost escrow-data))
        })
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-escrow-order (escrow-id uint))
    (map-get? escrow-orders escrow-id)
)

(define-read-only (get-user-escrow-stats (user principal))
    (map-get? user-escrow-stats user)
)

(define-read-only (get-delivery-confirmation (escrow-id uint))
    (map-get? delivery-confirmations escrow-id)
)

(define-read-only (is-escrow-ready-for-auto-release (escrow-id uint))
    (match (map-get? escrow-orders escrow-id)
        escrow-data
        (and
            (is-eq (get status escrow-data) STATUS_FULFILLED)
            (>= stacks-block-height (get dispute-deadline escrow-data))
        )
        false
    )
)

(define-read-only (get-escrow-contract-stats)
    {
        total-escrows: (var-get total-escrows-created),
        total-volume: (var-get total-escrow-volume),
        next-escrow-id: (var-get next-escrow-id)
    }
)

(define-read-only (calculate-escrow-cost (device-id (string-ascii 50)) (energy-amount uint) (price-per-watt uint))
    (let (
        (total-cost (* energy-amount price-per-watt))
        (estimated-delivery-time DELIVERY_CONFIRMATION_BLOCKS)
    )
        (ok {
            total-cost: total-cost,
            energy-amount: energy-amount,
            price-per-watt: price-per-watt,
            estimated-delivery-blocks: estimated-delivery-time,
            dispute-period-blocks: DISPUTE_RESOLUTION_BLOCKS
        })
    )
)

(define-read-only (get-escrow-status-info (escrow-id uint))
    (match (map-get? escrow-orders escrow-id)
        escrow-data
        (let (
            (current-block stacks-block-height)
            (time-until-deadline (if (> (get delivery-deadline escrow-data) current-block)
                (- (get delivery-deadline escrow-data) current-block)
                u0))
            (time-until-dispute-end (if (> (get dispute-deadline escrow-data) current-block)
                (- (get dispute-deadline escrow-data) current-block)
                u0))
        )
            (ok {
                status: (get status escrow-data),
                delivery-confirmed: (get delivery-confirmed escrow-data),
                blocks-until-deadline: time-until-deadline,
                blocks-until-auto-release: time-until-dispute-end,
                can-auto-release: (is-escrow-ready-for-auto-release escrow-id),
                is-expired: (>= current-block (get delivery-deadline escrow-data))
            })
        )
        ERR_ESCROW_NOT_FOUND
    )
)
