;; Decentralized Commodity Trading Platform
;; A smart contract for peer-to-peer commodity trading with escrow functionality

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_TRADE (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_TRADE_NOT_FOUND (err u103))
(define-constant ERR_TRADE_ALREADY_ACCEPTED (err u104))
(define-constant ERR_TRADE_NOT_ACTIVE (err u105))
(define-constant ERR_INVALID_COMMODITY (err u106))
(define-constant ERR_INVALID_QUANTITY (err u107))
(define-constant ERR_TRADE_EXPIRED (err u108))
(define-constant ERR_CANNOT_TRADE_WITH_SELF (err u109))
(define-constant ERR_INVALID_PRICE (err u110))

;; Platform fee (0.5% = 50 basis points)
(define-constant PLATFORM_FEE_BP u50)
(define-constant BASIS_POINTS u10000)

;; Trade status constants
(define-constant STATUS_PENDING u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_COMPLETED u2)
(define-constant STATUS_CANCELLED u3)
(define-constant STATUS_DISPUTED u4)

;; Data Variables
(define-data-var trade-counter uint u0)
(define-data-var platform-treasury uint u0)

;; Data Maps
(define-map trades uint {
    seller: principal,
    buyer: (optional principal),
    commodity: (string-ascii 50),
    quantity: uint,
    price-per-unit: uint,
    total-value: uint,
    status: uint,
    created-at: uint,
    expires-at: uint,
    escrow-amount: uint
})

(define-map user-balances principal uint)
(define-map commodity-inventory {user: principal, commodity: (string-ascii 50)} uint)
(define-map user-ratings principal {total-rating: uint, trade-count: uint})

;; Helper Functions
(define-private (get-user-balance (user principal))
    (default-to u0 (map-get? user-balances user))
)

(define-private (set-user-balance (user principal) (balance uint))
    (map-set user-balances user balance)
)

(define-private (get-commodity-inventory (user principal) (commodity (string-ascii 50)))
    (default-to u0 (map-get? commodity-inventory {user: user, commodity: commodity}))
)

(define-private (set-commodity-inventory (user principal) (commodity (string-ascii 50)) (quantity uint))
    (map-set commodity-inventory {user: user, commodity: commodity} quantity)
)

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount PLATFORM_FEE_BP) BASIS_POINTS)
)

(define-private (is-valid-commodity (commodity (string-ascii 50)))
    (and (> (len commodity) u0) (<= (len commodity) u50))
)

(define-private (is-trade-expired (expires-at uint))
    (> block-height expires-at)
)

;; Public Functions

;; Deposit STX to user balance
(define-public (deposit (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_PRICE)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (set-user-balance tx-sender (+ (get-user-balance tx-sender) amount))
        (ok amount)
    )
)

;; Withdraw STX from user balance
(define-public (withdraw (amount uint))
    (let ((current-balance (get-user-balance tx-sender)))
        (asserts! (>= current-balance amount) ERR_INSUFFICIENT_FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (set-user-balance tx-sender (- current-balance amount))
        (ok amount)
    )
)

;; Add commodity inventory
(define-public (add-inventory (commodity (string-ascii 50)) (quantity uint))
    (begin
        (asserts! (is-valid-commodity commodity) ERR_INVALID_COMMODITY)
        (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
        (let ((current-inventory (get-commodity-inventory tx-sender commodity)))
            (set-commodity-inventory tx-sender commodity (+ current-inventory quantity))
        )
        (ok quantity)
    )
)

;; Create a new trade listing
(define-public (create-trade (commodity (string-ascii 50)) (quantity uint) (price-per-unit uint) (duration uint))
    (let (
        (trade-id (+ (var-get trade-counter) u1))
        (total-value (* quantity price-per-unit))
        (expires-at (+ block-height duration))
        (current-inventory (get-commodity-inventory tx-sender commodity))
    )
        (asserts! (is-valid-commodity commodity) ERR_INVALID_COMMODITY)
        (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
        (asserts! (> price-per-unit u0) ERR_INVALID_PRICE)
        (asserts! (> duration u0) ERR_INVALID_TRADE)
        (asserts! (>= current-inventory quantity) ERR_INSUFFICIENT_FUNDS)
        
        ;; Lock the commodity in escrow
        (set-commodity-inventory tx-sender commodity (- current-inventory quantity))
        
        ;; Create the trade
        (map-set trades trade-id {
            seller: tx-sender,
            buyer: none,
            commodity: commodity,
            quantity: quantity,
            price-per-unit: price-per-unit,
            total-value: total-value,
            status: STATUS_PENDING,
            created-at: block-height,
            expires-at: expires-at,
            escrow-amount: quantity
        })
        
        (var-set trade-counter trade-id)
        (ok trade-id)
    )
)

;; Accept a trade (buyer perspective)
(define-public (accept-trade (trade-id uint))
    (let ((trade-data (unwrap! (map-get? trades trade-id) ERR_TRADE_NOT_FOUND)))
        (asserts! (is-eq (get status trade-data) STATUS_PENDING) ERR_TRADE_NOT_ACTIVE)
        (asserts! (not (is-trade-expired (get expires-at trade-data))) ERR_TRADE_EXPIRED)
        (asserts! (not (is-eq tx-sender (get seller trade-data))) ERR_CANNOT_TRADE_WITH_SELF)
        (asserts! (>= (get-user-balance tx-sender) (get total-value trade-data)) ERR_INSUFFICIENT_FUNDS)
        
        ;; Transfer payment from buyer to escrow
        (set-user-balance tx-sender (- (get-user-balance tx-sender) (get total-value trade-data)))
        
        ;; Update trade status
        (map-set trades trade-id 
            (merge trade-data {buyer: (some tx-sender), status: STATUS_ACTIVE})
        )
        
        (ok trade-id)
    )
)

;; Complete trade (releases escrow)
(define-public (complete-trade (trade-id uint))
    (let ((trade-data (unwrap! (map-get? trades trade-id) ERR_TRADE_NOT_FOUND)))
        (asserts! (is-eq (get status trade-data) STATUS_ACTIVE) ERR_TRADE_NOT_ACTIVE)
        (asserts! (or 
            (is-eq tx-sender (get seller trade-data))
            (is-eq tx-sender (unwrap! (get buyer trade-data) ERR_UNAUTHORIZED))
        ) ERR_UNAUTHORIZED)
        
        (let (
            (seller (get seller trade-data))
            (buyer (unwrap! (get buyer trade-data) ERR_UNAUTHORIZED))
            (total-value (get total-value trade-data))
            (platform-fee (calculate-platform-fee total-value))
            (seller-payment (- total-value platform-fee))
            (commodity (get commodity trade-data))
            (quantity (get quantity trade-data))
        )
            ;; Transfer commodity to buyer
            (let ((buyer-inventory (get-commodity-inventory buyer commodity)))
                (set-commodity-inventory buyer commodity (+ buyer-inventory quantity))
            )
            
            ;; Transfer payment to seller (minus platform fee)
            (set-user-balance seller (+ (get-user-balance seller) seller-payment))
            
            ;; Add platform fee to treasury
            (var-set platform-treasury (+ (var-get platform-treasury) platform-fee))
            
            ;; Update trade status
            (map-set trades trade-id (merge trade-data {status: STATUS_COMPLETED}))
            
            (ok trade-id)
        )
    )
)

;; Cancel trade (only seller can cancel pending trades)
(define-public (cancel-trade (trade-id uint))
    (let ((trade-data (unwrap! (map-get? trades trade-id) ERR_TRADE_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get seller trade-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status trade-data) STATUS_PENDING) ERR_TRADE_NOT_ACTIVE)
        
        ;; Return commodity to seller
        (let (
            (commodity (get commodity trade-data))
            (quantity (get quantity trade-data))
            (current-inventory (get-commodity-inventory tx-sender commodity))
        )
            (set-commodity-inventory tx-sender commodity (+ current-inventory quantity))
        )
        
        ;; Update trade status
        (map-set trades trade-id (merge trade-data {status: STATUS_CANCELLED}))
        
        (ok trade-id)
    )
)

;; Rate a completed trade
(define-public (rate-trade (trade-id uint) (rating uint) (target-user principal))
    (let ((trade-data (unwrap! (map-get? trades trade-id) ERR_TRADE_NOT_FOUND)))
        (asserts! (is-eq (get status trade-data) STATUS_COMPLETED) ERR_TRADE_NOT_ACTIVE)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_TRADE)
        (asserts! (or 
            (and (is-eq tx-sender (get seller trade-data)) (is-eq target-user (unwrap! (get buyer trade-data) ERR_UNAUTHORIZED)))
            (and (is-eq tx-sender (unwrap! (get buyer trade-data) ERR_UNAUTHORIZED)) (is-eq target-user (get seller trade-data)))
        ) ERR_UNAUTHORIZED)
        
        (let ((current-rating (default-to {total-rating: u0, trade-count: u0} (map-get? user-ratings target-user))))
            (map-set user-ratings target-user {
                total-rating: (+ (get total-rating current-rating) rating),
                trade-count: (+ (get trade-count current-rating) u1)
            })
        )
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-trade (trade-id uint))
    (map-get? trades trade-id)
)

(define-read-only (get-user-balance-read (user principal))
    (get-user-balance user)
)

(define-read-only (get-user-inventory (user principal) (commodity (string-ascii 50)))
    (get-commodity-inventory user commodity)
)

(define-read-only (get-user-rating (user principal))
    (map-get? user-ratings user)
)

(define-read-only (get-platform-treasury)
    (var-get platform-treasury)
)

(define-read-only (get-trade-count)
    (var-get trade-counter)
)

;; Admin function to withdraw platform fees (only contract owner)
(define-public (withdraw-platform-fees (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= amount (var-get platform-treasury)) ERR_INSUFFICIENT_FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
        (var-set platform-treasury (- (var-get platform-treasury) amount))
        (ok amount)
    )
)