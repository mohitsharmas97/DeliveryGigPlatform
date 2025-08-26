;; DeliveryGig Platform
;; A decentralized delivery network with fair compensation and route optimization
;; Smart contract for managing delivery gigs and driver payments

;; Define the fungible token for platform rewards

(define-fungible-token delivery-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-gig-not-found (err u102))
(define-constant err-not-authorized (err u103))
(define-constant err-gig-already-completed (err u104))
(define-constant err-insufficient-payment (err u105))

;; Data Variables
(define-data-var next-gig-id uint u1)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee

;; Data Maps
;; Gig structure: {customer: principal, driver: optional principal, amount: uint, distance: uint, status: uint, created-at: uint}
;; Status: 0 = open, 1 = assigned, 2 = completed
(define-map gigs
  { gig-id: uint }
  {
    customer: principal,
    driver: (optional principal),
    payment-amount: uint,
    distance-km: uint,
    pickup-location: (string-ascii 100),
    delivery-location: (string-ascii 100),
    status: uint,
    created-at: uint
  }
)

;; Driver profiles with ratings
(define-map driver-profiles
  { driver: principal }
  {
    total-deliveries: uint,
    total-earnings: uint,
    average-rating: uint,
    active: bool
  }
)

;; Function 1: Create Delivery Gig
(define-public (create-gig 
    (payment-amount uint) 
    (distance-km uint) 
    (pickup-location (string-ascii 100)) 
    (delivery-location (string-ascii 100)))
  (let 
    (
      (gig-id (var-get next-gig-id))
      (current-block-height stacks-block-height)
    )
    (begin
      ;; Validate inputs
      (asserts! (> payment-amount u0) err-invalid-amount)
      (asserts! (> distance-km u0) err-invalid-amount)
      (asserts! (<= (len pickup-location) u100) err-invalid-amount)
      (asserts! (<= (len delivery-location) u100) err-invalid-amount)

      ;; Transfer payment to contract escrow
      (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
      
      ;; Create the gig
      (map-set gigs
        { gig-id: gig-id }
        {
          customer: tx-sender,
          driver: none,
          payment-amount: payment-amount,
          distance-km: distance-km,
          pickup-location: pickup-location,
          delivery-location: delivery-location,
          status: u0, ;; open status
          created-at: current-block-height
        }
      )
      
      ;; Increment gig ID for next gig
      (var-set next-gig-id (+ gig-id u1))
      
      ;; Return success with gig ID
      (ok gig-id)
    )
  )
)

;; Function 2: Complete Delivery and Process Payment
(define-public (complete-delivery (gig-id uint) (customer-rating uint))
  (let 
    (
      (gig-data (unwrap! (map-get? gigs { gig-id: gig-id }) err-gig-not-found))
      (driver tx-sender)
      (payment-amount (get payment-amount gig-data))
      (platform-fee (/ (* payment-amount (var-get platform-fee-percentage)) u100))
      (driver-payment (- payment-amount platform-fee))
      (current-driver-profile (default-to 
        { total-deliveries: u0, total-earnings: u0, average-rating: u0, active: true }
        (map-get? driver-profiles { driver: driver })))
    )
    (begin
      ;; Validate gig status and authorization
      (asserts! (is-eq (get status gig-data) u1) err-gig-already-completed) ;; must be assigned
      (asserts! (is-eq (some driver) (get driver gig-data)) err-not-authorized)
      (asserts! (<= customer-rating u5) err-invalid-amount) ;; rating out of 5
      (asserts! (>= customer-rating u0) err-invalid-amount)

      ;; Update gig status to completed
      (map-set gigs
        { gig-id: gig-id }
        (merge gig-data { status: u2 })
      )
      
      ;; Transfer payment to driver (minus platform fee)
    ;; Extra validation for driver-payment and driver
    (asserts! (> driver-payment u0) err-invalid-amount)
    (asserts! (is-some (get driver gig-data)) err-not-authorized)
    (try! (as-contract (stx-transfer? driver-payment (as-contract tx-sender) driver)))
      
      ;; Update driver profile with new delivery stats
      (let 
        (
          (new-total-deliveries (+ (get total-deliveries current-driver-profile) u1))
          (new-total-earnings (+ (get total-earnings current-driver-profile) driver-payment))
          (new-average-rating (/ (+ (* (get average-rating current-driver-profile) 
                                     (get total-deliveries current-driver-profile)) 
                                  customer-rating) 
                               new-total-deliveries))
        )
        (map-set driver-profiles
          { driver: driver }
          {
            total-deliveries: new-total-deliveries,
            total-earnings: new-total-earnings,
            average-rating: new-average-rating,
            active: true
          }
        )
      )
      
      ;; Mint platform reward tokens based on distance and performance
      (let ((reward-tokens (* (get distance-km gig-data) u10))) ;; 10 tokens per km
        (try! (ft-mint? delivery-token reward-tokens driver))
      )
      
      (ok {
        gig-id: gig-id,
        driver-payment: driver-payment,
        platform-fee: platform-fee,
        reward-tokens: (* (get distance-km gig-data) u10)
      })
    )
  )
)

;; Read-only functions for data access
(define-read-only (get-gig (gig-id uint))
  (map-get? gigs { gig-id: gig-id }))

(define-read-only (get-driver-profile (driver principal))
  (map-get? driver-profiles { driver: driver }))

(define-read-only (get-total-gigs)
  (- (var-get next-gig-id) u1))

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage))

(define-read-only (get-token-balance (account principal))
  (ft-get-balance delivery-token account))
