(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_LOAN_NOT_FOUND (err u103))
(define-constant ERR_LOAN_ALREADY_REPAID (err u104))
(define-constant ERR_INVALID_ENDORSEMENT (err u105))
(define-constant ERR_SELF_ENDORSEMENT (err u106))
(define-constant ERR_ALREADY_ENDORSED (err u107))

(define-constant ERR_GUARANTEE_NOT_FOUND (err u108))
(define-constant ERR_GUARANTEE_ALREADY_EXISTS (err u109))
(define-constant ERR_INSUFFICIENT_GUARANTEE_SCORE (err u110))
(define-constant ERR_GUARANTEE_LIMIT_EXCEEDED (err u111))


(define-map user-profiles principal {
    total-borrowed: uint,
    total-repaid: uint,
    loans-count: uint,
    successful-repayments: uint,
    endorsements-received: uint,
    endorsements-given: uint,
    reputation-score: uint,
    last-activity: uint
})

(define-map loans uint {
    borrower: principal,
    lender: principal,
    amount: uint,
    interest-rate: uint,
    due-block: uint,
    repaid: bool,
    repaid-amount: uint,
    created-at: uint
})

(define-map endorsements {endorser: principal, endorsed: principal} {
    timestamp: uint,
    weight: uint
})

(define-map user-balances principal uint)

(define-data-var loan-counter uint u0)
(define-data-var total-pool uint u0)

(define-read-only (get-user-profile (user principal))
    (default-to 
        {
            total-borrowed: u0,
            total-repaid: u0,
            loans-count: u0,
            successful-repayments: u0,
            endorsements-received: u0,
            endorsements-given: u0,
            reputation-score: u500,
            last-activity: u0
        }
        (map-get? user-profiles user)
    )
)

(define-read-only (get-loan (loan-id uint))
    (map-get? loans loan-id)
)

(define-read-only (get-user-balance (user principal))
    (default-to u0 (map-get? user-balances user))
)

(define-read-only (calculate-credit-score (user principal))
    (let (
        (profile (get-user-profile user))
        (total-borrowed (get total-borrowed profile))
        (total-repaid (get total-repaid profile))
        (successful-repayments (get successful-repayments profile))
        (loans-count (get loans-count profile))
        (endorsements-count (get endorsements-received profile))
    )
    (if (is-eq loans-count u0)
        u500
        (let (
            (repayment-ratio (if (> total-borrowed u0) (/ (* total-repaid u100) total-borrowed) u100))
            (success-rate (if (> loans-count u0) (/ (* successful-repayments u100) loans-count) u0))
            (endorsement-bonus (* endorsements-count u10))
            (base-score (+ (* repayment-ratio u3) (* success-rate u4)))
        )
        (+ (+ base-score endorsement-bonus) u300)
        )
    ))
)

(define-read-only (get-endorsement (endorser principal) (endorsed principal))
    (map-get? endorsements {endorser: endorser, endorsed: endorsed})
)

(define-public (deposit (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-balances tx-sender (+ (get-user-balance tx-sender) amount))
        (var-set total-pool (+ (var-get total-pool) amount))
        (ok amount)
    )
)

(define-public (withdraw (amount uint))
    (let (
        (current-balance (get-user-balance tx-sender))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-balances tx-sender (- current-balance amount))
    (var-set total-pool (- (var-get total-pool) amount))
    (ok amount)
    )
)

(define-public (request-loan (amount uint) (interest-rate uint) (duration-blocks uint))
    (let (
        (loan-id (+ (var-get loan-counter) u1))
        (credit-score (calculate-credit-score tx-sender))
        (profile (get-user-profile tx-sender))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= credit-score u400) ERR_UNAUTHORIZED)
    (map-set loans loan-id {
        borrower: tx-sender,
        lender: CONTRACT_OWNER,
        amount: amount,
        interest-rate: interest-rate,
        due-block: (+ stacks-block-height duration-blocks),
        repaid: false,
        repaid-amount: u0,
        created-at: stacks-block-height
    })
    (map-set user-profiles tx-sender (merge profile {
        total-borrowed: (+ (get total-borrowed profile) amount),
        loans-count: (+ (get loans-count profile) u1),
        last-activity: stacks-block-height
    }))
    (var-set loan-counter loan-id)
    (ok loan-id)
    )
)

(define-public (fund-loan (loan-id uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
        (lender-balance (get-user-balance tx-sender))
        (loan-amount (get amount loan))
    )
    (asserts! (>= lender-balance loan-amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (is-eq (get lender loan) CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (as-contract (stx-transfer? loan-amount tx-sender (get borrower loan))))
    (map-set user-balances tx-sender (- lender-balance loan-amount))
    (map-set loans loan-id (merge loan {lender: tx-sender}))
    (ok true)
    )
)

(define-public (repay-loan (loan-id uint) (amount uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
        (borrower (get borrower loan))
        (lender (get lender loan))
        (loan-amount (get amount loan))
        (interest-amount (/ (* loan-amount (get interest-rate loan)) u100))
        (total-due (+ loan-amount interest-amount))
        (profile (get-user-profile borrower))
    )
    (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
    (asserts! (not (get repaid loan)) ERR_LOAN_ALREADY_REPAID)
    (asserts! (>= amount total-due) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender lender))
    (map-set loans loan-id (merge loan {
        repaid: true,
        repaid-amount: amount
    }))
    (map-set user-profiles borrower (merge profile {
        total-repaid: (+ (get total-repaid profile) amount),
        successful-repayments: (+ (get successful-repayments profile) u1),
        last-activity: stacks-block-height
    }))
    (ok true)
    )
)

(define-public (endorse-user (user principal))
    (let (
        (endorser-profile (get-user-profile tx-sender))
        (endorsed-profile (get-user-profile user))
        (endorser-score (calculate-credit-score tx-sender))
    )
    (asserts! (not (is-eq tx-sender user)) ERR_SELF_ENDORSEMENT)
    (asserts! (>= endorser-score u600) ERR_UNAUTHORIZED)
    (asserts! (is-none (get-endorsement tx-sender user)) ERR_ALREADY_ENDORSED)
    (map-set endorsements {endorser: tx-sender, endorsed: user} {
        timestamp: stacks-block-height,
        weight: (/ endorser-score u100)
    })
    (map-set user-profiles tx-sender (merge endorser-profile {
        endorsements-given: (+ (get endorsements-given endorser-profile) u1),
        last-activity: stacks-block-height
    }))
    (map-set user-profiles user (merge endorsed-profile {
        endorsements-received: (+ (get endorsements-received endorsed-profile) u1),
        last-activity: stacks-block-height
    }))
    (ok true)
    )
)

(define-public (update-reputation-score (user principal))
    (let (
        (profile (get-user-profile user))
        (new-score (calculate-credit-score user))
    )
    (map-set user-profiles user (merge profile {
        reputation-score: new-score,
        last-activity: stacks-block-height
    }))
    (ok new-score)
    )
)

(define-read-only (get-loan-status (loan-id uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
    )
    (ok {
        loan-id: loan-id,
        borrower: (get borrower loan),
        amount: (get amount loan),
        repaid: (get repaid loan),
        overdue: (and (not (get repaid loan)) (> stacks-block-height (get due-block loan)))
    })
    )
)

(define-read-only (get-user-loans (user principal))
    (ok (filter check-user-loan (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
)

(define-private (check-user-loan (loan-id uint))
    (match (get-loan loan-id)
        loan (is-eq (get borrower loan) tx-sender)
        false
    )
)

(define-map loan-guarantees uint {
    guarantor: principal,
    guaranteed-amount: uint,
    guarantee-fee: uint,
    active: bool,
    created-at: uint
})

(define-map user-guarantee-stats principal {
    total-guaranteed: uint,
    active-guarantees: uint,
    successful-guarantees: uint,
    defaulted-guarantees: uint,
    guarantee-earnings: uint
})

(define-read-only (get-guarantee (loan-id uint))
    (map-get? loan-guarantees loan-id)
)

(define-read-only (get-guarantee-stats (user principal))
    (default-to 
        {
            total-guaranteed: u0,
            active-guarantees: u0,
            successful-guarantees: u0,
            defaulted-guarantees: u0,
            guarantee-earnings: u0
        }
        (map-get? user-guarantee-stats user)
    )
)

(define-public (offer-guarantee (loan-id uint) (guarantee-fee uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
        (guarantor-score (calculate-credit-score tx-sender))
        (stats (get-guarantee-stats tx-sender))
    )
    (asserts! (>= guarantor-score u700) ERR_INSUFFICIENT_GUARANTEE_SCORE)
    (asserts! (not (is-eq tx-sender (get borrower loan))) ERR_UNAUTHORIZED)
    (asserts! (is-none (get-guarantee loan-id)) ERR_GUARANTEE_ALREADY_EXISTS)
    (asserts! (< (get active-guarantees stats) u5) ERR_GUARANTEE_LIMIT_EXCEEDED)
    (map-set loan-guarantees loan-id {
        guarantor: tx-sender,
        guaranteed-amount: (get amount loan),
        guarantee-fee: guarantee-fee,
        active: true,
        created-at: stacks-block-height
    })
    (map-set user-guarantee-stats tx-sender (merge stats {
        total-guaranteed: (+ (get total-guaranteed stats) (get amount loan)),
        active-guarantees: (+ (get active-guarantees stats) u1)
    }))
    (ok true)
    )
)

(define-public (resolve-guarantee (loan-id uint))
    (let (
        (loan (unwrap! (get-loan loan-id) ERR_LOAN_NOT_FOUND))
        (guarantee (unwrap! (get-guarantee loan-id) ERR_GUARANTEE_NOT_FOUND))
        (guarantor (get guarantor guarantee))
        (stats (get-guarantee-stats guarantor))
    )
    (asserts! (get active guarantee) ERR_UNAUTHORIZED)
    (if (get repaid loan)
        (begin
            (map-set user-guarantee-stats guarantor (merge stats {
                successful-guarantees: (+ (get successful-guarantees stats) u1),
                active-guarantees: (- (get active-guarantees stats) u1),
                guarantee-earnings: (+ (get guarantee-earnings stats) (get guarantee-fee guarantee))
            }))
            (map-set loan-guarantees loan-id (merge guarantee {active: false}))
            (ok true)
        )
        (if (> stacks-block-height (get due-block loan))
            (begin
                (map-set user-guarantee-stats guarantor (merge stats {
                    defaulted-guarantees: (+ (get defaulted-guarantees stats) u1),
                    active-guarantees: (- (get active-guarantees stats) u1)
                }))
                (map-set loan-guarantees loan-id (merge guarantee {active: false}))
                (ok true)
            )
            ERR_UNAUTHORIZED
        )
    )
    )
)