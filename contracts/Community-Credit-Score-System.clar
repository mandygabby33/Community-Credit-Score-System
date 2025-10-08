(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_LOAN_NOT_FOUND (err u103))
(define-constant ERR_LOAN_ALREADY_REPAID (err u104))
(define-constant ERR_INVALID_ENDORSEMENT (err u105))
(define-constant ERR_SELF_ENDORSEMENT (err u106))
(define-constant ERR_ALREADY_ENDORSED (err u107))

(define-constant ERR_INSURANCE_NOT_FOUND (err u112))
(define-constant ERR_INSURANCE_EXPIRED (err u113))
(define-constant ERR_INSUFFICIENT_PREMIUM (err u114))
(define-constant ERR_CLAIM_NOT_ELIGIBLE (err u115))
(define-constant ERR_POLICY_ACTIVE (err u116))

(define-constant ERR_GUARANTEE_NOT_FOUND (err u108))
(define-constant ERR_GUARANTEE_ALREADY_EXISTS (err u109))
(define-constant ERR_INSUFFICIENT_GUARANTEE_SCORE (err u110))
(define-constant ERR_GUARANTEE_LIMIT_EXCEEDED (err u111))

(define-constant ERR_FREEZE_NOT_FOUND (err u117))
(define-constant ERR_FREEZE_ALREADY_ACTIVE (err u118))
(define-constant ERR_INSUFFICIENT_FREEZE_FEE (err u119))
(define-constant ERR_FREEZE_EXPIRED (err u120))

(define-constant ERR_MILESTONE_ALREADY_CLAIMED (err u121))
(define-constant ERR_MILESTONE_NOT_ACHIEVED (err u122))
(define-constant ERR_INSUFFICIENT_REWARD_POOL (err u123))

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

(define-map insurance-policies principal {
    coverage-amount: uint,
    premium-paid: uint,
    protected-score: uint,
    expiry-block: uint,
    active: bool,
    claims-used: uint,
    max-claims: uint,
    purchased-at: uint
})

(define-map insurance-claims principal {
    last-claim-block: uint,
    total-claims: uint,
    total-payout: uint
})

(define-data-var insurance-pool uint u0)
(define-data-var total-premiums uint u0)

(define-map freeze-protections principal {
    frozen-score: uint,
    expiry-block: uint,
    fee-paid: uint,
    active: bool,
    purchased-at: uint
})

(define-data-var freeze-revenue uint u0)

(define-read-only (get-insurance-policy (user principal))
    (map-get? insurance-policies user)
)

(define-read-only (get-claim-history (user principal))
    (default-to 
        {
            last-claim-block: u0,
            total-claims: u0,
            total-payout: u0
        }
        (map-get? insurance-claims user)
    )
)

(define-read-only (calculate-premium (coverage-amount uint) (duration-blocks uint))
    (let (
        (base-rate u5)
        (duration-factor (/ duration-blocks u1000))
        (coverage-factor (/ coverage-amount u10000))
    )
    (+ (* base-rate duration-factor) coverage-factor)
    )
)

(define-public (purchase-insurance (coverage-amount uint) (duration-blocks uint) (protected-score uint))
    (let (
        (premium (calculate-premium coverage-amount duration-blocks))
        (current-score (calculate-credit-score tx-sender))
        (existing-policy (get-insurance-policy tx-sender))
    )
    (asserts! (> coverage-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-score u500) ERR_UNAUTHORIZED)
    (asserts! (>= protected-score u400) ERR_INVALID_AMOUNT)
    (asserts! (is-none existing-policy) ERR_POLICY_ACTIVE)
    (asserts! (>= (get-user-balance tx-sender) premium) ERR_INSUFFICIENT_PREMIUM)
    (map-set user-balances tx-sender (- (get-user-balance tx-sender) premium))
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    (var-set total-premiums (+ (var-get total-premiums) premium))
    (map-set insurance-policies tx-sender {
        coverage-amount: coverage-amount,
        premium-paid: premium,
        protected-score: protected-score,
        expiry-block: (+ stacks-block-height duration-blocks),
        active: true,
        claims-used: u0,
        max-claims: u3,
        purchased-at: stacks-block-height
    })
    (ok premium)
    )
)

(define-public (claim-insurance)
    (let (
        (policy (unwrap! (get-insurance-policy tx-sender) ERR_INSURANCE_NOT_FOUND))
        (current-score (calculate-credit-score tx-sender))
        (claim-history (get-claim-history tx-sender))
        (score-drop (- (get protected-score policy) current-score))
        (payout (if (< (get coverage-amount policy) (* score-drop u1000))
                    (get coverage-amount policy)
                    (* score-drop u1000)))
    )
    (asserts! (get active policy) ERR_INSURANCE_EXPIRED)
    (asserts! (< stacks-block-height (get expiry-block policy)) ERR_INSURANCE_EXPIRED)
    (asserts! (< current-score (get protected-score policy)) ERR_CLAIM_NOT_ELIGIBLE)
    (asserts! (< (get claims-used policy) (get max-claims policy)) ERR_CLAIM_NOT_ELIGIBLE)
    (asserts! (>= (var-get insurance-pool) payout) ERR_INSUFFICIENT_BALANCE)
    (map-set user-balances tx-sender (+ (get-user-balance tx-sender) payout))
    (var-set insurance-pool (- (var-get insurance-pool) payout))
    (map-set insurance-policies tx-sender (merge policy {
        claims-used: (+ (get claims-used policy) u1)
    }))
    (map-set insurance-claims tx-sender (merge claim-history {
        last-claim-block: stacks-block-height,
        total-claims: (+ (get total-claims claim-history) u1),
        total-payout: (+ (get total-payout claim-history) payout)
    }))
    (ok payout)
    )
)

(define-read-only (get-freeze-protection (user principal))
    (map-get? freeze-protections user)
)

(define-read-only (calculate-freeze-fee (duration-blocks uint) (score-value uint))
    (let (
        (base-fee u1000)
        (duration-factor (/ duration-blocks u100))
        (score-factor (/ score-value u100))
    )
    (+ base-fee (* duration-factor score-factor))
    )
)

(define-read-only (is-freeze-active (user principal))
    (match (get-freeze-protection user)
        freeze-data (and 
            (get active freeze-data)
            (< stacks-block-height (get expiry-block freeze-data))
        )
        false
    )
)

(define-public (purchase-freeze-protection (duration-blocks uint))
    (let (
        (current-score (calculate-credit-score tx-sender))
        (freeze-fee (calculate-freeze-fee duration-blocks current-score))
        (user-balance (get-user-balance tx-sender))
        (existing-freeze (get-freeze-protection tx-sender))
    )
    (asserts! (> duration-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-score u500) ERR_UNAUTHORIZED)
    (asserts! (is-none existing-freeze) ERR_FREEZE_ALREADY_ACTIVE)
    (asserts! (>= user-balance freeze-fee) ERR_INSUFFICIENT_FREEZE_FEE)
    (map-set user-balances tx-sender (- user-balance freeze-fee))
    (var-set freeze-revenue (+ (var-get freeze-revenue) freeze-fee))
    (map-set freeze-protections tx-sender {
        frozen-score: current-score,
        expiry-block: (+ stacks-block-height duration-blocks),
        fee-paid: freeze-fee,
        active: true,
        purchased-at: stacks-block-height
    })
    (ok current-score)
    )
)

(define-public (deactivate-freeze-protection)
    (let (
        (freeze-data (unwrap! (get-freeze-protection tx-sender) ERR_FREEZE_NOT_FOUND))
    )
    (asserts! (get active freeze-data) ERR_FREEZE_EXPIRED)
    (map-set freeze-protections tx-sender (merge freeze-data {active: false}))
    (ok true)
    )
)

(define-read-only (get-protected-score (user principal))
    (let (
        (current-score (calculate-credit-score user))
        (freeze-protection (get-freeze-protection user))
    )
    (match freeze-protection
        freeze-data (if (and 
            (get active freeze-data)
            (< stacks-block-height (get expiry-block freeze-data))
            (> (get frozen-score freeze-data) current-score)
        )
            (get frozen-score freeze-data)
            current-score
        )
        current-score
    )
    )
)

(define-map milestone-achievements principal {
    milestone-600-claimed: bool,
    milestone-700-claimed: bool,
    milestone-800-claimed: bool,
    milestone-900-claimed: bool,
    total-rewards-earned: uint,
    last-claim-block: uint
})

(define-data-var reward-pool-balance uint u0)
(define-data-var total-rewards-distributed uint u0)

(define-read-only (get-milestone-achievements (user principal))
    (default-to 
        {
            milestone-600-claimed: false,
            milestone-700-claimed: false,
            milestone-800-claimed: false,
            milestone-900-claimed: false,
            total-rewards-earned: u0,
            last-claim-block: u0
        }
        (map-get? milestone-achievements user)
    )
)

(define-read-only (get-reward-pool-stats)
    (ok {
        pool-balance: (var-get reward-pool-balance),
        total-distributed: (var-get total-rewards-distributed)
    })
)

(define-read-only (calculate-milestone-reward (milestone-tier uint))
    (if (is-eq milestone-tier u600)
        u50000
        (if (is-eq milestone-tier u700)
            u100000
            (if (is-eq milestone-tier u800)
                u200000
                (if (is-eq milestone-tier u900)
                    u500000
                    u0
                )
            )
        )
    )
)

(define-public (claim-milestone-reward (milestone-tier uint))
    (let (
        (current-score (calculate-credit-score tx-sender))
        (achievements (get-milestone-achievements tx-sender))
        (reward-amount (calculate-milestone-reward milestone-tier))
    )
    (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-score milestone-tier) ERR_MILESTONE_NOT_ACHIEVED)
    (asserts! (>= (var-get reward-pool-balance) reward-amount) ERR_INSUFFICIENT_REWARD_POOL)
    (if (is-eq milestone-tier u600)
        (asserts! (not (get milestone-600-claimed achievements)) ERR_MILESTONE_ALREADY_CLAIMED)
        (if (is-eq milestone-tier u700)
            (asserts! (not (get milestone-700-claimed achievements)) ERR_MILESTONE_ALREADY_CLAIMED)
            (if (is-eq milestone-tier u800)
                (asserts! (not (get milestone-800-claimed achievements)) ERR_MILESTONE_ALREADY_CLAIMED)
                (asserts! (not (get milestone-900-claimed achievements)) ERR_MILESTONE_ALREADY_CLAIMED)
            )
        )
    )
    (map-set user-balances tx-sender (+ (get-user-balance tx-sender) reward-amount))
    (var-set reward-pool-balance (- (var-get reward-pool-balance) reward-amount))
    (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward-amount))
    (map-set milestone-achievements tx-sender {
        milestone-600-claimed: (or (get milestone-600-claimed achievements) (is-eq milestone-tier u600)),
        milestone-700-claimed: (or (get milestone-700-claimed achievements) (is-eq milestone-tier u700)),
        milestone-800-claimed: (or (get milestone-800-claimed achievements) (is-eq milestone-tier u800)),
        milestone-900-claimed: (or (get milestone-900-claimed achievements) (is-eq milestone-tier u900)),
        total-rewards-earned: (+ (get total-rewards-earned achievements) reward-amount),
        last-claim-block: stacks-block-height
    })
    (ok reward-amount)
    )
)

(define-public (contribute-to-reward-pool (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set reward-pool-balance (+ (var-get reward-pool-balance) amount))
        (ok amount)
    )
)