(define-trait sbtc-token-trait
  (
    ;; 允许调用协议铸造和销毁
    (protocol-mint (uint principal) (response bool uint))
    (protocol-burn (uint principal) (response bool uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; 🚫 Hiro Web端不支持 (impl-trait .sbtc-token-trait)
;; 直接注释掉或删除，后续用 contract-call? 时注意目标合约名字
;; (impl-trait .sbtc-token-trait)

(define-constant collateral-ratio-threshold u150) ;; 150% 抵押率
(define-constant btc-loan-rate u66) ;; 可借 66% 抵押品价值

(define-map user-collaterals {user: principal} {amount: uint})
(define-map user-loans {user: principal} {amount: uint})

;; 用户抵押 sBTC
(define-public (lock-collateral (amount uint))
  (begin
    (try! (contract-call? .sbtc-token transfer amount tx-sender (as-contract tx-sender)))
    (map-set user-collaterals {user: tx-sender} {amount: amount})
    (ok amount)
  )
)

;; 用户借 BTC
(define-public (borrow-btc)
  (let 
    (
      (collateral (default-to u0 (get amount (map-get? user-collaterals {user: tx-sender}))))
      (loan-amount (/ (* collateral btc-loan-rate) u100))
    )
    (begin
      (map-set user-loans {user: tx-sender} {amount: loan-amount})
      (ok loan-amount)
    )
  )
)

;; 用户还款
(define-public (repay-loan (amount uint))
  (let 
    (
      (loan (default-to u0 (get amount (map-get? user-loans {user: tx-sender}))))
    )
    (if (<= amount loan)
      (begin
        (map-set user-loans {user: tx-sender} {amount: (- loan amount)})
        (ok true)
      )
      (err u400) ;; 还款金额超出
    )
  )
)

;; 释放抵押品
(define-public (release-collateral)
  (let 
    (
      (loan (default-to u0 (get amount (map-get? user-loans {user: tx-sender}))))
      (collateral (default-to u0 (get amount (map-get? user-collaterals {user: tx-sender}))))
    )
    (if (= loan u0)
      (begin
        (try! (contract-call? .sbtc-token transfer collateral (as-contract tx-sender) tx-sender))
        (map-delete user-collaterals {user: tx-sender})
        (ok collateral)
      )
      (err u401) ;; 未还清贷款
    )
  )
)

;; 清算逻辑
(define-public (liquidate (user principal))
  (let 
    (
      (collateral (default-to u0 (get amount (map-get? user-collaterals {user: user}))))
      (loan (default-to u0 (get amount (map-get? user-loans {user: user}))))
      (collateral-ratio (if (> loan u0) (/ (* collateral u100) loan) u0))
    )
    (if (< collateral-ratio collateral-ratio-threshold)
      (begin
        (map-delete user-collaterals {user: user})
        (map-delete user-loans {user: user})
        (ok collateral)
      )
      (err u402) ;; 抵押率正常，不能清算
    )
  )
)
