(ns mortgage.core)


(defn pct->num [pct]
  (/ pct 100))

(defn mpr [apr]
  (/ apr 12))

(defn interest-for-month [remain-princip apr]
  (* (pct->num (mpr apr))
     remain-princip))

(defn expt [b e] (Math/pow b e))

(defn fixed-payment-size [loan-amt, apr, num-payments]
  (let [L loan-amt
        c (pct->num (mpr apr))
        n num-payments]
  (* L (/ (* c (expt (+ 1 c) n))
          (- (expt (+ 1 c) n) 1)))))

(defn balance-after-n-payments [loan-amt apr num-payments-total num-payments-made]
  (let [L loan-amt
        c (pct->num (mpr apr))
        n num-payments-total
        p num-payments-made]
    (* L (/ (- (expt (+ 1 c) n) (expt (+ 1 c) p))
            (- (expt (+ 1 c) n) 1)))))

(defn interest-on-nth-payment [loan-amt apr num-payments-total
                               num-payments-made]
  (let [balance (balance-after-n-payments
                 loan-amt apr num-payments-total num-payments-made)
        c (pct->num (mpr apr))
        interest-now (* c balance)]
    interest-now))

(defn principal-on-nth-payment [loan-amt apr num-payments-total
                               num-payments-made]
  (let [interest (interest-on-nth-payment loan-amt apr num-payments-total num-payments-made)
        payment-size (fixed-payment-size loan-amt apr num-payments-total)]
    (- payment-size interest)))
