(ns clara-talk.core
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]
            [clara.tools.inspect :as inspect]
            [clojure.pprint :refer [pprint]])
  (:import [java.time
            LocalDate]))



(defrecord Customer [customer-id])

(defrecord BillingAddress [customer-id country])

(defrecord CustomerCurrency [customer-id currency])

(definterface Discount)

(defrecord BaseCaseDiscount [discount-rate]
  Discount)

(defrecord RecentPurchasesDiscount [discount-rate]
  Discount)

(defrecord Purchase [price customer-id purchase-id cart-currency])

(defrecord PreviousPurchase [purchase-time price customer-id purchase-id cart-currency])

(defrecord FinalPurchase [price customer-id purchase-id cart-currency])

(defrecord CurrentTime [datetime])

(defrecord FxPaymentOffer [purchase-id payment-currency payment-amount])

(defn within-days?
  [earlier-date later-date days]
  (try
    
    (<= (.getDays (.until earlier-date later-date)) days)
    (catch Exception e
      (throw (ex-info "error " {:e earlier-date
                        :l later-date
                        :d days} e)))))

(defn get-fx-rate
  [purchase source-currency destination-currency]
  (cond
    (and (= source-currency "EUR")
         (= destination-currency "GBP"))
    0.89

    :default
    (throw (UnsupportedOperationException.))))

(defrule historic-purchase-discount
  [CurrentTime (= ?current-time datetime)]
  [?last-week-count <- (acc/count) :from [PreviousPurchase (within-days? purchase-time ?current-time 7)]]
  [:test (>= ?last-week-count 2)]
  =>
  (insert! (map->RecentPurchasesDiscount {:discount-rate 0.9})))

(defrule insert-base-discount
  =>
  (insert! (->BaseCaseDiscount 1)))

(defrule final-price-with-max-discount
  [?best-rate <- (acc/min :discount-rate) :from [Discount]]
  [?purchase <- Purchase]
  =>
  (insert! (map->FinalPurchase (update ?purchase :price #(* % ?best-rate)))))

(def country->currency-code {"GB" "GBP"
                             "US" "USD"
                             "FR" "EUR"})

(defrule possible-customer-currencies
  [Customer (= ?customer-id customer-id)]
  [:exists [BillingAddress (= ?customer-id customer-id) (= ?country country)]]
  =>
  (insert! (map->CustomerCurrency {:currency (country->currency-code ?country)
                                   :customer-id ?customer-id})))

(defrule fx-fees-offer
  [?purchase <- FinalPurchase (= ?source-currency cart-currency) (= ?purchase-id purchase-id)]
  [CustomerCurrency (= ?destination-currency currency)]
  =>
  (let [fx-rate (get-fx-rate (:price ?purchase) ?source-currency ?destination-currency)]
    (insert! (map->FxPaymentOffer
              {:purchase-id ?purchase-id
               :payment-currency ?destination-currency
               :payment-amount (* (:price ?purchase) fx-rate)}))))

(defquery fx-for-purchase-query
  [:?purchase-id]
  [FxPaymentOffer
   (= ?currency payment-currency)
   (= ?amount payment-amount)
   (= ?purchase-id purchase-id)])

(def example-session-1
  (-> (mk-session 'clara-talk.core)
      (insert (map->Customer {:customer-id "ABC"})
              (map->BillingAddress {:customer-id "ABC" :country "GB"})
              (map->Purchase {:customer-id "ABC"
                              :purchase-id "DEF"
                              :cart-currency "EUR"
                              :price 10})
              (->CurrentTime (LocalDate/parse "2020-07-04")))
      
      fire-rules))

(def example-session-2
  (-> example-session-1
      (insert
       (map->PreviousPurchase {:purchase-time (LocalDate/parse "2020-07-01")
                               :price 10
                               :customer-id "ABC"
                               :purchase-id "HISTORY_1"
                               :cart-currency "EUR"})
       (map->PreviousPurchase {:purchase-time (LocalDate/parse "2020-07-01")
                               :price 10
                               :customer-id "ABC"
                               :purchase-id "HISTORY_1"
                               :cart-currency "EUR"}))
      fire-rules))

(def example-session-3
  (-> example-session-2
      (retract
       (map->PreviousPurchase {:purchase-time (LocalDate/parse "2020-07-01")
                               :price 10
                               :customer-id "ABC"
                               :purchase-id "HISTORY_1"
                               :cart-currency "EUR"}))
      fire-rules))


(defn query-demo
  []
  (query example-session-1 fx-for-purchase-query :?purchase-id "DEF"))

;; (defrecord PreviousPurchase [purchase-time price customer-id purchase-id cart-currency])

(defn add-discount-demo
  []
  (query example-session-2 fx-for-purchase-query :?purchase-id "DEF"))

(defn retract-previous-purchase-demo
  []
  (query example-session-3 fx-for-purchase-query :?purchase-id "DEF"))
  

(defn inspect-demo
  []
  (->> example-session-1
       inspect/inspect
       :fact->explanations
       (filter (fn [[k v]] (instance? FxPaymentOffer k)))
       pprint))



