(ns cljs-blackjack.core
  (:require [clojure.browser.repl :as repl]
            [reagent.core :as reagent :refer [atom]]))

;(defonce conn
;         (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(defonce game (atom {:deck (into [] (shuffle (range 0 52)))
                     :discard-pile []
                     :dealer-hand []
                     :player-hand []
                     :player-money 1000.0
                     :current-bet 10
                     :playing false
                     :player-feedback ""
                     :dealer-feedback ""}))

(def cardnames ["ace", "2", "3", "4", "5", "6", "7", "8", "9" "10" "J" "Q" "K"])
(def suits [" of clubs" " of diamonds" " of hearts" " of spades"])

(defn deal
  "Deal one card from the deck to a hand in the given
  position (face up or face down), possibly using the
  discard pile. Return a vector of
  the remaining deck, new hand, and discard pile"
  [[deck hand discard-pile] position]
  (let [new-deck (if (empty? deck) (shuffle discard-pile) deck)
        new-discard (if (empty? deck) [] discard-pile)
        card [(first new-deck) position]]
      [(rest new-deck) (conj hand card) new-discard]))

(defn discard
  "Discard contents of a hand onto a pile; just the card number,
  not the up/down position. Return an empty hand and the new
  discard pile."
  [[hand pile]]
  [ [] (vec (reduce (fn [acc x] (conj acc (first x))) pile hand))])

(defn accumulate-value
  "Helper function to total a hand. The accumulator is
  a vector giving the current total of the hand and the
  current value of an ace (1 or 11)"
  [[acc ace-value] card]
  (let [card-mod (inc (mod (first card) 13))
        card-value (if (= card-mod 1)
                     ace-value
                     (min 10 card-mod))]
    [(+ acc card-value) (if (= card-mod 1) 1 ace-value)]))

(defn english [[cval pos]]
  (if (= pos :up)
    (str (nth cardnames (rem cval 13))
         (nth suits (quot cval 13)))
    "face down card"))

(defn english-hand [hand]
    (reduce (fn [acc card] (conj acc (english card))) '[] hand))

(defn evaluate-hand
  "Get total value of hand. Return a vector with total and
  the status (:ok, :blackjack, :bust)"
  [hand]
  (let [[pre-total ace-value] (reduce accumulate-value [0 11] hand)
        total (if (and (> pre-total 21) (= ace-value 1)) (- pre-total 10) pre-total)]
    (vec [total (cond
                  (and (= total 21) (= (count hand) 2)) :blackjack
                  (<= total 21) :ok
                  :else :bust)])))

(defn immediate-win
  "Given player hand and dealer hand, return
  :none (nobody wins instantly)
  :tie (both have blackjack)
  :dealer-blackjack
  :player-blackjack"
  [player-hand dealer-hand]
  (let [[ptotal _] (evaluate-hand player-hand)
        [dtotal _] (evaluate-hand dealer-hand)]
    (or (= ptotal 21) (= dtotal 21))))

(defn reveal
  "This function takes a player's hand and returns a new hand
  with all the cards in the :up position"
  [hand]
  (let [result (vec (map (fn [card] [(first card) :up]) hand))]
    result))

(defn feedback
  [player dealer]
  (swap! game assoc :player-feedback player :dealer-feedback dealer))

(defn update-money [ptotal pstatus dtotal dstatus]
  (let [{:keys [player-money current-bet]} @game
        new-money (cond
                    (= ptotal dtotal) player-money ;; least common, but avoids lots of ugliness later
                    (= pstatus :blackjack) (+ player-money (* 1.5 current-bet))
                    (= pstatus :bust) (- player-money current-bet)
                    (or (= dstatus :bust) (> ptotal dtotal)) (+ player-money current-bet)
                    (< ptotal dtotal)(- player-money current-bet)
                    :else player-money)]
    (swap! game assoc :player-money new-money :current-bet (max 0 (min current-bet new-money)))))

(defn end-game [dealer player]
  (let [[ptotal pstatus] (evaluate-hand player)
        [dtotal dstatus] (evaluate-hand dealer)]
;;    (println "Player:" player ptotal pstatus)
;;    (println "Dealer:" dealer dtotal dstatus)
    (cond
      (> ptotal 21) (feedback "Sorry, you busted." "Dealer wins.")
      (> dtotal 21) (feedback "You win!" "Dealer goes bust.")
      (= ptotal dtotal) (feedback "Tie." "")
      (= pstatus :blackjack) (feedback "You win with blackjack!" "")
      (= dstatus :blackjack) (feedback "" "Dealer has blackjack.")
      (< ptotal dtotal) (feedback "" "Dealer wins.")
      (> ptotal dtotal) (feedback "You win!" ""))
    (update-money ptotal pstatus dtotal dstatus)
    (swap! game assoc :playing false)))

(defn start-game [event]
  (let [{:keys [deck discard-pile dealer-hand player-hand current-bet]} @game
        [player1 disc0] (discard [player-hand discard-pile])
        [dealer1 disc1] (discard [dealer-hand disc0])]
    (let [[deck2 dealer2 disc2] (deal (deal [deck dealer1 disc1] :down) :up)
          [deck3 player2 disc3] (deal (deal [deck2 player1 disc2] :up) :up)]
      (swap! game assoc :playing true :discard-pile disc3 :player-hand player2
             :dealer-hand dealer2 :deck deck3 :dealer-feedback "" :player-feedback "")
      (if (immediate-win player2 dealer2)
        (do
          (swap! game assoc :dealer-hand (reveal dealer2))
          (end-game dealer2 player2))))))

(defn hit-me [event]
  (let [{:keys [deck discard-pile dealer-hand player-hand]} @game
        [deck2 player2 discard2] (deal [deck player-hand discard-pile] :up)
        [total status] (evaluate-hand player2)]
    (swap! game assoc :player-hand player2 :deck deck2 :discard-pile discard2)
    (if (= status :bust) (end-game dealer-hand player2))))

(defn stand [event]
  (let [{:keys [deck dealer-hand player-hand discard-pile]} @game
        dhand (reveal dealer-hand)]
    (swap! game assoc :dealer-hand dhand)
    (loop [loop-deck deck
           loop-hand dhand
           loop-pile discard-pile]
      (let [[total status] (evaluate-hand loop-hand)]
        (if (or (= status :bust) (>= total 17))
          (do
            (swap! game assoc :dealer-hand loop-hand)
            (end-game loop-hand player-hand))
          (let [[new-deck new-hand new-discard] (deal [loop-deck loop-hand loop-pile] :up)]
            (swap! game assoc :dealer-hand new-hand)
            (recur new-deck new-hand new-discard)))))))

(defn change-bet [event]
  (let [val (.parseFloat js/window (.-value (.-target event)))
        amount (if (js/isNaN val) 0 val)
        total (:player-money @game)]
    (swap! game assoc :current-bet (max 0 (min amount total)))))

(defn recharge-money [event]
  (swap! game assoc :current-bet 10 :player-money 1000))

(defn card-image [whose n]
  (let [hand (@game whose)
        [card pos :as wholecard] (if (< n (count hand))
                                   (nth hand n)
                                   [-1 :up])
        filename (if (< card 0) "outline" (if (= pos :up) card "blue_grid_back"))]
    [:img {:src (str "./images/" filename ".svg")
           :alt (english wholecard)
           :title (english wholecard)
           :style {:position "relative"
                   :left (* -55 n)
                   :height "133px"
                   :width "98px"}}]))

(defn player-cards [whose-hand]
  (into [] (concat [:div {:class "cards"}]
                   (map (fn [x] (card-image whose-hand x)) (range 0 (count (@game whose-hand)))))))

(defn tableau []
  [:div
   [:h2 "Dealer’s Cards " [:span {:class "feedback"} (:dealer-feedback @game)]]
   [player-cards :dealer-hand]
   [:hr]
   [:h2 "Your Cards " [:span {:class "feedback"} (:player-feedback @game)]]
   [player-cards :player-hand]
   [:p
    [:input {:type "button"
             :value "Start Game"
             :style {:display (if (:playing @game) "none" "inline")}
             :on-click start-game}]
    [:input {:type "button"
             :value "Hit"
             :on-click hit-me
             :style {:display (if (:playing @game) "inline" "none")}}]
    [:input {:type "button"
             :value "Stand"
             :on-click stand
             :style {:display (if (:playing @game) "inline" "none")}}]
     " Your bet: $"
     [:input {:type "text"
              :size "5"
              :on-change change-bet
              :value (:current-bet @game)}]
     " Current money: $" (:player-money @game)
    "\u00a0"
    [:input {:type "button"
             :value "Recharge money"
             :on-click recharge-money
             :style {:display (if (= (:player-money @game) 0) "inline" "none")}}]]])

(swap! game assoc :deck (into [] (shuffle (range 0 52))))
(reagent/render-component [tableau] (.getElementById js/document "tableau"))
