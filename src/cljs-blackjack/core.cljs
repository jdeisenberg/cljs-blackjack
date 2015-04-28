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
                     :feedback-color :black}))

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
  "Given player hand and dealer hand, return true if someone
  has blackjack, false otherwise."
  [dealer-hand player-hand]
  (let [[ptotal _] (evaluate-hand player-hand)
        [dtotal _] (evaluate-hand dealer-hand)]
    (or (= ptotal 21) (= dtotal 21))))

(defn reveal
  "This function takes a player's hand and returns a new hand
  with all the cards in the :up position"
  [hand]
  (let [result (vec (map (fn [card] [(first card) :up]) hand))]
    result))

(defn update-money
  "Given the totals and status for dealer and player,
  update the player's money. Make sure current bet is
  between 0 and the current amount of money. This function
  is awfully imperative. (Sorry.)"
  [dtotal dstatus ptotal pstatus]
  (let [{:keys [player-money current-bet]} @game
        new-money (cond
                    (= ptotal dtotal) player-money ;; least common, but avoids lots of ugliness later
                    (= pstatus :blackjack) (+ player-money (* 1.5 current-bet))
                    (= pstatus :bust) (- player-money current-bet)
                    (or (= dstatus :bust) (> ptotal dtotal)) (+ player-money current-bet)
                    (< ptotal dtotal)(- player-money current-bet)
                    :else player-money)]
    (swap! game assoc :player-money new-money :current-bet (max 0 (min current-bet new-money)))))

(defn feedback
  "Given feedback for the player; update the reactive field."
  [message color]
  (swap! game assoc :player-feedback (str "\u00a0" message) :feedback-color (name color)))

(defn end-game
  "Evaluate the dealer's and player's hands when the
  game has ended."
  [dealer player]
  (let [[ptotal pstatus] (evaluate-hand player)
        [dtotal dstatus] (evaluate-hand dealer)]
;;    (println "Player:" player ptotal pstatus)
;;    (println "Dealer:" dealer dtotal dstatus)
    (cond
      (> ptotal 21) (feedback "Sorry, you busted." :red)
      (> dtotal 21) (feedback "Dealer goes bust. You win!" :green)
      (= ptotal dtotal) (feedback "Tie game." :black)
      (= pstatus :blackjack) (feedback "You win with blackjack!" :green)
      (= dstatus :blackjack) (feedback "Dealer has blackjack." :red)
      (< ptotal dtotal) (feedback "Dealer wins." :red)
      (> ptotal dtotal) (feedback "You win!" :green)
      :else (feedback "Unknown result (Shouldn't happen.)" :gray))
    (update-money dtotal dstatus ptotal pstatus)
    (swap! game assoc :playing false)))

(defn start-game
  "Deal two cards to the player (both face up), and two to the dealer (one
  face down and one face up). Update the game atom, and check for an immediate
  win."
  [event]
  (let [{:keys [deck discard-pile dealer-hand player-hand current-bet]} @game
        [player1 pile0] (discard [player-hand discard-pile])
        [dealer1 pile1] (discard [dealer-hand pile0])
        [deck2 dealer2 pile2] (deal (deal [deck dealer1 pile1] :up) :down)
        [deck3 player2 pile3] (deal (deal [deck2 player1 pile2] :up) :up)]
    (swap! game assoc :playing true :discard-pile pile3 :player-hand player2
           :dealer-hand dealer2 :deck deck3 :player-feedback "" :feedback-color :black)
    (if (immediate-win dealer2 player2)
      (do
        (swap! game assoc :dealer-hand (reveal dealer2))
        (end-game dealer2 player2)))))

(defn hit-me
  "Deal a card face up to the player, and evaluate the hand.
  If the player went bust, end the game."
  [event]
  (let [{:keys [deck discard-pile dealer-hand player-hand]} @game
        [deck2 player2 discard2] (deal [deck player-hand discard-pile] :up)
        [total status] (evaluate-hand player2)]
    (swap! game assoc :player-hand player2 :deck deck2 :discard-pile discard2)
    (if (= status :bust) (end-game dealer-hand player2))))

(defn stand
  "Player is satisfied with hand. Reveal the dealer's hand,
  then deal cards one at a time until the dealer has to stand
  or goes bust."
  [event]
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

(defn change-bet
  "Allow user to change amount of bet, which must be between zero and
  current amount of money."
  [event]
  (let [val (.parseFloat js/window (.-value (.-target event)))
        amount (if (js/isNaN val) 0 val)
        total (:player-money @game)]
    (swap! game assoc :current-bet (max 0 (min amount total)))))

(defn recharge-money
  "When player's money goes to zero, allow them to recharge
  to $1000 with default bet of $10."
  [event]
  (swap! game assoc :current-bet 10 :player-money 1000))

(def cardnames ["Ace", "2", "3", "4", "5", "6", "7", "8", "9" "10" "Jack" "Queen" "King"])
(def suits ["clubs" "diamonds" "hearts" "spades"])

(defn english
  "Given a card value and position (:up or :down),
  return a string giving the card name."
  [cval pos]
  (if (= pos :up)
    (str (nth cardnames (rem cval 13)) " of "
         (nth suits (quot cval 13)))
    "face down card"))

(defn card-image
  "Display card number N from the given hand. Assign the
  alt and title attributes of the image, and use relative
  positioning to overlap the cards."
  [n [card pos]]
  (let [filename (if (= pos :up) card "blue_grid_back")]
    [:img {:src (str "./images/" filename ".svg")
           :alt (english card pos)
           :title (english card pos)
           :style {:position "relative"
                   :left (* -55 n)
                   :height "133px"
                   :width "98px"}}]))

(defn show-cards
  "Show all the cards in a hand."
  [hand]
  (into [] (concat [:div {:class "cards"}]
                   (map-indexed card-image hand))))

(defn tableau
  "Display the playing area (the tableau)."
  []
  (let [{:keys [dealer-hand player-hand playing player-feedback feedback-color
                player-money current-bet]} @game]
  [:div
   [:h2 "Dealer’s Cards"]
   [show-cards dealer-hand]
   [:hr]
   [:h2 "Your Cards" [:span {:style {:color feedback-color}} player-feedback]]
   [show-cards player-hand]
   [:p
    [:input {:type "button"
             :value "Start Game"
             :style {:display (if playing "none" "inline")}
             :on-click start-game}]
    [:input {:type "button"
             :value "Hit"
             :on-click hit-me
             :style {:display (if playing "inline" "none")}}]
    [:input {:type "button"
             :value "Stand"
             :on-click stand
             :style {:display (if playing "inline" "none")}}]
     " Your bet: $"
     [:input {:type "text"
              :size "5"
              :on-change change-bet
              :value current-bet}]
     " Current money: $" player-money
    "\u00a0 \u00a0" ;; spacing
    [:input {:type "button"
             :value "Recharge money"
             :on-click recharge-money
             :style {:display (if (= player-money 0) "inline" "none")}}]]]))

;; Shuffle the deck initially and display the tableau.

(swap! game assoc :deck (into [] (shuffle (range 0 52))))
(reagent/render-component [tableau] (.getElementById js/document "tableau"))
