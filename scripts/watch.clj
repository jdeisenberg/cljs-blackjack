(require '[cljs.closure :as cljsc])

(cljsc/watch "src"
  {:main 'cljs-blackjack.core
   :output-to "out/cljs_blackjack.js"
   :output-dir "out"
   :optimizations :none
   :cache-analysis true
   :source-map true})
