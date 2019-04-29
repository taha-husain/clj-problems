(ns win-or-lose
 (:require [clojure.string :refer [split join]]))

(def results (atom []))

(defn main
 []
 (let [iterations (read-line)
       iterations (Integer/parseInt iterations)]

  (when (or (> 1 iterations) (> iterations 10))
    (System/exit 0))

  (dotimes [n iterations]
   (let [iteration (inc n)
         no-of-players (read-line)
         villains (read-line)
         players (read-line)

         no-of-players (Integer/parseInt no-of-players)
         villains (split villains #" ")
         players (split players #" ")]


     (when (or (or (> 1 no-of-players) (> no-of-players 1000))
               (not= (count villains) no-of-players)
               (not= (count players) no-of-players))
       (System/exit 0))

     (swap! results conj
       (if (every?
             true?
             (map
              (fn [villain]
               (let [opponent (->> players
                                   (filter
                                     (fn [player]
                                       (let [player (Integer/parseInt player)
                                             villain (Integer/parseInt villain)]
                                         (when (or (or (> 1 player) (> player 100000))
                                                   (or (> 1 villain) (> villain 100000)))
                                           (System/exit 0))
                                         (> player villain))))
                                   sort
                                   first)]
                (if opponent
                 (do
                  (remove #(= opponent %) players)
                  (remove #(= villain %) villains)
                  true)
                 false)))
              villains))
        "WIN"
        "LOSE"))))
  (println (join "\n" @results))))

(main)
