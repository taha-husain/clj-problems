(ns win-or-lose
 (:require [clojure.string :refer [split join]]))

(def results (atom []))
(def win? true?)
(def win true)
(def lose false)

(defn remove-one
 [x coll]
 (let [[n m] (split-with (partial not= x) coll)]
  (concat n (rest m))))

(defn check-test-cases-constraint!
 [iterations]
 (when (or (> 1 iterations) (> iterations 10))
  (println "LOSE")))

(defn check-contraints!
  [villains players no-of-players]
  (when (or (not (every? #(and (<= 1 %) (<= % 100000)) villains))
            (not (every? #(and (<= 1 %) (<= % 100000)) players))
            (or (> 1 no-of-players) (> no-of-players 1000))
            (not= (count villains) no-of-players)
            (not= (count players) no-of-players))
   (println "LOSE")))

(defn player-vs-villain
 [players villains]
 (map
   (fn [villain]
    (let [player (->> players
                      (filter (fn [player] (> player villain)))
                      first)]
     (if player
      (do
       (remove-one player players)
       (remove-one villain villains)
       win)
      lose)))
   villains))


(defn play
 []
 (let [iterations (Integer/parseInt (read-line))]
  (check-test-cases-constraint! iterations)

  (dotimes [n iterations]
   (let [iteration (inc n)
         no-of-players (Integer/parseInt (read-line))
         villains (read-line)
         players (read-line)
         villains (sort (map #(Integer/parseInt %) (split villains #" ")))
         players (sort (map #(Integer/parseInt %) (split players #" ")))]

     (check-contraints! villains players no-of-players)

     (swap! results conj
       (if (every? win? (player-vs-villain players villains))
        "WIN"
        "LOSE"))))
  (println (join "\n" @results))))

(play)
