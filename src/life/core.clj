;;;; Conway's game of life
;;; certainly not the best implementation, but it works...

(ns life.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener)))

(def width 50)
(def height 50)
(def point-size 10)
(def turn-millis 1000)
;(def colors 
;     [(Color. 0 255 0) ; green
;      (Color. 255 0 0) ; red
;      (Color. 0 0 255) ; blue
;      (Color. 0 255 255) ; cyan
;      (Color. 255 255 0)]) ; yellow


;(defn random-color []
;  (rand-nth colors))

(defn make-board [w h]
  "Returns a 2d vector of false"
  (vec (for [_ (range h)] 
         (vec (for [_ (range w)] (> (rand) 0.4))))))

(defn point-at [b x y]
  ((b y) x))

(defn point-to-screen-rect [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))

(defn next-generation [current pred? width height]
  (vec (for [y (range height)]
         (vec (for [x (range width)]
                (pred? current x y width height))))))

(defn count-neighbors [board x y w h]
  (let [pts (fn [xs ys]
              (apply concat (for [y ys]
                              (for [x xs :when (not (and (= x 0) (= y 0)))]
                                [x y]))))]
    (count (filter (fn [pt]
                     (let [ax (mod (+ (pt 0) x) w)
                           ay (mod (+ (pt 1) y) h)]
                       (point-at board ax ay))) 
                   (pts [-1 0 1] [-1 0 1])))))

(defn survives? [board x y w h]
  "Tests whether or not current thing survives"
  (let [status (point-at board x y)
        neighbors (count-neighbors board x y w h)]
    (if status
      (if (or (< neighbors 2) (> neighbors 3)) false true)
      (if (= neighbors 3) true false))))

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn paint-buffer [g buffer w h]
  (doseq [y (range h) x (range w)]
    (when (point-at buffer x y)
      (fill-point g [x y] (Color. 0 0 0)))))

;; (defn paint-buffer [g buffer w h]
;;  (doseq [y (range h) x (range w)]
;;    (when (point-at buffer x y)
;;      (fill-point g [x y] (random-color)))))


(defn game-panel [frame onsb paused]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (paint-buffer g @onsb width height))
    (actionPerformed [e]
                     (dosync 
                      (alter onsb next-generation survives? width height))
                     (.repaint this))
    (keyPressed [e]
                (dosync
                 (if @paused
                   (ref-set paused false)
                   (ref-set paused true))))
    (getPreferredSize []
                      (Dimension. (* width point-size)
                                  (* height point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [board (ref (make-board width height))
        paused (ref false)
        frame (JFrame. "Life")
        panel (game-panel frame board paused)
        timer (Timer. turn-millis panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    [board paused timer]))
