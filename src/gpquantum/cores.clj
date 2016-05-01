(ns gpquantum.core
  (:require [reagent.core :as reagent :refer [atom]]
            [qgame.utils.gpqgame :as qg]
            [qgame.gp.evaluator :as ev]
            [qgame.simulator.interpreter :as i]
            ))

(enable-console-print!)

(println "Edits to this text have shown up in your developer console.")
(def aoa-cases (qg/gen-and-or-and-cases))
(def or-cases {'(0 0) 0, '(0 1) 1, '(1 0) 1, '(1 1) 1})
(def and-or-cases {'(0 0 0 0) 0,
 '(0 0 0 1) 0,
 '(0 0 1 0) 0,
 '(0 0 1 1) 0,
 '(0 1 0 0) 0,
 '(0 1 0 1) 1,
 '(0 1 1 0) 1,
 '(0 1 1 1) 1,
 '(1 0 0 0) 0,
 '(1 0 0 1) 1,
 '(1 0 1 0) 1,
 '(1 0 1 1) 1,
 '(1 1 0 0) 0,
 '(1 1 0 1) 1,
 '(1 1 1 0) 1,
 '(1 1 1 1) 1})
(def sample-OR-init-prog-pop '(((nand 0 0 0) (qnot 0) (oracle 1 0) (utheta 0 4.15208267191425) (srn 0)) ((cnot 0 0) (utheta 0 1.7770179045107217) (swap 0 0) (oracle 1 0) (srn 0) (swap 0 0) (u2 0 0.5238096923846751 3.5756346378475428 1.1302016100287438 2.7219356784783306)) ((oracle 1 0)) ((cnot 0 0) (oracle 1 0) (u2 0 2.746269316794351 0.7390194404125214 3.327840401856229 2.768858401691541)) ((cphase 0 0 1.4471730569843204) (hadamard 0) (qnot 0) (nand 0 0 0) (oracle 1 0) (qnot 0) (u2 0 3.711076012440026 2.831825133729726 1.515008683046326 5.536347292372957) (cphase 0 0 0.16675655011087656) (u2 0 5.670343877263368 4.700450126482174 0.7951780961640179 1.5348517625685782)) ((hadamard 0) (u2 0 0.2588917823322117 5.169908034494147 4.855066692028195 4.791770850392059) (cphase 0 0 4.05389395465143) (oracle 1 0) (qnot 0) (cphase 0 0 3.6936640907637774) (srn 0)) ((srn 0) (nand 0 0 0) (utheta 0 1.1428459708951415) (srn 0) (oracle 1 0) (swap 0 0) (nand 0 0 0) (utheta 0 0.3570419390778989) (cnot 0 0)) ((oracle 1 0)) ((u2 0 5.72163669786416 0.5154636111855507 2.648374596424401 5.118814907716588) (u2 0 4.579345176964998 4.308966836249456 3.0596052886638794 2.688036571070552) (cnot 0 0) (u2 0 3.4004325039684775 2.431012499816716 1.3321964837238194 4.007025369983166) (cnot 0 0) (oracle 1 0) (nand 0 0 0) (cnot 0 0) (swap 0 0) (srn 0) (cnot 0 0)) ((hadamard 0) (oracle 1 0) (cnot 0 0))))
;(println (interleave sample-OR-init-prog-pop (map #(qg/error % or-cases) sample-OR-init-prog-pop)))
(def sample-aoa-pop (repeatedly 10 #(qg/generate-qgame-program 4 10)))
(def sample-aoa (qg/generate-qgame-program 4 10))
(def sample-ao (qg/generate-qgame-program 3 10))
(println "sample-aoa = " sample-aoa "\n")
(println "sample-ao = " sample-ao "\n")
;(println "spaced once = " (map ev/spacify sample-aoa) "\n")
;(println "spaced twice = " (ev/spacify (map ev/spacify sample-aoa)) "\n")
(println "aoa-cases = " aoa-cases)
(let [program+ (cons (cons 'with-oracle (first (keys qg/and-or-cases))) sample-ao)]
	(println "program+ = " program+ "\n")

(let [mid-output (ev/prog-to-string program+)]
	(println "mid-output = " mid-output "\n")
  (println "spacified program+ = " (map ev/spacify program+))

(let [output (i/interpret mid-output)]
	(println "output = " output "\n"))
))

;(println (qg/evolve (repeatedly 10 #(qg/generate-qgame-program 3 10)) 10 0.4 3 10 3 and-or-cases))
(println (qg/evolve (repeatedly 10 #(qg/generate-qgame-program 4 10)) 10 0.4 10 10 4 aoa-cases))

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "GPQuantum"}))

(defn hello-world []
  [:h1 (:text @app-state)])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
