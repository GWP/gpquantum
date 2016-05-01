(ns gpquantum.core
  (:require
            [qgame.gp.evaluator :as ev]
            [qgame.simulator.interpreter :as i]
            ))
;[reagent.core :as reagent :refer [atom]]


;;Sample run: (evolve (repeatedly 25 #(generate-qgame-program 2 10)) 10 0.10 10 10 2 or-cases correct-or-cases)

(def or-cases {'(0 0) 0, '(0 1) 1, '(1 0) 1, '(1 1) 1})
(def sample-OR-init-prog-pop '(((nand 0 0 0) (qnot 0) (oracle 1 0) (utheta 0 4.15208267191425) (srn 0)) ((cnot 0 0) (utheta 0 1.7770179045107217) (swap 0 0) (oracle 1 0) (srn 0) (swap 0 0) (u2 0 0.5238096923846751 3.5756346378475428 1.1302016100287438 2.7219356784783306)) ((oracle 1 0)) ((cnot 0 0) (oracle 1 0) (u2 0 2.746269316794351 0.7390194404125214 3.327840401856229 2.768858401691541)) ((cphase 0 0 1.4471730569843204) (hadamard 0) (qnot 0) (nand 0 0 0) (oracle 1 0) (qnot 0) (u2 0 3.711076012440026 2.831825133729726 1.515008683046326 5.536347292372957) (cphase 0 0 0.16675655011087656) (u2 0 5.670343877263368 4.700450126482174 0.7951780961640179 1.5348517625685782)) ((hadamard 0) (u2 0 0.2588917823322117 5.169908034494147 4.855066692028195 4.791770850392059) (cphase 0 0 4.05389395465143) (oracle 1 0) (qnot 0) (cphase 0 0 3.6936640907637774) (srn 0)) ((srn 0) (nand 0 0 0) (utheta 0 1.1428459708951415) (srn 0) (oracle 1 0) (swap 0 0) (nand 0 0 0) (utheta 0 0.3570419390778989) (cnot 0 0)) ((oracle 1 0)) ((u2 0 5.72163669786416 0.5154636111855507 2.648374596424401 5.118814907716588) (u2 0 4.579345176964998 4.308966836249456 3.0596052886638794 2.688036571070552) (cnot 0 0) (u2 0 3.4004325039684775 2.431012499816716 1.3321964837238194 4.007025369983166) (cnot 0 0) (oracle 1 0) (nand 0 0 0) (cnot 0 0) (swap 0 0) (srn 0) (cnot 0 0)) ((hadamard 0) (oracle 1 0) (cnot 0 0))))
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


(defn random-gate 
"generates a quantum gate from one of the 9 possible gates. Randomly generates parameters for the gate."
	[num-qubit]
	(let [index (rand-int 9)] 
		(case index
			0 (list 'qnot (rand-int num-qubit))
			1 (list 'cnot (rand-int num-qubit) (rand-int num-qubit))
			2 (list 'hadamard (rand-int num-qubit))
			3 (list 'srn (rand-int num-qubit))
			4 (list 'utheta (rand-int num-qubit) (rand 6.28))
			5 (list 'cphase (rand-int num-qubit) (rand-int num-qubit) (rand 6.28))
			6 (list 'u2 (rand-int num-qubit) (rand 6.28) (rand 6.28) (rand 6.28) (rand 6.28))
			7 (list 'swap (rand-int num-qubit) (rand-int num-qubit))
			8 (list 'nand (rand-int num-qubit) (rand-int num-qubit) (rand-int num-qubit)))))

(defn error 
	"defines a total error for a quantum circuit (program) by finding the case that resulted in the maximum error and summing with all the cases for which the program missed. A program 'misses' a case when the read-from qubit has the wrong value with a greater than 52% probability. Each miss counts as 10 in the error sum. This way, comparing the maximum error of two programs is only relevant if they have the same number of misses. For simplicity, this function has the read-from qubit set to qubit 0 by default."
[program cases]
	(println program "\n")
	(let [cases-and-output (ev/test-quantum-program :program program :read-from [0] :cases (keys cases))]
		(loop [miss-count 0 
				max-error 0 
				cases-left (keys cases)]
			(if (empty? cases-left) 
				(+ (* 10 miss-count) max-error)
				(let [current-prog (cases-and-output (first cases-left)) 
					  prob-correct (current-prog (cases (first cases-left)))]
					(recur
						(if (> 0.48 prob-correct) (inc miss-count) miss-count)
						(max (- 1 prob-correct) max-error)
						(rest cases-left)
						))))))

(defn sort-by-error [population cases] ;;removes duplicates
	(loop [paired (apply assoc {} (interleave population (map #(error % cases) population))) 
		   sorted []]
		(if (empty? paired) 
			sorted
			(let [max-error (apply max-key val paired)]
				(recur 
					(dissoc paired (first max-error))
					(cons max-error sorted))
			))))

(defn mutate 
	"Takes a program and either changes one gate with a new random gate, slots a new gate in between two existing ones (including possibly at the beginning or at the end), or deletes one gate"
	[program maxsize num-qubits]
	(let [vprogram (into [] program) 
		  r (rand 1) 
		  n (loop [x (rand-int (count program))] 
			  	(if (or (not (= 'oracle (first (nth vprogram x)))) (= 1 (count vprogram)))
			  		x 
			  		(recur (rand-int (count vprogram)))))]
		(cond
			(= 1 (count vprogram)) (if (> 0.5 (rand 1)) (conj vprogram (random-gate num-qubits)) (conj (random-gate num-qubits) vprogram))
			(or (= maxsize (count program)) (< r 0.33)) (into '() (assoc vprogram n (random-gate num-qubits)))
			(< r 0.66) (vec (concat (subvec vprogram 0 n) (subvec vprogram (+ n 1) (count vprogram)))) ;deletes nth gate in program
			:else (let [n (rand-int (inc (count program)))]
						(if (= n (count program))
							(conj vprogram (random-gate num-qubits))
							(concat 
								(conj (subvec vprogram 0 n) (random-gate num-qubits)) ;;is this the right order?
								(subvec vprogram n))))
			)))

(defn crossover
	"Picks a random index (within the size of the smaller program) and outputs a concatenation of the first program truncated after the index, with the second program truncated before the index, and a second program as a concatenation of the two remaining bits. The maxsize input is to ensure that the new program does not exceed the maximum size."
	[program1 program2 maxsize qubits]
	(let [p1 (into [] program1) 
		  p2 (into [] program2)]
		(loop [x 0]
			(cond 
				(= 'oracle (first (nth p1 x))) (list (concat (take x p1) (drop x p2)) (concat (take x p2) (drop x p1)))
				(= 'oracle (first (nth p2 x))) (list (concat (take x p2) (drop x p1)) (concat (take x p1) (drop x p2)))
				:else (recur (inc x))
		))))

(defn generate-qgame-program
	"A general quantum circuit generator that just concatenates a random number (<= max-length) of randomly generated gates, with the appropriate oracle in the middle."	
[num-qubits max-length]
	(let [size-holder (rand-int max-length)
		  randomly-chosen-size (cond (< size-holder 1) (inc (inc size-holder))
		  							 (< size-holder 2) (inc size-holder)
		  							 :else size-holder)
		  oracle-placement (rand-int randomly-chosen-size)] ;The cond statement is to avoid an error from ((oracle 1 0))
		(loop [current-size 0 program '()]
			(if (= current-size randomly-chosen-size) program
				(recur
					(inc current-size)
					(conj program
						(cond
							(= current-size oracle-placement)
								(case num-qubits
									1 '(oracle 0)
									2 '(oracle 1 0)
									3 '(oracle 1 2 0)
									4 '(oracle 1 2 3 0))
							:else (random-gate num-qubits)
						)))))))

(defn evolve
	"the top level call that begins an evolution. Right now the popsize element is a little confusing as it is set as 2/5 of the actual initial population size and represents the size of the population to be used as parents for the next generation."
[init popsize tolerance maxgeneration maxsize num-qubits cases] ;;popsize multiple of 5
	(loop [generation 0 
		   population (sort-by-error init cases)]
		(let [best (first (first population)) 
			  best-error (second (first population)) 
			  parentpop (map #(first %) (take popsize population))] ;(drop (- (count population) (/ popsize 5)) population)
			;(println "===============")
			;(println "Generation: " generation)
			;(println "Best error: " best-error)
			;(println "Best program: " best)
			(cond 
				(< best-error tolerance) (do (println "Success: " best " Generation: " generation " Best error: " best-error) best)
				(= generation maxgeneration) (do (println "Failure: " best " Generation: " generation " Best error: " best-error) best)
				;(= generation 10) (println "limit reached")
				:else (recur
						(inc generation)
						(sort-by-error 
							(loop [newpop (take (/ (count parentpop) 2) parentpop) 
								   n 0]
								(if (= n (/ (count parentpop) 2)) 
									newpop
									(recur
										(concat 
											(conj newpop
												(generate-qgame-program num-qubits maxsize))
											(repeatedly 2 #(mutate (nth parentpop n) maxsize num-qubits))
											(repeatedly 2 #(mutate (nth parentpop (dec (- (count parentpop) n))) maxsize num-qubits))
											(crossover (nth parentpop n) (nth parentpop (dec (- (count parentpop) n))) maxsize num-qubits)
											(crossover (nth parentpop (* n 2)) (nth parentpop (inc (* n 2))) maxsize num-qubits)
											)
										(inc n))))
							cases))
				))))

(defn multievolve 
	"performs 25 separate OR evolutions, and then a final one using the best of each evolution as the population"
	[]
	(loop [n 0 super-init-prog-pop '()]
		(if (= n 25)
			(evolve super-init-prog-pop 10 0.10 50 20 2 or-cases)
			(let [current-trial (evolve (repeatedly 25 #(generate-qgame-program 2 20)) 10 0.10 50 20 2 or-cases)]
				(cond
					(< (error current-trial or-cases) 0.1) (str "Success: " current-trial)
					:else (recur
							(inc n)
							(conj super-init-prog-pop current-trial)))))))

(defn multievolve-2 
	"The same as multievolve, except for AND/OR"
	[]
	(loop [n 0 super-init-prog-pop '()]
		(if (= n 25)
			(evolve super-init-prog-pop 10 0.15 50 20 3 and-or-cases)
			(let [current-trial (evolve (repeatedly 25 #(generate-qgame-program 3 20)) 10 0.15 50 20 3 and-or-cases)]
				(cond
					(< (error current-trial and-or-cases) 0.15) (str "Success: " current-trial)
					:else (recur
							(inc n)
							(conj super-init-prog-pop current-trial)))))))

(defn permutation [bit-count] ;; i.e 8
	(into [] (for [x (range (#(reduce * (repeat %2 %1)) 2 bit-count))] 
		(map #(. Integer parseInt %) 
			(loop [bin-sequence (seq (.toString (int (str x ".")) 2))]
				(if (= (count bin-sequence) bit-count) 
					bin-sequence
					(recur (conj bin-sequence "0"))
					))))))

(defn gen-and-or-and-cases
	[]
	(apply assoc {} 
		(interleave 
			(permutation 8)
			(into [] (for [perm (permutation 8)]
						(if (and
								(or
									(and (= (nth perm 0) 1) (= (nth perm 1) 1))
									(and (= (nth perm 2) 1) (= (nth perm 3) 1)))
								(or
									(and (= (nth perm 4) 1) (= (nth perm 5) 1))
									(and (= (nth perm 6) 1) (= (nth perm 7) 1)))
								)
							1 
							0)
			)))))

(def AND-sample (conj (repeatedly 24 #(generate-qgame-program 3 20)) '((srn 2) (cphase 0 1 0.18988939795643092) (utheta 0 0.11555971846915782) (srn 2) (cnot 0 2) (u2 1 4.261788923768327 0.6810310229100287 1.7455522052384913 1.1971161058731377) (srn 2) (cnot 0 0) (hadamard 0) (swap 0 1) (oracle 1 2 0) (cnot 1 0) (cnot 1 2) (nand 2 1 2) (srn 2) (qnot 2) (nand 1 0 0) (u2 0 5.416449490869418 1.3663847628422081 2.5490581375639887 5.051497216718271) (swap 1 2) (nand 0 1 0))))


(defn -main
	[& args]
	(println "Now we begin!")
	(let [sample (generate-qgame-program 2 10)]
		(println "our sample is: " sample)
		(println "our cases are: " or-cases)
		(println "and the error is: " (error sample or-cases))
		))




