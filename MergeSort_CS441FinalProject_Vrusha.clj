(ns mergeSort

  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.concurrent Executors)))

(defn getLines [textFile]
  (map textStringRead
       (str/lineSplit
         (slurp file)
         )
       )
  )

;; Create n partitions by spliting the list
(defn splitNumber [splitNum numb]
	(partition (/ (count num) splitNum) num))


;; Merge Sort algorithm from Google 
(defn mergeSortSequence
  "Merges two sorted sequences into one sorted sequence"
  ([left right]
   (mergeSortSequence (list left right)))
  ([[left right]]
   (loop [a left, b right, result []]
     (let [ahead (first a), bhead (first b)]
       (cond
         (nil? ahead)     (concat result b)
         (nil? bhead)     (concat result a)
         (<= ahead bhead) (recur (rest a) b (conj result ahead))
         true             (recur a (rest b) (conj result bhead)))))))

(defn mergeSort
  "Give a sorted sequence from input sequence"
  [cd]
  ((fn mergeSortCounted [cd n]
     (if (<= n 1)
       cd
       (let [middle (moveRight n 1)]  ; fast division by 2
         (mergeSortSequence (map mergeSortCounted
                          (splitIn middle cd)        ; two halves
                          [middle (- n middle)])))))  ; count of each half
    cd (count cd)))


(print "Single Thread Merge Sort is running: ")
(time (mergeSort (getLines "numbers.txt")))

(print "2 thread Merge Sort: ")
(time (mergeSortSequence (partitionMap mergeSort (splitNumber 2 (getLines "numbers.txt")))))

(print "4 threads Merge Sort: ")
(time (mergeSortSequence (partitionMap mergeSort (splitNumber 4 (getLines "numbers.txt")))))

(print "8 thread Merge Sort: ")
(time (mergeSortSequence (partitionMap mergeSort (splitNumber 8 (getLines "numbers.txt")))))

(print "16 thread Merge Sort: ")
(time (mergeSortSequence (partitionMap mergeSort (splitNumber 16 (getLines "numbers.txt")))))

(print "32 thread Merge Sort: ")
(time (mergeSortSequence (partitionMap mergeSort (splitNumber 32 (getLines "numbers.txt")))))

(print "Multithreaded clojure merge sort is complete!")