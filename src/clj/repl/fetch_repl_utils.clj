;; Copyright (c) Frank Siebenlist. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clj.repl.fetch-repl-utils
  "fetch-repl related utility functions looking for a home..."
  (:import [clojure.lang.PersistentQueue]))


;; basic queue facility 
;; http://stackoverflow.com/questions/8938330/clojure-swap-atom-dequeuing

(defn make-atomic-queue
  "(make-atomic-queue)  =>  atom-of-persistent-queue
  atom-of-persistent-queue --- returns a new atomic-queue
  Factory function that returns a new persistent queue wrapped-up in an atom."
  [] 
  (atom clojure.lang.PersistentQueue/EMPTY))

(defn dequeue!
  "(dequeue! a-queue)  =>  an-item-or-nil
  a-queue --- atomic-queue.
  an-item-or-nil --- returns the dequeue'd item or nil when a-queue is empty.
  Returns the dequeue'ed value from a-queue.
  a-queue is replaced with the dequeue'd a-queue.
  (note that nil should not be a valid value)"
  [a-queue]
  (when (seq @a-queue)
    (loop []
      (let [q     @a-queue
            value (peek q)
            nq    (pop q)]
        (if (compare-and-set! a-queue q nq)
          value
          (recur))))))


(defn enqueue!
  "(enqueue! a-queue an-item)  => changed-atomic-queue
  a-queue --- atomic-queue
  an-item --- anything but nil
  changed-atomic-queue --- returns the atom with the enqueue'd queue
  Add an-item to the atomic-queue, and replace a-queue's queue with that new queue."
  [a-queue an-item]
  (swap! a-queue conj an-item))

;;;;

;;; basic counter facility to provide unique sequential numbers within a session
(defn next-count! 
  "(next-count! a-counter)  =>  incremented-counter-value
  a-counter --- an atom with an integer counter value
  incremented-counter-value --- returns the incremented counter value
  Increments the atom's integer value and returns that next counter value"
  [a-counter] 
  (swap! a-counter inc))


