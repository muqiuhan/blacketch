;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; blockchain.rkt : The blockchain.                                                            ;;
;;                                                                                             ;;
;; Copyright (c) 2022 Muqiu Han                                                                ;;
;;                                                                                             ;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy                ;;
;; of this software and associated documentation files (the "Software"), to deal               ;;
;; in the Software without restriction, including without limitation the rights                ;;
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell                   ;;
;; copies of the Software, and to permit persons to whom the Software is                       ;;
;; furnished to do so, subject to the following conditions:                                    ;;
;;                                                                                             ;;
;; The above copyright notice and this permission notice shall be included in all              ;;
;; copies or substantial portions of the Software.                                             ;;
;;                                                                                             ;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR                  ;;
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                    ;;
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                 ;;
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                      ;;
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,               ;;
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE               ;;
;; SOFTWARE.                                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(require "block.rkt")
(require "transaction.rkt")
(require "utils.rkt")
(require "wallet.rkt")
(require "smart-contracts.rkt")

(require racket/set)
(require racket/list)

(struct blockchain
  (blocks utxo)
  #:prefab)

; Procedure for initialization of the blockchain
(define (blockchain/init t seed-hash utxo)
  (blockchain
   (cons
    (block/mine
     (transaction/process t) seed-hash) '())
   utxo))

; Start with 50 coins initially, and halve them on every 210000 blocks
(define (blockchain/mining-reward-factor blocks)
  (/ 50 (expt 2 (floor (/ (length blocks) 210000)))))

; Add transaction to blockchain by processing the unspent transaction outputs
(define (blockchain/add-transaction b t)
  (letrec ([hashed-blockchain (block/mine t (block-hash (car (blockchain-blocks b))))]
           [processed-inputs (transaction-inputs t)]
           [processed-outputs (transaction-outputs t)]
           [utxo (set-union processed-outputs
                            (set-subtract (blockchain-utxo b) processed-inputs))]
           [new-blocks (cons hashed-blockchain (blockchain-blocks b))]
           [utxo-rewarded (cons
                           (transaction-io/make (blockchain/mining-reward-factor new-blocks)
                                                (transaction-from t))
                           utxo)])
    (blockchain
     new-blocks
     utxo-rewarded)))

; Send money from one wallet to another by initiating transaction, and then adding it to the blockchain for processing
(define (blockchain/send-money b from to value c)
  (letrec ([my-ts (filter
                   (lambda (t)
                     (equal? from (transaction-io-owner t)))
                   (blockchain-utxo b))]
           [t (transaction/make from to value my-ts)])
    (if (transaction? t)
        (let ([processed-transaction (transaction/process t)])
          (if (and
               (>= (blockchain/balance-wallet b from) value)
               (smart-contract/valid-transaction? processed-transaction c))
              (blockchain/add-transaction b processed-transaction)
              b))
        (blockchain/add-transaction b '()))))

; The balance of a wallet is determined by the sum of all unspent transactions for the matching owner
(define (blockchain/balance-wallet b w)
  (letrec ([utxo (blockchain-utxo b)]
           [my-ts (filter
                   (lambda (t)
                     (equal? w (transaction-io-owner t)))
                   utxo)])
    (foldr + 0 (map (lambda (t)
                      (transaction-io-value t))
                    my-ts))))

; A blockchain is valid if...
(define (blockchain/valid? b)
  (let ([blocks (blockchain-blocks b)])
    (and
     ; All blocks are valid
     (utils/true-for-all? block/valid? blocks)
     ; Previous hashes are matching
     (equal? (drop-right (map block-previous-hash blocks) 1)
             (cdr (map block-hash blocks)))
     ; All transactions are valid
     (utils/true-for-all?
      transaction/valid? (map
                          (lambda (block) (block-transaction block))
                          blocks))
     ; All blocks are mined
     (utils/true-for-all?
      block/mined? (map block-hash blocks)))))

(provide (all-from-out "block.rkt")
         (all-from-out "transaction.rkt")
         (all-from-out "wallet.rkt")
         (struct-out blockchain)
         blockchain/init
         blockchain/send-money
         blockchain/balance-wallet
         blockchain/valid?)
