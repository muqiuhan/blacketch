#lang racket/base

(require sha)
(require racket/serialize)

;; A block should contain the current hash, the previous hash, the data and the timestamp that it was generate
;; Using a hashing algorithm will allow us to confirm that the block is valid.
;; In general, blocks can contain any data, not just transactions, but I'm limiting them to transactions for now.
(struct block
  (current-hash previous-hash data timestamp transaction nonce)
  #:prefab)

;; The block also contains a transaction that is roughly of the following form:
(struct transaction
  (signature from to value)
  #:prefab)

;; Calculates a block's hash with the SHA hashing algorithm.
(define (block/calculate-hash previous-hash timestamp transaction nonce)
  (bytes->hex-string
   (sha256
    (bytes-append
     (string->bytes/utf-8 previous-hash)
     (string->bytes/utf-8 (number->string timestamp))
     (string->bytes/utf-8 (format "~a" (serialize transaction)))
     (string->bytes/utf-8 (number->string nonce))))))

;; Verify block, just hash the block's contents again and compare this hash to the one stored in the block.
(define (valid-block? a-block)
  (equal? (block-current-hash a-block)
          (block/calculate-hash
           (block-previous-hash a-block)
           (block-timestamp a-block)
           (block-transaction a-block)
           (block-nonce a-block))))
