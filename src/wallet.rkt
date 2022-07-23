#lang racket/base

(require crypto)
(require crypto/all)

;; A wallet is structure that contains a public and a private key.
(struct wallet
  (private-key public-key)
  #:prefab)

;; Create a wallet by generating random public and private keys, and it will rely on the RSA algorithm.
(define (wallet/create)
  (letrec ([rsa-impl (get-pk 'rsa libcrypto-factory)]
           [prikey (generate-private-key rsa-impl '((nbits 512)))]
           [pubkey (pk-key->public-only-key prikey)])
    (wallet
     (bytes->hex-string (pk-key->datum prikey 'PrivateKeyInfo))
     (bytes->hex-string (pk-key->datum pubkey 'SubjectPublicKeyInfo)))))

(provide (struct-out wallet)
         wallet/create)
