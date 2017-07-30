#lang racket

;; read in a name value pair style config file where:
;; "#" is a comment character
;; name then value appear on a single line separated by whitespace

;; contract with client
(define names? (listof string?))
(provide (contract-out (file->config (-> string? config?))
                       (config-value (-> config? string? (or/c string? #f)))
                       (config-duplicates (-> config? names?))
                       (config-missing-names (-> config? names? names?))
                       (config-additional-names (-> config? names? names?))))

;; name-value pair datatype
(define nvp? (cons/c string? string?))
(define (make-nvp name value) (cons name value))
(define (nvp-name nvp) (car nvp))
(define (nvp-value nvp) (cdr nvp))

;; config datatype
(define config? (listof nvp?))
(define (make-config config-lines)
  (cond ((null? config-lines) '())
        (else
          (let ((tokens (string->tokens (car config-lines))))
            (cond ((null? tokens) (make-config (cdr config-lines)))
                  ((nvp-pair? tokens)
                   (cons (tokens->nvp tokens) (make-config (cdr config-lines))))
                  (else (raise-user-error (format "Invalid name value pair: ~a"
                                                  (car config-lines)))))))))
(define (config-value config name)
  (cond ((null? config) #f)
        ((string=? (nvp-name (car config)) name) (nvp-value (car config)))
        (else (config-value (cdr config) name))))
        
(define (string->tokens str)
  ;; this simple approach to comment removal means our names/values cannot
  ;; contain hash chars
  (define (remove-comment str) (regexp-replace #rx"#.*$" str ""))
  (define (non-empty? str) (not (string=? str "")))
  (filter non-empty? (regexp-split #rx"[ \t\r\n]" (remove-comment str))))

(define (nvp-pair? tokens) (= (length tokens) 2))

(define (tokens->nvp tokens) (make-nvp (car tokens) (cadr tokens)))

(define (file->config file-name)
  (cond ((file-exists? file-name) (make-config (file->lines file-name)))
        (else (raise-user-error (format "File ~a not found" file-name)))))


;; config validation routines - client decides which rules to enforce

;; find duplicate names - if nothing is done about duplicates, the
;; default behaviour will be to use the first instance
(define (config-duplicates config)
  (define (duplicates names)
    (cond ((null? names) '())
          ((member (car names) (cdr names))
           (cons (car names) (duplicates (cdr names))))
          (else (duplicates (cdr names)))))
  (remove-duplicates (duplicates (map nvp-name config))))

;; to allow checking for the presence of mandatory options
(define (config-missing-names config names)
  (cond ((null? names) '())
        ((config-value config (car names)) 
         (config-missing-names config (cdr names)))
        (else (cons (car names) (config-missing-names config (cdr names))))))

;; to allow checking for unknown options
(define (config-additional-names config names)
  (cond ((null? config) '())
        ((member (nvp-name (car config)) names) 
         (config-additional-names (cdr config) names))
        (else (cons (nvp-name (car config))
                    (config-additional-names (cdr config) names)))))
