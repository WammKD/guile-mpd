(define-module (mpd utils)
  :use-module (srfi srfi-1))

(define (mpd-response-list->assoc lst)
  (map (lambda (str)
         (let ([key-value (map string-trim-both (string-split str #\:))])
           (cons (first key-value) (second key-value)))) lst))
