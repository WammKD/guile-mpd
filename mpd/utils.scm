(define-module (mpd utils))

(define (mpd-response-list->assoc lst)
  (map (lambda (str)
         (let ([key-value (map string-trim-both (string-split str #\:))])
           (cons (list-ref key-value 1) (list-ref key-value 2)))) lst))
