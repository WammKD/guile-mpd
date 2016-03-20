(define-module (mpd)
  #:use-module (mpd client)
  #:use-module (mpd commands))
  
(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use!
         (module-public-interface (current-module))
         (resolve-interface '(mod ...)))
       ...))))

(re-export-modules
  (mpd client)
  (mpd commands))
