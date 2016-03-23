(define-module (mpd client)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:export (<mpd-client>
            new-mpd-client
            handle-response
            send-command
            mpd-connect
            connected?
            disconnect))

(define-record-type <mpd-client>
  (make-mpd-client host port)
  mpd-client?
  (host mpd-host set-mpd-host!)
  (port mpd-port set-mpd-port!)
  (socket mpd-socket set-mpd-sock!)
  (version mpd-version set-mpd-version!)
  (tags mpd-tags set-mpd-tags!))


(set-record-type-printer!
  <mpd-client>
  (lambda (client port)
    (format port "<mpd-client ~a:~a" (mpd-host client) (mpd-port client))

    (when (mpd-version client)
      (format port " version: ~a" (mpd-version client)))

    (format port ">")))



(define* (new-mpd-client #:optional [hostname "localhost"] [port 6600])
  (make-mpd-client hostname port))

(define (mpd-connect client)
  (define addresses (delete-duplicates
                      (getaddrinfo
                        (mpd-host client)
                        (number->string (mpd-port client))
                        AI_NUMERICSERV)
                      (lambda (a1 a2)
                        (equal? (addrinfo:addr a1) (addrinfo:addr a2)))))

  (let address-loop ([addresses addresses])
    (let* ([ai   (car addresses)]
           [sock (with-fluids ((%default-port-encoding #f)) (socket
                                                              (addrinfo:fam ai)
                                                              SOCK_STREAM
                                                              IPPROTO_IP))])
      (catch 'system-error
        (lambda ()
          (connect sock (addrinfo:addr ai))
          (set-mpd-sock! client sock)
          (set-mpd-version! client (match:substring
                                     (string-match "^OK MPD (.+)$" (read-line
                                                                     sock))
                                     1))
          client)
        (lambda args
          (close sock)
          (if (null? (cdr addresses))
              (apply throw args)
            (address-loop (cdr addresses))))))))

(define (mpd-receive sock)
  (call/cc
   (lambda (return)
     (while #t
       (when (char-ready? sock)
         (let loop ([line  (read-line sock)]
                    [lines              '()])
           (if (string=? "OK" line)
               (return (reverse lines))
             (loop (read-line sock) (cons (let* ([i   (string-index line #\:)]
                                                 [s2 (substring line (+ i 2))]
                                                 [n?      (string->number s2)])
                                            (if i
                                                (cons
                                                  (string->symbol
                                                    (substring line 0 i))
                                                  (if n? n? s2))
                                              line)) lines)))))))))

(define* (send-command client str #:optional [handler *unspecified*])
  (write-line str (mpd-socket client))

  (let ([response (mpd-receive (mpd-socket client))])
    (if (or (equal? handler #t) (equal? handler *unspecified*))
        handler
      (handler response))))

(define (connected? client)
  (or (mpd-socket client) #t))

(define (disconnect client)
  (write-line "close" (mpd-socket client))
  (close (mpd-socket client))
  #t)
