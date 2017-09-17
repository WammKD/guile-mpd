(define-module (mpd client)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:export (<mpd-client>
	    <mpd-client-response>
	    mpd-response?
	    mpd-response-error?
	    get-mpd-response
            new-mpd-client
	    mpd-client?
            handle-response
            send-command
            mpd-connect
            connected?
            disconnect))

(define-record-type <mpd-client>
  (make-mpd-client host port)
  mpd-client?
  (host      get-mpd-host      set-mpd-host!)
  (port      get-mpd-port      set-mpd-port!)
  (socket    get-mpd-socket    set-mpd-sock!)
  (version   get-mpd-version   set-mpd-version!)
  (tags      get-mpd-tags      set-mpd-tags!)
  (connected get-mpd-connected set-mpd-connected!))
(set-record-type-printer!
  <mpd-client>
  (lambda (client port)
    (format
      port "<mpd-client ~a:~a" (get-mpd-host client) (get-mpd-port client))

    (when (get-mpd-version client)
      (format port " version: ~a" (get-mpd-version client)))

    (format port ">")))

(define-record-type <mpd-client-response>
  (make-mpd-response error response)
  mpd-response?
  (error    mpd-response-error? set-response-error!)
  (response get-mpd-response    set-mpd-response-message!))
(set-record-type-printer!
  <mpd-client-response>
  (lambda (client port)
    (format port (string-append
		   "<mpd-client-response, "
		   (if (mpd-response-error? client) "with" "no")
		   " error>"))))



(define* (new-mpd-client #:optional [hostname "localhost"] [port 6600])
  (make-mpd-client hostname port))

(define (mpd-connect client)
  (define addresses (delete-duplicates
                      (getaddrinfo
                        (get-mpd-host client)
                        (number->string (get-mpd-port client))
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
          (set-mpd-sock!      client sock)
          (set-mpd-version!   client (match:substring
				       (string-match "^OK MPD (.+)$" (read-line
								       sock))
				       1))
	  (set-mpd-connected! client #t)
          client)
        (lambda args
          (close sock) (if (null? (cdr addresses))
			   (apply throw args)
			 (address-loop (cdr addresses))))))))

(define (mpd-receive sock)
  (call/cc
    (lambda (return)
      (while #t
	(when (char-ready? sock)
	  (let loop ([line   (read-line sock)]
		     [result              '()])
	    (cond
	     [(string=? "OK" line)
	           (return                      result)]
	     [(let ([response (string-contains line "ACK [")])
		(and (number? response) (= 0 response)))
	           (return (make-mpd-response #t line))]
	     [else (loop
		     (read-line sock)
		     (append
		       result
		       (list
			 (let ([str-ind (string-index line #\:)])
			   (if str-ind
			       (cons
				 (string->symbol
				   (string-trim-both (substring line 0
								str-ind)))
				 (let* ([scnd (string-trim-both
					        (substring line (1+ str-ind)))]
					[num?            (string->number scnd)])
				   (if num? num? scnd)))
			       line)))))])))))))

(define* (send-command client str #:optional [handler *unspecified*])
  (write-line str (get-mpd-socket client))

  (let ([response (mpd-receive (get-mpd-socket client))])
    (cond  ; Return errors as mpd-responses so non-errors can be any value
     [(mpd-response?  response)                                       response]
     [(equal? handler *unspecified*) (make-mpd-response #f            handler)]
     [else                           (make-mpd-response #f (handler response))])))

(define (connected? client)
  (get-mpd-connected client))

(define (disconnect client)
  (write-line "close" (get-mpd-socket client))
  (close (get-mpd-socket client))
  (set-mpd-connected! client #f)
  #t)
