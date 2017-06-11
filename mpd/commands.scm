(define-module (mpd commands)
  :use-module (mpd client)
  :use-module (mpd utils)
  :use-module (srfi srfi-1)
  :use-module (ice-9 regex)
  :use-module (ice-9 optargs))

;;;   Helper Methods   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (number?->string n)
  (if (number? n) (number->string n) n))

; Due to – it seems – that #:optional and a #f – if nothing is given for the
; optional argument – are sent through, there's no easy way to pick apart which
; values are from people calling the function so we discard all non-number/strings
(define (filter/convert-strings/nums l)
  (map
    (lambda (ns)
      (string-append "\"" (number?->string ns) "\""))
    (filter (lambda (ns)
              (or (string? ns) (number? ns))) l)))

(define (create-ranges-from-list values)
  (define (make-range e1 e2)
    (string-append (number?->string e1) ":" (number?->string e2)))

  (let loop ([init values] [final '()])
    (cond
     [(null? init)
               final]
     [(let ([ci (car init)]) (and (string? ci) (string-index ci #\:)))
               (loop (cdr init)  (cons (car init)                 final))]
     [else (let ([v2 (cadr init)])
             (if (and (string? v2) (string-index v2 #\:))
                 (error "wrong format of paired values")
               (loop (cddr init) (cons (make-range (car init) v2) final))))])))

;;;   Handler Methods   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mpdHandlers::general (lambda (resp)
                               resp))

; Converting '((file . "f") (artist . "a") … (file . "f2") (artist . "a2") …)
; to '(((file . "f") (artist . "a") …) ((file . "f2") (artist . "a2") …) …),
; for example
(define (mpdHandlers::parse-files delimeters)
  (lambda (resp)
    (let ([result (reduce-right
                    (lambda (elem lst)
                      (if (list? lst)
                          (if (any (lambda (delim)
                                     (equal? (caaar lst) delim)) delimeters)
                              (cons (list elem) lst)
                            (cons (cons elem (car lst)) (cdr lst)))
                        (list (list elem lst))))
                    '()
                    resp)])
      (if (list? result) result (list (list result))))))

(define (mpdHandlers::parse-dirs d)
  (lambda (resp)
    (cdr (let loop ([orig resp] [final '()] [dir d])
           (cond
            [(null? orig)
                  (cons orig (reverse final))]
            [(and (eq? (caar orig) 'directory) (not
                                                 (string=? (cdar orig) dir)))
                  (if (string-contains (cdar orig) dir)
                      (let ([answer (loop
                                      (cdr orig)
                                      (list (car orig))
                                      (cdar orig))])
                        (loop (car answer) (cons (cdr answer) final) dir))
                    (cons orig (reverse final)))]
            [else (loop (cdr orig) (cons (car orig) final) dir)])))))

(define (mpdHandlers::convert-states attributes-with-state)
  (lambda (resp)
    (map (lambda (attr)
	   (if (any (lambda (title)
		      (equal? title (car attr))) attributes-with-state)
	       (cons (car attr) (if (= (cdr attr) 1) #t #f))
	       attr)) resp)))

;;;   Creation Methods   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bind-all-arguments-to-one-string  command . args)
  (string-join
    (cons command (filter/convert-strings/nums args))
    " "))

(define (convert-state-and-bind-to-command command  state)
  (if (boolean? state)
      (string-append command (if state " 1" " 0"))
    (error "STATE must be a boolean value; do not pass 0 or 1")))

(define-syntax mpd-define
  (lambda (stx)
    (syntax-case stx ()
      ([_ (id arg ...) doc mpd_name creator        ]
           #'(define*-public (id client arg ...)
               doc
               (let ([cmd_string (creator (syntax->datum #'mpd_name) arg ...)])
                 (send-command client cmd_string))))
      ([_ (id arg ...) doc mpd_name creator handler]
           #'(define*-public (id client arg ...)
               doc
               (let ([cmd_string (creator (syntax->datum #'mpd_name) arg ...)])
                 (send-command client cmd_string handler)))))))



;; Status Commands

(mpd-define (mpdStatus::clear-error!)
            "Clears the current error message in status (this is also accomplished by any command that starts playback)."

            "clearerror"
            bind-all-arguments-to-one-string)


(mpd-define (mpdStatus::current-song)
            "Displays the song info of the current song (same song that is identified in status)."
            
            "currentsong"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(define-public (mpdStatus::idle client . subsystems)
  "idle [SUBSYSTEMS...]

Waits until there is a noteworthy change in one or more of MPD's subsystems (introduced with MPD 0.14). As soon as there is one, it lists all changed systems in a line in the format changed: SUBSYSTEM, where SUBSYSTEM is one of the following:

 * database       : the song database has been modified after update.
 * update         : a database update has started or finished. If the database was modified during the update, the database event is also emitted.
 * stored_playlist: a stored playlist has been modified, renamed, created or deleted
 * playlist       : the current playlist has been modified
 * player         : the player has been started, stopped or seeked
 * mixer          : the volume has been changed
 * output         : an audio output has been enabled or disabled
 * options        : options like repeat, random, crossfade, replay gain
 * sticker        : the sticker database has been modified.
 * subscription   : a client has subscribed or unsubscribed to a channel
 * message        : a message was received on a channel this client is subscribed to; this event is only emitted when the queue is empty

While a client is waiting for idle results, the server disables timeouts, allowing a client to wait for events as long as mpd runs. The idle command can be canceled by sending the command noidle (no other commands are allowed). MPD will then leave idle mode and print results immediately; might be empty at this time.

If the optional SUBSYSTEMS argument is used, MPD will only send notifications when something changed in one of the specified subsytems."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "idle"
      (filter/convert-strings/nums subsystems))
    mpdHandlers::general))


(mpd-define (mpdStatus::status)
            "Reports the current status of the player and the volume level.

 * volume        : 0-100
 * repeat        : 0 or 1
 * random        : 0 or 1
 * single        : 0 or 1
                     (introduced with MPD 0.15)
 * consume       : 0 or 1
                     (introduced with MPD 0.15)
 * playlist      : 31-bit unsigned integer, the playlist version number
 * playlistlength: integer, the length of the playlist
 * state         : play, stop, or pause
 * song          : playlist song number of the current song stopped on or playing
 * songid        : playlist songid of the current song stopped on or playing
 * nextsong      : playlist song number of the next song to be played
                     (introduced with MPD 0.15)
 * nextsongid    : playlist songid of the next song to be played
                     (introduced with MPD 0.15)
 * time          : total time elapsed (of current playing/paused song)
 * elapsed       : Total time elapsed within the current song, but with higher resolution.
                     (introduced with MPD 0.16)
 * duration      : Duration of the current song in seconds.
                     (introduced with MPD 0.20)
 * bitrate       : instantaneous bitrate in kbps
 * xfade         : crossfade in seconds
 * mixrampdb     : mixramp threshold in dB
 * mixrampdelay  : mixrampdelay in seconds
 * audio         : sampleRate:bits:channels
 * updating_db   : job id
 * error         : if there is an error, returns message here"

            "status"
            bind-all-arguments-to-one-string
            (mpdHandlers::convert-states '(repeat random single consume)))


(mpd-define (mpdStatus::stats)
            "Displays statistics.

 * artists    : number of artists
 * albums     : number of albums
 * songs      : number of songs
 * uptime     : daemon uptime in seconds
 * db_playtime: sum of all song times in the db
 * db_update  : last db update in UNIX time
 * playtime   : time length of music played"

            "stats"
            bind-all-arguments-to-one-string
            mpdHandlers::general)



;; Playback options

(mpd-define (mpdPlaybackOption::consume!           state)
            "consume {STATE}

Sets consume state to STATE (introduced with MPD 0.15); STATE should be 0 or 1. When consume is activated, each song played is removed from playlist."

            "consume"
            convert-state-and-bind-to-command)


(mpd-define (mpdPlaybackOption::crossfade!         seconds)
            "crossfade {SECONDS}

Sets crossfading between songs"

            "crossfade"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackOption::mix-ramp-db!       decibels)
            "mixrampdb {deciBels}

Sets the threshold at which songs will be overlapped. Like crossfading but doesn't fade the track volume, just overlaps. The songs need to have MixRamp tags added by an external tool. 0dB is the normalized maximum volume so use negative values; I prefer -17dB. In the absence of mixramp tags crossfading will be used. See http://sourceforge.net/projects/mixramp"

            "mixrampdb"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackOption::mix-ramp-delay!    seconds)
            "mixrampdelay {SECONDS}

Additional time subtracted from the overlap calculated by mixrampdb. A value of \"nan\" disables MixRamp overlapping and falls back to crossfading."

            "mixrampdelay"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackOption::random!            state)
            "random {STATE}

Sets random state to STATE; STATE should be 0 or 1."

            "random"
            convert-state-and-bind-to-command)


(mpd-define (mpdPlaybackOption::repeat!            state)
            "repeat {STATE}

Sets repeat state to STATE; STATE should be 0 or 1."

            "repeat"
            convert-state-and-bind-to-command)


(mpd-define (mpdPlaybackOption::set-vol!           vol)
            "setvol {VOL}

Sets volume to VOL; the range of volume is 0-100."

            "setvol"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackOption::single!            state)
            "single {STATE}

Sets single state to STATE (introduced with MPD 0.15); STATE should be 0 or 1. When single is activated, playback is stopped after current song or song is repeated, if the 'repeat' mode is enabled."

            "single"
            convert-state-and-bind-to-command)


(mpd-define (mpdPlaybackOption::replay-gain-mode!  mode)
            "replay_gain_mode {MODE}

Sets the replay gain mode. One of:
 * off
 * track
 * album
 * auto (added in MPD 0.16)

Changing the mode during playback may take several seconds, because the new settings does not affect the buffered data.

This command triggers the options idle event."
            
            "replay_gain_mode"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackOption::replay-gain-status )
            "Prints replay gain options. Currently, only the variable replay_gain_mode is returned."
            
            "replay_gain_status"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


;; volume not included since it is depreciated



;; Controlling Playback

(mpd-define (mpdPlaybackControl::play         #:optional song_pos)
            "play [SONGPOS]

Begins playing the playlist at song number SONG-POS"

            "play"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackControl::play-id      song_id)
            "playid [SONGID]

Beings playing the playlist at song SONG-ID"

            "playid"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackControl::stop         )
            "Stops playing"

            "stop"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackControl::pause        state)
            "pause {PAUSE}

Toggles pause/resumes playing; STATE is 0 or 1"

            "pause"
            convert-state-and-bind-to-command)


(mpd-define (mpdPlaybackControl::next         )
            "Plays next song in the playlist"

            "next"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackControl::previous     )
            "Plays previous song in the playlist"

            "previous"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackControl::seek         song_pos time)
            "seek {SONGPOS} {TIME}

Seeks to position TIME (in seconds) of entry SONG-POS in the playlist"

            "seek"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackControl::seek-id      song_id time)
            "seekid {SONGID} {TIME}

Seeks to position TIME (in seconds) of SONG-ID"

            "seekid"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaybackControl::seek-current time)
            "seekcur {TIME}

Seeks to the position TIME within the current song.  If passed a string prefixed with +/-, the time is relative to the current playing position"

            "seekcur"
            bind-all-arguments-to-one-string)



;; The Current Playlist

(mpd-define (mpdPlaylistCurrent::add!                    uri)
            "add {URI}

Adds the file URI to the playlist (directories add recursively). URI can also be a single file."

            "add"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistCurrent::add-id!                 uri #:optional
                                                               position)
            "addid {URI} [POSITION]

Adds a song to the playlist (non-recursive) and returns the song id.

URI is always a single file or URL. For example:

|> addid \"foo.mp3\"
|   Id: 999
|   OK"

            "addid"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdPlaylistCurrent::clear!                  )
            "Clears the current playlist."

            "clear"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistCurrent::delete!                 #:optional
                                                           pos/start end)
            "delete [{POS} | {START:END}]

Deletes a song from the playlist.

Warning: a range seems to delete [START, END)."

            "delete"
            (lambda (command . l)
              (let* ([p/s (number?->string  (cadr l))]
                     [  e (number?->string (caddr l))])
                (string-join
                  (cons command (if p/s (if e (list (string-append p/s ":" e))
                                          (list p/s)) '()))
                  " "))))


(mpd-define (mpdPlaylistCurrent::delete-id!              song_id)
            "deleteid {SONGID}

Deletes the song SONGID from the playlist"

            "deleteid"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistCurrent::move!                   to #:optional
                                                              from/start end)
            "move [{FROM} | {START:END}] {TO}

Moves the song at FROM or range of songs at START:END to TO in the playlist (ranges are supported since MPD 0.15).

Warning: a range seems to move [START, END)."

            "move"
            (lambda (command . l)
              (let* ([  t (number?->string    (car l))]
                     [f/s (number?->string  (caddr l))]
                     [  e (number?->string (cadddr l))])
                (string-join
                  (cons command (append (if f/s
                                            (if e
                                                (list (string-append f/s ":" e))
                                              (list f/s))
                                          '()) (list t)))
                  " "))))


(mpd-define (mpdPlaylistCurrent::move-id!                from to)
            "moveid {FROM} {TO}

Moves the song with FROM (songid) to TO (playlist index) in the playlist. If TO is negative, it is relative to the current song in the playlist (if there is one)."

            "moveid"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistCurrent::playlist-find           tag needle)
            "playlistfind {TAG} {NEEDLE}

Finds songs in the current playlist with strict matching."

            "playlistfind"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(file)))


(mpd-define (mpdPlaylistCurrent::playlist-id             #:optional song_id)
            "playlistid [SONGID]

Displays a list of songs in the playlist. SONGID is optional and specifies a single song to display info for."

            "playlistid"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(file)))


(mpd-define (mpdPlaylistCurrent::playlist-info           #:optional
                                                           song_pos/start end)
            "playlistinfo [{SONGPOS} | {START:END}]

Displays a list of all songs in the playlist or, if the optional argument is given, displays information only for the song SONGPOS or the range of songs START:END (ranges are supported since MPD 0.15).

This function returns a list of association lists, each a-list representing a single file.

Warning: a range seems to consist of [START, END)."

            "playlistinfo"
            (lambda (command . l)
              (let ([p/s (number?->string  (cadr l))]
                    [  e (number?->string (caddr l))])
                (string-join
                  (cons command (if p/s (if e (list (string-append p/s ":" e))
                                          (list p/s)) '()))
                  " ")))
            (mpdHandlers::parse-files '(file)))


(mpd-define (mpdPlaylistCurrent::playlist-search         tag needle)
            "playlistsearch {TAG} {NEEDLE}

Searches case-insensitively for partial matches in the current playlist."

            "playlistsearch"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(file)))


(mpd-define (mpdPlaylistCurrent::playlist-changes        version)
            "plchanges {VERSION}

Displays changed songs currently in the playlist since VERSION.

To detect songs that were deleted at the end of the playlist, use playlistlength returned by status command."

            "plchanges"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(file)))


(mpd-define (mpdPlaylistCurrent::playlist-changes-pos-id version)
            "plchangesposid {VERSION}

Displays changed songs currently in the playlist since VERSION. This function only returns the position and the id of the changed song, not the complete metadata. This is more bandwidth efficient.

To detect songs that were deleted at the end of the playlist, use playlistlength returned by status command."

            "plchangesposid"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(cpos)))


(define*-public (mpdPlaylistCurrent::priority!           client priority
                                                         start . ranges)
  "prio {PRIORITY} {START:END...}

Set the priority of the specified songs. A higher priority means that it will be played first when \"random\" mode is enabled.

A priority is an integer between 0 and 255. The default priority of new songs is 0.

Ranges can be passed as strings (e.g. \"1:4\") or single integers as string or numbers (e.g. 1 or \"1\") so long as two values are passed to make a range of, for the latter (e.g. (mpdStatus::priority! client 0 1 2 \"4:7\" 8 \"10\"))."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "prio"
      (create-ranges-from-list (cons start ranges))
      priority)))


(define-public (mpdPlaylistCurrent::priority-id!         client priority
                                                         id . ids)
  "prioid {PRIORITY} {ID...}

Same as prio, but address the songs with their id."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "prioid"
      priority
      id
      (filter/convert-strings/nums ids))))


(mpd-define (mpdPlaylistCurrent::range-id!               id start #:optional
                                                                    end)
            "rangeid {ID} {START:END}

Specifies the portion of the song that shall be played (since MPD 0.19). START and END are offsets in seconds (fractional seconds allowed); both are optional. Omitting both (i.e. sending just \":\") means \"remove the range, play everything\". A song that is currently playing cannot be manipulated this way."

            "rangeid"
            (lambda (command . l)
              (let ([i (number?->string    (car l))]
                    [s (number?->string   (cadr l))]
                    [e (number?->string (cadddr l))])
                (string-join            ;; Potentially only need start
                  (cons command (cons i (if e (list (string-append s ":" e))
                                          (list s))))
                  " "))))


(mpd-define (mpdPlaylistCurrent::shuffle!                #:optional start end)
            "shuffle [START:END]

Finds songs in the current playlist with strict matching. Shuffles the current playlist. START:END is optional and specifies a range of songs."

            "shuffle"
            (lambda (command . l)
              (let ([s (number?->string  (cadr l))]
                    [e (number?->string (caddr l))])
                (string-join
                  (cons command (if s (if e (list (string-append s ":" e))
                                        (list s)) '()))
                  " "))))


(mpd-define (mpdPlaylistCurrent::swap!                   song1 song2)
            "swap {SONG1} {SONG2}

Swaps the positions of SONG1 and SONG2."

            "swap"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistCurrent::swap-id!                song1 song2)
            "swapid {SONG1} {SONG2}

Swaps the positions of SONG1 and SONG2 (both song ids)."

            "swapid"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistCurrent::add-tag-id!             song_id tag value)
            "addtagid {SONGID} {TAG} {VALUE}

Adds a tag to the specified song. Editing song tags is only possible for remote songs. This change is volatile: it may be overwritten by tags received from the server, and the data is gone when the song gets removed from the queue."

            "addtagid"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistCurrent::clear-tag-id!           song_id #:optional
                                                                   tag)
            "cleartagid {SONGID} [TAG]

Removes tags from the specified song. If TAG is not specified, then all tag values will be removed. Editing song tags is only possible for remote songs."

            "cleartagid"
            bind-all-arguments-to-one-string)



;; Stored Playlists

(mpd-define (mpdPlaylistsStored::list-playlist      name)
            "listplaylist {NAME}

Lists the songs in the playlist. Playlist plugins are supported."

            "listplaylist"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdPlaylistsStored::list-playlist-info name)
            "listplaylistinfo {NAME}

Lists the songs with metadata in the playlist. Playlist plugins are supported."

            "listplaylistinfo"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(file)))


(mpd-define (mpdPlaylistsStored::list-playlists     )
            "Prints a list of the playlist directory.

After each playlist name the server sends its last modification time as attribute \"Last-Modified\" in ISO 8601 format. To avoid problems due to clock differences between clients and the server, clients should not compare this value with their local clock."

            "listplaylists"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(playlist)))


(mpd-define (mpdPlaylistsStored::load!              name #:optional start end)
            "load {NAME} [START:END]

Loads the playlist into the current queue. Playlist plugins are supported. A range may be specified to load only a part of the playlist."

            "load"
            (lambda (command . l)
              (let ([n (number?->string    (car l))]
                    [s (number?->string  (caddr l))]
                    [e (number?->string (cadddr l))])
                (string-join
                  (cons command (cons n (if s (if e (list
                                                      (string-append s ":" e))
                                                (list s)) '())))
                  " "))))


(mpd-define (mpdPlaylistsStored::playlist-add!      name uri)
            "playlistadd {NAME} {URI}

Adds URI to the playlist NAME.m3u.

NAME.m3u will be created if it does not exist."

            "playlistadd"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistsStored::playlist-clear!    name)
            "playlistclear {NAME}

Clears the playlist NAME.m3u."

            "playlistclear"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistsStored::playlist-delete!   name song_pos)
            "playlistdelete {NAME} {SONGPOS}

Deletes SONGPOS from the playlist NAME.m3u."

            "playlistdelete"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistsStored::playlist-move!     name song_id song_pos)
            "playlistmove {NAME} {SONGID} {SONGPOS}

Moves SONGID in the playlist NAME.m3u to the position SONGPOS.

Warning: MPD API says SONGID but I think they mean the original song position followed by the new song position."

            "playlistmove"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistsStored::rename!            name new_name)
            "rename {NAME} {NEW_NAME}

Renames the playlist NAME.m3u to NEW_NAME.m3u."

            "rename"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistsStored::remove!            name)
            "rm {NAME}

Removes the playlist NAME.m3u from the playlist directory."

            "rm"
            bind-all-arguments-to-one-string)


(mpd-define (mpdPlaylistsStored::save!              name)
            "save {NAME}

Saves the current playlist to NAME.m3u in the playlist directory."

            "save"
            bind-all-arguments-to-one-string)



;; The Music Database

(define-public (mpdDatabase::count                client tag needle . rest)
  "count {TAG} {NEEDLE} [...] [group] [GROUPTYPE]

Counts the number of songs and their total playtime in the db matching TAG exactly.

The group keyword may be used to group the results by a tag. The following prints per-artist counts:

|> count group artist

At the moment, – if you wish to specify a grouptype – you'll have to provide the \"group\" word yourself as the second-to-last argument to count."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "count"
      tag
      needle
      (filter/convert-strings/nums rest))
    mpdHandlers::general))


(define-public (mpdDatabase::find                 client type what . rest)
  "find {TYPE} {WHAT} [...] [window START:END]

Finds songs in the db that are exactly WHAT. TYPE can be any tag supported by MPD, or one of the special parameters:

 * any           : checks all tag values
 * file          : checks the full path (relative to the music directory)
 * base          : restricts the search to songs in the given directory (also relative to the music directory)
 * modified-since: compares the file's time stamp with the given value (ISO 8601 or UNIX time stamp)

WHAT is what to find.

window can be used to query only a portion of the real response. The parameter is two zero-based record numbers; a start number and an end number.

At the moment, – if you wish to specify a window range – you'll have to provide the \"window\" word yourself as the second-to-last argument to count.

A range can be passed as a string (e.g. \"1:4\") or single integers as string or numbers (e.g. 1 or \"1\") so long as two values are passed to make a range of, for the latter (e.g. (mpdDatabase::find client \"any\" \"Born to Run\" \"window\" 0 \"10\") or (mpdDatabase::find client \"any\" \"Born to Run\" \"window\" \"4:7\"))."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "find"
      type
      what
      (filter/convert-strings/nums
        (let ([end (member "window" rest)])  ; Finds if "window" was given in
          (if end  ; rest and then feed everything after "window" (which should
              (let* ([ranges        (cdr       end)]   ; be a range) to
                     [ranges_length (length ranges)])  ; create-ranges-from-list
                (append                                ; and then reappends
                  (list-head rest (- (length rest) ranges_length))
                  (if (= ranges_length 1)
                      (create-ranges-from-list ranges) 
                    (create-ranges-from-list (list-head ranges 2)))))
            rest))))
    (mpdHandlers::parse-files '(file))))


(define-public (mpdDatabase::find-add!            client type what . rest)
  "findadd {TYPE} {WHAT} [...]

Finds songs in the db that are exactly WHAT and adds them to current playlist. Parameters have the same meaning as for find."

  (send-command
    client
    (bind-all-arguments-to-one-string "findadd" type what (filter/convert-strings/nums rest))))


(define*-public (mpdDatabase::list                client type #:optional
                                                                filter_type
                                                                filter_what
                                                  . rest)
  "list {TYPE} [FILTERTYPE] [FILTERWHAT] [...] [group] [GROUPTYPE] [...]

Lists unique tags values of the specified type. TYPE can be any tag supported by MPD or file.

Additional arguments may specify a filter like the one in the find command.

The group keyword may be used (repeatedly) to group the results by one or more tags. The following example lists all album names, grouped by their respective (album) artist:

|> list album group albumartist

At the moment, – if you wish to specify a grouptype – you'll have to provide the \"group\" word yourself as the second-to-last argument to count."

  (send-command
    client
    (bind-all-arguments-to-one-string "list" type filter_type filter_what (filter/convert-strings/nums rest))
    mpdHandlers::general))


(mpd-define (mpdDatabase::list-all                #:optional uri)
            "listall [URI]

Lists all songs and directories in URI.

Do not use this command. Do not manage a client-side copy of MPD's database. That is fragile and adds huge overhead. It will break with large databases. Instead, query MPD whenever you need something.

The data returned is returned as a tree (comprised of lists), starting from the base directory (or specified directory). Each directory is its own list with the name of the directory as the first item (except for the base directory) and every song or other directory in said directory following as succeeding elements of the list."

            "listall"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-dirs (if uri uri "")))


(mpd-define (mpdDatabase::list-all-info           #:optional uri)
            "listallinfo [URI]

Same as listall, except it also returns metadata info in the same format as lsinfo.

Do not use this command. Do not manage a client-side copy of MPD's database. That is fragile and adds huge overhead. It will break with large databases. Instead, query MPD whenever you need something.

The data returned is returned as a tree (comprised of lists), starting from the base directory (or specified directory). Each directory is its own list with the name of the directory as the first item and the Last-Modified date as the second item (except for the base directory) and every song (as a list consisting of the song's details) or other directory in said directory following as succeeding elements of the list."

            "listallinfo"
            bind-all-arguments-to-one-string
            (lambda (resp)
              ((mpdHandlers::parse-files '(file))
                ((mpdHandlers::parse-dirs (if uri uri "")) resp))))


(mpd-define (mpdDatabase::list-files              #:optional uri)
            "listfiles [URI]

Lists the contents of the directory URI, including files are not recognized by MPD. URI can be a path relative to the music directory or an URI understood by one of the storage plugins. The response contains at least one line for each directory entry with the prefix \"file: \" or \"directory: \", and may be followed by file attributes such as \"Last-Modified\" and \"size\".

For example, \"smb://SERVER\" returns a list of all shares on the given SMB/CIFS server; \"nfs://servername/path\" obtains a directory listing from the NFS server."

            "listfiles"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(directory file)))


(mpd-define (mpdDatabase::ls-info                 #:optional uri)
            "lsinfo [URI]

Lists the contents of the directory URI.

When listing the root directory, this currently returns the list of stored playlists. This behavior is deprecated; use \"listplaylists\" instead.

This command may be used to list metadata of remote files (e.g. URI beginning with \"http://\" or \"smb://\").

Clients that are connected via UNIX domain socket may use this command to read the tags of an arbitrary local file (URI is an absolute path)."

            "lsinfo"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(directory)))


(mpd-define (mpdDatabase::read-comments           #:optional uri)
            "readcomments [URI]

Read \"comments\" (i.e. key-value pairs) from the file specified by \"URI\". This \"URI\" can be a path relative to the music directory or an absolute path.

This command may be used to list metadata of remote files (e.g. URI beginning with \"http://\" or \"smb://\").

The response consists of lines in the form \"KEY: VALUE\". Comments with suspicious characters (e.g. newlines) are ignored silently.

The meaning of these depends on the codec, and not all decoder plugins support it. For example, on Ogg files, this lists the Vorbis comments."

            "readcomments"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(define-public (mpdDatabase::search               client type what . rest)
  "search {TYPE} {WHAT} [...] [window START:END]

Searches for any song that contains WHAT. Parameters have the same meaning as for find, except that search is not case sensitive.

At the moment, – if you wish to specify a window range – you'll have to provide the \"window\" word yourself as the second-to-last argument to count.

A range can be passed as a string (e.g. \"1:4\") or single integers as string or numbers (e.g. 1 or \"1\") so long as two values are passed to make a range of, for the latter (e.g. (mpdDatabase::search client \"any\" \"Born to Run\" 0 \"10\") or (mpdDatabase::search client \"any\" \"Born to Run\" \"4:7\"))."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "search"
      type
      what
      (let ([end (member "window" rest)])  ; Finds if "window" was given in
	(if end  ; rest and then feed everything after "window" (which should
	    (let* ([ranges        (cdr       end)]   ; be a range) to
		   [ranges_length (length ranges)])  ; create-ranges-from-list
	      (append                                ; and then reappends
	        (list-head rest (- (length rest) ranges_length))
		(if (= ranges_length 1)
		    (create-ranges-from-list ranges)
                  (create-ranges-from-list (list-head ranges 2)))))
          rest)))
    (mpdHandlers::parse-files '(file))))


(define-public (mpdDatabase::search-add!          client type what . rest)
  "searchadd {TYPE} {WHAT} [...]

Searches for any song that contains WHAT in tag TYPE and adds them to current playlist.

Parameters have the same meaning as for find, except that search is not case sensitive."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "searchadd" type
      what        (filter/convert-strings/nums rest))))


(define-public (mpdDatabase::search-add-playlist! client name type what . rest)
  "searchaddpl {NAME} {TYPE} {WHAT} [...]

Searches for any song that contains WHAT in tag TYPE and adds them to the playlist named NAME.

If a playlist by that name doesn't exist it is created.

Parameters have the same meaning as for find, except that search is not case sensitive."

  (send-command
    client
    (bind-all-arguments-to-one-string
      "searchaddpl" 
      name
      type
      what
      (filter/convert-strings/nums rest))))


(mpd-define (mpdDatabase::update!                 #:optional uri)
            "update [URI]

Updates the music database: find new files, remove deleted files, update modified files.

URI is a particular directory or song/file to update. If you do not specify it, everything is updated.

Prints \"updating_db: JOBID\" where JOBID is a positive number identifying the update job. You can read the current job id in the status response."

            "update"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdDatabase::rescan!                 uri)
            "rescan [URI]

Same as update, but also rescans unmodified files."

            "rescan"
            bind-all-arguments-to-one-string
            mpdHandlers::general)



;; Mounts and neighbors

(mpd-define (mpdMounts::mount         path uri)
            "mount {PATH} {URI}

Mount the specified remote storage URI at the given path. Example:

|> mount foo nfs://192.168.1.4/export/mp3"

            "mount"
            bind-all-arguments-to-one-string)


(mpd-define (mpdMounts::unmount       path)
            "unmount {PATH}

Unmounts the specified path. Example:

|> unmount foo"

            "unmount"
            bind-all-arguments-to-one-string)


(mpd-define (mpdMounts::list-mounts   )
            "Queries a list of all mounts. By default, this contains just the configured music_directory. Example:

|> listmounts
|   mount: 
|   storage: /home/foo/music
|   mount: foo
|   storage: nfs://192.168.1.4/export/mp3
|   OK"

            "listmounts"
            bind-all-arguments-to-one-string
            (mpdHandlers::parse-files '(mount)))


(mpd-define (mpdMounts::list-neighbors)
            "Queries a list of \"neighbors\" (e.g. accessible file servers on the local net). Items on that list may be used with the mount command. Example:

|> listneighbors
|   neighbor: smb://FOO
|   name: FOO (Samba 4.1.11-Debian)
|   OK"

            "listneighbors"
            bind-all-arguments-to-one-string
            mpdHandlers::general)



;; Stickers

(mpd-define (mpdStickers::get     type uri name)
            "sticker get {TYPE} {URI} {NAME}

Reads a sticker value for the specified object."

            "sticker get"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdStickers::set!    type uri name value)
            "sticker set {TYPE} {URI} {NAME} {VALUE}

Adds a sticker value to the specified object. If a sticker item with that name already exists, it is replaced."

            "sticker set"
            bind-all-arguments-to-one-string)


(mpd-define (mpdStickers::delete! type uri #:optional name)
            "sticker delete {TYPE} {URI} [NAME]

Deletes a sticker value from the specified object. If you do not specify a sticker name, all sticker values are deleted."

            "sticker delete"
            bind-all-arguments-to-one-string)


(mpd-define (mpdStickers::list    type uri)
            "sticker list {TYPE} {URI}

Lists the stickers for the specified object."

            "sticker list"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdStickers::find    type uri name)
            "sticker find {TYPE} {URI} {NAME}

Searches the sticker database for stickers with the specified name, below the specified directory (URI). For each matching song, it prints the URI and that one sticker's value."

            "sticker find"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdStickers::find>=< type uri name op value)
            "sticker find {TYPE} {URI} {NAME} = {VALUE}

Searches for stickers with the given value.

Other supported operators are: \"<\", \">\""

            "sticker find"
            bind-all-arguments-to-one-string
            mpdHandlers::general)



;; Connection settings

(mpd-define (mpdConnection::close    )
            "Closes the connection to MPD. MPD will try to send the remaining output buffer before it actually closes the connection, but that cannot be guaranteed. This command will not generate a response."

            "close"
            bind-all-arguments-to-one-string)


(mpd-define (mpdConnection::kill!    )
            "Kills MPD."

            "kill"
            bind-all-arguments-to-one-string)


(mpd-define (mpdConnection::password password)
            "password {PASSWORD}

This is used for authentication with the server. PASSWORD is simply the plaintext password."

            "password"
            bind-all-arguments-to-one-string)


(mpd-define (mpdConnection::ping     )
            "Does nothing but return \"OK\"."

            "ping"
            bind-all-arguments-to-one-string
            mpdHandlers::general)



;; Audio output devices

(mpd-define (mpdAudioOutput::disable-output! id)
            "disableoutput {ID}

Turns an output off."

            "disableoutput"
            bind-all-arguments-to-one-string)


(mpd-define (mpdAudioOutput::enable-output!  id)
            "enableoutput {ID}

Turns an output on."

            "enableoutput"
            bind-all-arguments-to-one-string)


(mpd-define (mpdAudioOutput::toggle-output!  id)
            "toggleoutput {ID}

Turns an output on or off, depending on the current state."

            "toggleoutput"
            bind-all-arguments-to-one-string)


(mpd-define (mpdAudioOutput::outputs         )
            "Shows information about all outputs.

|   outputid: 0
|   outputname: My ALSA Device
|   outputenabled: 0
|   OK

Return information:

 * outputid     : ID of the output. May change between executions
 * outputname   : Name of the output. It can be any.
 * outputenabled: Status of the output. 0 if disabled, 1 if enabled."

            "outputs"
            bind-all-arguments-to-one-string
            (mpdHandlers::convert-states '(outputenabled)))



;; Reflection

(mpd-define (mpdAudioOutput::config      )
            "Dumps configuration values that may be interesting for the client. This command is only permitted to \"local\" clients (connected via UNIX domain socket).

The following response attributes are available:

 * music_directory: The absolute path of the music directory."

            "config"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdAudioOutput::commands    )
            "Shows which commands the current user has access to."

            "commands"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdAudioOutput::not-commands)
            "Shows which commands the current user does not have access to."

            "notcommands"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdAudioOutput::tag-types   )
            "Shows a list of available song metadata."

            "tagtypes"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdAudioOutput::url-handlers)
            "Gets a list of available URL handlers."

            "urlhandlers"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdAudioOutput::decoders    )
            "Print a list of decoder plugins, followed by their supported suffixes and MIME types. Example response:

|   plugin: mad
|   suffix: mp3
|   suffix: mp2
|   mime_type: audio/mpeg
|   plugin: mpcdec
|   suffix: mpc"

            "decoders"
            bind-all-arguments-to-one-string
            mpdHandlers::general)



;; Client to client

(mpd-define (mpdAudioOutput::subscribe    name)
            "subscribe {NAME}

Subscribe to a channel. The channel is created if it does not exist already. The name may consist of alphanumeric ASCII characters plus underscore, dash, dot and colon."

            "subscribe"
            bind-all-arguments-to-one-string)


(mpd-define (mpdAudioOutput::unsubscribe  name)
            "unsubscribe {NAME}

Unsubscribe from a channel."

            "unsubscribe"
            bind-all-arguments-to-one-string)


(mpd-define (mpdAudioOutput::channels     )
            "Obtain a list of all channels. The response is a list of \"channel:\" lines."

            "channels"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdAudioOutput::read-messages)
            "Reads messages for this client. The response is a list of \"channel:\" and \"message:\" lines."

            "readmessages"
            bind-all-arguments-to-one-string
            mpdHandlers::general)


(mpd-define (mpdAudioOutput::send-message channel text)
            "sendmessage {CHANNEL} {TEXT}

Send a message to the specified channel."

            "sendmessage"
            bind-all-arguments-to-one-string)
