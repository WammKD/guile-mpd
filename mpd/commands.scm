(define-module (mpd commands)
  :use-module (mpd client)
  :use-module (mpd utils)
  :use-module (ice-9 regex)
  :use-module (ice-9 optargs))

(define-syntax mpd-define
  (lambda (stx)
    (syntax-case stx ()
      ([_ (id arg ...) doc mpd-name handler]
       #'(define*-public (id client arg ...)
           doc
           (let ([cmd-string (string-join
                               (map
                                 (lambda (v)
                                   (if (number? v)
                                       (number->string v)
                                     v))
                                 (filter
                                   (lambda (v)
                                     (or (string? v) (number? v)))
                                   (list (syntax->datum #'mpd-name) arg ...)))
                               " ")])
             (send-command client cmd-string handler))))
      ([_ (id arg ...) doc mpd-name]
       #'(define*-public (id client arg ...)
           doc
           (let ([cmd-string (string-join
                               (map
                                 (lambda (v)
                                   (if (number? v)
                                       (number->string v)
                                     v))
                                 (filter
                                   (lambda (v)
                                     (or (string? v) (number? v)))
                                   (list (syntax->datum #'mpd-name) arg ...)))
                               " ")])
             (send-command client cmd-string)))))))

;; Status Commands

(mpd-define (status)
            "Query MPD's Status"
            "status"
            (lambda (resp)
              resp))

;; Controlling Playback

(mpd-define (play #:optional song-pos)
            "Begins playing the playlist at song number SONG-POS")

(mpd-define (play-id song-id)
            "Beings playing the playlist at song SONG-ID")

(mpd-define (stop)
            "Stops playing")

(mpd-define (pause state)
            "Toggles pause/resumes playing, STATE is 0 or 1")


(mpd-define (next)
            "Plays next song in the playlist")

(mpd-define (previous)
            "Plays previous song in the playlist")

(mpd-define (seek song-pos time)
            "Seeks to position TIME (in seconds) of entry SONG-POS in the playlist")

(mpd-define (seek-id song-id time)
            "Seeks to position TIME (in seconds) of SONG-ID")

(mpd-define (seek-current time)
            "Seeks to the position TIME within the current song.  If passed a string prefixed with +/-, the time is relative to the current playing position")

;; Playback options

(mpd-define (consume! state)
            "Sets consume state to STATE.  STATE should be 0 or 1.  When consume is activated, each song played is removed from playlist.")

(mpd-define (crossfade! seconds)
            "Sets crossfading between songs")

(mpd-define (mix-ramp-db! decibels)
            "Sets the threshold at which songs will be overlapped. Like crossfading but doesn't fade the track volume, just overlaps. The songs need to have MixRamp tags added by an external tool. 0dB is the normalized maximum volume so use negative values, I prefer -17dB. In the absence of mixramp tags crossfading will be used. See http://sourceforge.net/projects/mixramp")

(mpd-define (mix-ramp-delay! seconds)
            "Additional time subtracted from the overlap calculated by mixrampdb. A value of 'nan' disables MixRamp overlapping and falls back to crossfading.")

(mpd-define (random! state)
            "Sets random state to STATE, STATE should be 0 or 1.")

(mpd-define (repeat! state)
            "Sets repeat state to STATE, STATE should be 0 or 1.")

(mpd-define (set-vol! vol)
            "Sets volume to VOL, the range of volume is 0-100.")

(mpd-define (single! state)
            "Sets single state to STATE, STATE should be 0 or 1. When single is activated, playback is stopped after current song, or song is repeated if the 'repeat' mode is enabled.")

(mpd-define (replay-gain-mode! mode)
            "Sets the replay gain mode. One of off, track, album, auto.
Changing the mode during playback may take several seconds, because the new settings does not affect the buffered data.

This command triggers the options idle event.")

(mpd-define (replay-gain-status)
            "Prints replay gain options. Currently, only the variable replay_gain_mode is returned.")

;; The Music Database

(mpd-define (count #:optional tag needle)
            "Counts the number of songs and their total playtime in the db matching tag exactly."
            "count"
            (lambda (resp)
              resp))
