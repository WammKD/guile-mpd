(define-module (mpd commands)
  :use-module (mpd client)
  :use-module (mpd utils)
  :use-module (ice-9 regex)
  :use-module (ice-9 optargs))

(define-syntax mpd-define
  (lambda (stx)
    (syntax-case stx ()
      ([_ (id arg ...) doc mpd_name handler]
       #'(define*-public (id client arg ...)
           doc
           (let ([cmd_string (string-join
                               (map
                                 (lambda (v)
                                   (if (number? v)
                                       (number->string v)
                                     v))
                                 (filter
                                   (lambda (v)
                                     (or (string? v) (number? v)))
                                   (list (syntax->datum #'mpd_name) arg ...)))
                               " ")])
             (send-command client cmd_string handler))))
      ([_ (id arg ...) doc mpd_name]
       #'(define*-public (id client arg ...)
           doc
           (let ([cmd_string (string-join
                               (map
                                 (lambda (v)
                                   (if (number? v)
                                       (number->string v)
                                     v))
                                 (filter
                                   (lambda (v)
                                     (or (string? v) (number? v)))
                                   (list (syntax->datum #'mpd_name) arg ...)))
                               " ")])
             (send-command client cmd_string)))))))



;; Status Commands

(mpd-define (mpdStatus::clear-error!)
            "Clears the current error message in status (this is also accomplished by any command that starts playback)."

            "clearerror")


(mpd-define (mpdStatus::current-song)
            "Displays the song info of the current song (same song that is identified in status)."
            
            "currentsong"
            (lambda (resp)
              resp))


(define*-public (mpdStatus::idle client . subsystems)
  "Waits until there is a noteworthy change in one or more of MPD's subsystems (introduced with MPD 0.14). As soon as there is one, it lists all changed systems in a line in the format changed: SUBSYSTEM, where SUBSYSTEM is one of the following:

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
    (string-join (cons "idle" (map (lambda (v)
                                     (if (number? v)
                                         (number->string v)
                                       v)) (filter
                                             (lambda (v)
                                               (or (string? v) (number? v)))
                                             subsystems))) " ")
      (lambda (resp)
        resp)))


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
            (lambda (resp)
              resp))


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
            (lambda (resp)
              resp))



;; Playback options

(mpd-define (mpdPlaybackOption::consume!           state)
            "Sets consume state to STATE (ntroduced with MPD 0.15); STATE should be 0 or 1. When consume is activated, each song played is removed from playlist."

            "consume")


(mpd-define (mpdPlaybackOption::crossfade!         seconds)
            "Sets crossfading between songs"

            "crossfade")


(mpd-define (mpdPlaybackOption::mix-ramp-db!       decibels)
            "Sets the threshold at which songs will be overlapped. Like crossfading but doesn't fade the track volume, just overlaps. The songs need to have MixRamp tags added by an external tool. 0dB is the normalized maximum volume so use negative values; I prefer -17dB. In the absence of mixramp tags crossfading will be used. See http://sourceforge.net/projects/mixramp"

            "mixrampdb")


(mpd-define (mpdPlaybackOption::mix-ramp-delay!    seconds)
            "Additional time subtracted from the overlap calculated by mixrampdb. A value of \"nan\" disables MixRamp overlapping and falls back to crossfading."

            "mixrampdelay")


(mpd-define (mpdPlaybackOption::random!            state)
            "Sets random state to STATE; STATE should be 0 or 1."

            "random")


(mpd-define (mpdPlaybackOption::repeat!            state)
            "Sets repeat state to STATE; STATE should be 0 or 1."

            "repeat")


(mpd-define (mpdPlaybackOption::set-vol!           vol)
            "Sets volume to VOL; the range of volume is 0-100."

            "setvol")


(mpd-define (mpdPlaybackOption::single!            state)
            "Sets single state to STATE (introduced with MPD 0.15); STATE should be 0 or 1. When single is activated, playback is stopped after current song or song is repeated, if the 'repeat' mode is enabled."

            "single")


(mpd-define (mpdPlaybackOption::replay-gain-mode!  mode)
            "Sets the replay gain mode. One of:
 * off
 * track
 * album
 * auto (added in MPD 0.16)

Changing the mode during playback may take several seconds, because the new settings does not affect the buffered data.

This command triggers the options idle event."
            
            "replay_gain_mode")


(mpd-define (mpdPlaybackOption::replay-gain-status )
            "Prints replay gain options. Currently, only the variable replay_gain_mode is returned."
            
            "replay_gain_status")


;; volume not included since it is depreciated



;; Controlling Playback

(mpd-define (mpdPlaybackControl::play         #:optional song_pos)
            "Begins playing the playlist at song number SONG-POS"

            "play")


(mpd-define (mpdPlaybackControl::play-id      song_id)
            "Beings playing the playlist at song SONG-ID"

            "playid")


(mpd-define (mpdPlaybackControl::stop         )
            "Stops playing"

            "stop")


(mpd-define (mpdPlaybackControl::pause        state)
            "Toggles pause/resumes playing; STATE is 0 or 1"

            "pause")


(mpd-define (mpdPlaybackControl::next         )
            "Plays next song in the playlist"

            "next")


(mpd-define (mpdPlaybackControl::previous     )
            "Plays previous song in the playlist"

            "previous")


(mpd-define (mpdPlaybackControl::seek         song_pos time)
            "Seeks to position TIME (in seconds) of entry SONG-POS in the playlist"

            "seek")


(mpd-define (mpdPlaybackControl::seek-id      song_id time)
            "Seeks to position TIME (in seconds) of SONG-ID"

            "seekid")


(mpd-define (mpdPlaybackControl::seek-current time)
            "Seeks to the position TIME within the current song.  If passed a string prefixed with +/-, the time is relative to the current playing position"

            "seekcur")



;; The Current Playlist

(mpd-define (mpdPlaylistCurrent::add!                    uri)
            "Adds the file URI to the playlist (directories add recursively). URI can also be a single file."

            "add")


(mpd-define (mpdPlaylistCurrent::add-id!                 uri #:optional
                                                               position)
            "Adds a song to the playlist (non-recursive) and returns the song id.

URI is always a single file or URL. For example:

addid \"foo.mp3\"
Id: 999
OK"

            "addid"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistCurrent::clear!                  )
            "Clears the current playlist."

            "clear")


(mpd-define (mpdPlaylistCurrent::delete!                 #:optional
                                                           pos/start_end)
            "Deletes a song from the playlist."

            "delete")


(mpd-define (mpdPlaylistCurrent::delete-id!              song_id)
            "Deletes the song SONGID from the playlist"

            "deleteid")


(mpd-define (mpdPlaylistCurrent::move!                   to #:optional
                                                              from/start_end)
            "Moves the song at FROM or range of songs at START:END to TO in the playlist (ranges are supported since MPD 0.15)."

            "move")


(mpd-define (mpdPlaylistCurrent::move-id!                from to)
            "Moves the song with FROM (songid) to TO (playlist index) in the playlist. If TO is negative, it is relative to the current song in the playlist (if there is one)."

            "moveid")


(mpd-define (mpdPlaylistCurrent::playlist-find           tag needle)
            "Finds songs in the current playlist with strict matching."

            "playlistfind")


(mpd-define (mpdPlaylistCurrent::playlist-id             song_id)
            "Displays a list of songs in the playlist. SONGID is optional and specifies a single song to display info for."

            "playlistid"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistCurrent::playlist-info           #:optional
                                                           song_pos/start_end)
            "Displays a list of all songs in the playlist, or if the optional argument is given, displays information only for the song SONGPOS or the range of songs START:END (ranges are supported since MPD 0.15)."

            "playlistinfo"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistCurrent::playlist-search         tag needle)
            "Searches case-insensitively for partial matches in the current playlist."

            "playlistinfo"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistCurrent::playlist-changes        version)
            "Displays changed songs currently in the playlist since VERSION.

To detect songs that were deleted at the end of the playlist, use playlistlength returned by status command."

            "plchanges"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistCurrent::playlist-changes-pos-id version)
            "Displays changed songs currently in the playlist since VERSION. This function only returns the position and the id of the changed song, not the complete metadata. This is more bandwidth efficient.

To detect songs that were deleted at the end of the playlist, use playlistlength returned by status command."

            "plchangesposid"
            (lambda (resp)
              resp))


(define*-public (mpdStatus::priority!                    client priority
                                                         start_end . ranges)
  "Set the priority of the specified songs. A higher priority means that it will be played first when \"random\" mode is enabled.

A priority is an integer between 0 and 255. The default priority of new songs is 0."
  (send-command
    client
    (string-join (cons "prio" (map (lambda (v)
                                     (if (number? v)
                                         (number->string v)
                                       v)) (filter
                                             (lambda (v)
                                               (or (string? v) (number? v)))
                                             (cons priority (cons
                                                              start_end
                                                              ranges))))) " ")
    (lambda (resp)
      resp)))


(define*-public (mpdStatus::priority-id!                 client priority
                                                         id . ids)
  "Same as prio, but address the songs with their id."
  (send-command
    client
    (string-join
      (cons "prioid" (map (lambda (v)
                            (if (number? v)
                                (number->string v)
                              v)) (filter
                                    (lambda (v)
                                      (or (string? v) (number? v)))
                                    (cons priority (cons id ids)))))
      " ")
    (lambda (resp)
      resp)))


(mpd-define (mpdPlaylistCurrent::range-id!               id start_end)
            "Specifies the portion of the song that shall be played (since MPD 0.19). START and END are offsets in seconds (fractional seconds allowed); both are optional. Omitting both (i.e. sending just \":\") means \"remove the range, play everything\". A song that is currently playing cannot be manipulated this way."

            "rangeid")


(mpd-define (mpdPlaylistCurrent::shuffle!                #:optional start_end)
            "Finds songs in the current playlist with strict matching.Shuffles the current playlist. START:END is optional and specifies a range of songs."

            "shuffle")


(mpd-define (mpdPlaylistCurrent::swap!                   song1 song2)
            "Swaps the positions of SONG1 and SONG2."

            "swap")


(mpd-define (mpdPlaylistCurrent::swap-id!                song1 song2)
            "Swaps the positions of SONG1 and SONG2 (both song ids)."

            "swapid")


(mpd-define (mpdPlaylistCurrent::add-tag-id!             song_id tag value)
            "Adds a tag to the specified song. Editing song tags is only possible for remote songs. This change is volatile: it may be overwritten by tags received from the server, and the data is gone when the song gets removed from the queue."

            "addtagid")


(mpd-define (mpdPlaylistCurrent::clear-tag-id!           song_id tag)
            "Removes tags from the specified song. If TAG is not specified, then all tag values will be removed. Editing song tags is only possible for remote songs."

            "cleartagid")



;; Stored Playlists

(mpd-define (mpdPlaylistsStored::list-playlist      name)
            "Lists the songs in the playlist. Playlist plugins are supported."

            "listplaylist"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistsStored::list-playlist-info name)
            "Lists the songs with metadata in the playlist. Playlist plugins are supported."

            "listplaylistinfo"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistsStored::list-playlists     )
            "Prints a list of the playlist directory.

After each playlist name the server sends its last modification time as attribute \"Last-Modified\" in ISO 8601 format. To avoid problems due to clock differences between clients and the server, clients should not compare this value with their local clock."

            "listplaylists"
            (lambda (resp)
              resp))


(mpd-define (mpdPlaylistsStored::load!              name #:optional start_end)
            "Loads the playlist into the current queue. Playlist plugins are supported. A range may be specified to load only a part of the playlist."

            "load")


(mpd-define (mpdPlaylistsStored::playlist-add!      name uri)
            "Adds URI to the playlist NAME.m3u.

NAME.m3u will be created if it does not exist."

            "playlistadd")


(mpd-define (mpdPlaylistsStored::playlist-clear!    name)
            "Clears the playlist NAME.m3u."

            "playlistclear")


(mpd-define (mpdPlaylistsStored::playlist-delete!   name song_pos)
            "Deletes SONGPOS from the playlist NAME.m3u."

            "playlistdelete")


(mpd-define (mpdPlaylistsStored::playlist-move!     name song_id song_pos)
            "Moves SONGID in the playlist NAME.m3u to the position SONGPOS."

            "playlistmove")


(mpd-define (mpdPlaylistsStored::rename!            name new_name)
            "Renames the playlist NAME.m3u to NEW_NAME.m3u."

            "rename")


(mpd-define (mpdPlaylistsStored::remove!            name)
            "Removes the playlist NAME.m3u from the playlist directory."

            "rm")


(mpd-define (mpdPlaylistsStored::save!              name)
            "Saves the current playlist to NAME.m3u in the playlist directory."

            "save")



;; The Music Database

(mpd-define (mpdDatabase::count                tag needle #:optional
                                                            group group_type)
            "Counts the number of songs and their total playtime in the db matching TAG exactly.

The group keyword may be used to group the results by a tag. The following prints per-artist counts:

count group artist"

            "count"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::find                 type what #:optional
                                                           window start_end)
            "Finds songs in the db that are exactly WHAT. TYPE can be any tag supported by MPD, or one of the special parameters:

 * any           : checks all tag values
 * file          : checks the full path (relative to the music directory)
 * base          : restricts the search to songs in the given directory (also relative to the music directory)
 * modified-since: compares the file's time stamp with the given value (ISO 8601 or UNIX time stamp)

WHAT is what to find.

window can be used to query only a portion of the real response. The parameter is two zero-based record numbers; a start number and an end number."

            "find"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::find-add!            type what)
            "Finds songs in the db that are exactly WHAT and adds them to current playlist. Parameters have the same meaning as for find."

            "findadd")


(mpd-define (mpdDatabase::list                 type #:optional
                                                      filter_type filter_what
                                                      group       group_type)
            "Lists unique tags values of the specified type. TYPE can be any tag supported by MPD or file.

Additional arguments may specify a filter like the one in the find command.

The group keyword may be used (repeatedly) to group the results by one or more tags. The following example lists all album names, grouped by their respective (album) artist:

list album group albumartist"

            "list"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::list-all             #:optional uri)
            "Lists all songs and directories in URI.

Do not use this command. Do not manage a client-side copy of MPD's database. That is fragile and adds huge overhead. It will break with large databases. Instead, query MPD whenever you need something."

            "listall"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::list-all-info        #:optional uri)
            "Same as listall, except it also returns metadata info in the same format as lsinfo.

Do not use this command. Do not manage a client-side copy of MPD's database. That is fragile and adds huge overhead. It will break with large databases. Instead, query MPD whenever you need something."

            "listallinfo"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::list-files           #:optional uri)
            "Lists the contents of the directory URI, including files are not recognized by MPD. URI can be a path relative to the music directory or an URI understood by one of the storage plugins. The response contains at least one line for each directory entry with the prefix \"file: \" or \"directory: \", and may be followed by file attributes such as \"Last-Modified\" and \"size\".

For example, \"smb://SERVER\" returns a list of all shares on the given SMB/CIFS server; \"nfs://servername/path\" obtains a directory listing from the NFS server."

            "listfiles"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::ls-info              #:optional uri)
            "Lists the contents of the directory URI.

When listing the root directory, this currently returns the list of stored playlists. This behavior is deprecated; use \"listplaylists\" instead.

This command may be used to list metadata of remote files (e.g. URI beginning with \"http://\" or \"smb://\").

Clients that are connected via UNIX domain socket may use this command to read the tags of an arbitrary local file (URI is an absolute path)."

            "lsinfo"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::read-comments        #:optional uri)
            "Read \"comments\" (i.e. key-value pairs) from the file specified by \"URI\". This \"URI\" can be a path relative to the music directory or an absolute path.

This command may be used to list metadata of remote files (e.g. URI beginning with \"http://\" or \"smb://\").

The response consists of lines in the form \"KEY: VALUE\". Comments with suspicious characters (e.g. newlines) are ignored silently.

The meaning of these depends on the codec, and not all decoder plugins support it. For example, on Ogg files, this lists the Vorbis comments."

            "readcomments"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::search               type what #:optional
                                                           window start_end)
            "Searches for any song that contains WHAT. Parameters have the same meaning as for find, except that search is not case sensitive."

            "search"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::search-add!          type what)
            "Searches for any song that contains WHAT in tag TYPE and adds them to current playlist.

Parameters have the same meaning as for find, except that search is not case sensitive."

            "searchadd")


(mpd-define (mpdDatabase::search-add-playlist! name type what)
            "Searches for any song that contains WHAT in tag TYPE and adds them to the playlist named NAME.

If a playlist by that name doesn't exist it is created.

Parameters have the same meaning as for find, except that search is not case sensitive."

            "searchaddpl")


(mpd-define (mpdDatabase::update!              #:optional uri)
            "Updates the music database: find new files, remove deleted files, update modified files.

URI is a particular directory or song/file to update. If you do not specify it, everything is updated.

Prints \"updating_db: JOBID\" where JOBID is a positive number identifying the update job. You can read the current job id in the status response."

            "update"
            (lambda (resp)
              resp))


(mpd-define (mpdDatabase::rescan!              uri)
            "Same as update, but also rescans unmodified files."

            "rescan"
            (lambda (resp)
              resp))



;; Mounts and neighbors

(mpd-define (mpdMounts::mount         path uri)
            "Mount the specified remote storage URI at the given path. Example:

mount foo nfs://192.168.1.4/export/mp3"

            "mount")


(mpd-define (mpdMounts::unmount       path)
            "Unmounts the specified path. Example:

unmount foo"

            "unmount")


(mpd-define (mpdMounts::list-mounts   )
            "Queries a list of all mounts. By default, this contains just the configured music_directory. Example:

listmounts
mount: 
storage: /home/foo/music
mount: foo
storage: nfs://192.168.1.4/export/mp3
OK"

            "listmounts"
            (lambda (resp)
              resp))


(mpd-define (mpdMounts::list-neighbors)
            "Queries a list of \"neighbors\" (e.g. accessible file servers on the local net). Items on that list may be used with the mount command. Example:

listneighbors
neighbor: smb://FOO
name: FOO (Samba 4.1.11-Debian)
OK"

            "unmount"
            (lambda (resp)
              resp))



;; Stickers

(mpd-define (mpdStickers::get     type uri name)
            "Reads a sticker value for the specified object."

            "sticker get"
            (lambda (resp)
              resp))


(mpd-define (mpdStickers::set!    type uri name value)
            "Adds a sticker value to the specified object. If a sticker item with that name already exists, it is replaced."

            "sticker set")


(mpd-define (mpdStickers::delete! type uri #:optional name)
            "Deletes a sticker value from the specified object. If you do not specify a sticker name, all sticker values are deleted."

            "sticker delete")


(mpd-define (mpdStickers::list    type uri)
            "Lists the stickers for the specified object."

            "sticker list"
            (lambda (resp)
              resp))


(mpd-define (mpdStickers::find    type uri name)
            "Searches the sticker database for stickers with the specified name, below the specified directory (URI). For each matching song, it prints the URI and that one sticker's value."

            "sticker find"
            (lambda (resp)
              resp))


(mpd-define (mpdStickers::find>=< type uri name op value)
            "Searches for stickers with the given value.

Other supported operators are: \"<\", \">\""

            "sticker find"
            (lambda (resp)
              resp))



;; Connection settings

(mpd-define (mpdConnection::close    )
            "Closes the connection to MPD. MPD will try to send the remaining output buffer before it actually closes the connection, but that cannot be guaranteed. This command will not generate a response."

            "close")


(mpd-define (mpdConnection::kill!    )
            "Kills MPD."

            "kill")


(mpd-define (mpdConnection::password password)
            "This is used for authentication with the server. PASSWORD is simply the plaintext password."

            "password")


(mpd-define (mpdConnection::ping     )
            "Does nothing but return \"OK\"."

            "ping"
            (lambda (resp)
              resp))



;; Audio output devices

(mpd-define (mpdAudioOutput::disable-output! id)
            "Turns an output off."

            "disableoutput")


(mpd-define (mpdAudioOutput::enable-output!  id)
            "Turns an output on."

            "enableoutput")


(mpd-define (mpdAudioOutput::toggle-output!  id)
            "Turns an output on or off, depending on the current state."

            "toggleoutput")


(mpd-define (mpdAudioOutput::outputs         )
            "Shows information about all outputs.

outputid: 0
outputname: My ALSA Device
outputenabled: 0
OK

Return information:

 * outputid     : ID of the output. May change between executions
 * outputname   : Name of the output. It can be any.
 * outputenabled: Status of the output. 0 if disabled, 1 if enabled."

            "outputs"
            (lambda (resp)
              resp))



;; Reflection

(mpd-define (mpdAudioOutput::config      )
            "Dumps configuration values that may be interesting for the client. This command is only permitted to \"local\" clients (connected via UNIX domain socket).

The following response attributes are available:

 * music_directory: The absolute path of the music directory."

            "config"
            (lambda (resp)
              resp))


(mpd-define (mpdAudioOutput::commands    )
            "Shows which commands the current user has access to."

            "commands"
            (lambda (resp)
              resp))


(mpd-define (mpdAudioOutput::not-commands)
            "Shows which commands the current user does not have access to."

            "notcommands"
            (lambda (resp)
              resp))


(mpd-define (mpdAudioOutput::tag-types   )
            "Shows a list of available song metadata."

            "tagtypes"
            (lambda (resp)
              resp))


(mpd-define (mpdAudioOutput::url-handlers)
            "Gets a list of available URL handlers."

            "urlhandlers"
            (lambda (resp)
              resp))


(mpd-define (mpdAudioOutput::decoders    )
            "Print a list of decoder plugins, followed by their supported suffixes and MIME types. Example response:

plugin: mad
suffix: mp3
suffix: mp2
mime_type: audio/mpeg
plugin: mpcdec
suffix: mpc"

            "decoders"
            (lambda (resp)
              resp))



;; Client to client

(mpd-define (mpdAudioOutput::subscribe    name)
            "Subscribe to a channel. The channel is created if it does not exist already. The name may consist of alphanumeric ASCII characters plus underscore, dash, dot and colon."

            "subscribe")


(mpd-define (mpdAudioOutput::unsubscribe  name)
            "Unsubscribe from a channel."

            "unsubscribe")


(mpd-define (mpdAudioOutput::channels     )
            "Obtain a list of all channels. The response is a list of \"channel:\" lines."

            "channels"
            (lambda (resp)
              resp))


(mpd-define (mpdAudioOutput::read-messages)
            "Reads messages for this client. The response is a list of \"channel:\" and \"message:\" lines."

            "readmessages"
            (lambda (resp)
              resp))


(mpd-define (mpdAudioOutput::send-message channel text)
            "Send a message to the specified channel."

            "sendmessage")










