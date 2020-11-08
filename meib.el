;;; meib.el --- My Emacs I(RC) Bot (and sort-of client).

;;; Commentary:
;; The code is largely inspired (and in some parts copied) from
;; rcirc.el.
;; 
;; TODO:
;;
;; o Regex-matching fix for the commands.
;;
;; o Reconnecting to channels after being kicked.
;;
;; o Make an option to make the bot speak through /me. The format is
;;   ACTION <text>.

;;; Code:

(require 'meib-permission (concat (file-name-directory (buffer-file-name)) "meib-permission.el"))
(require 'meib-spook (concat (file-name-directory (buffer-file-name)) "meib-spook.el"))
(require 'meib-highlight (concat (file-name-directory (buffer-file-name)) "meib-highlight.el"))
(require 'meib-covid-19 (concat (file-name-directory (buffer-file-name)) "meib-covid-19.el"))
(require 'meib-quotes (concat (file-name-directory (buffer-file-name)) "meib-quotes.el"))
(require 'meib-love (concat (file-name-directory (buffer-file-name)) "meib-love.el"))

(defgroup meib nil
  "MEIB Emacs I(RC) Bot."
  :link '(variable-link meib-command-alist)
  :link '(variable-link meib-server-alist)
  :link '(url-link "https://github.com/emssej/meib.el")
  :version "26.3"
  :prefix "meib-"
  :group 'applications)

(defcustom meib-server-alist
  (quote (("irc.freenode.net" :port 7000
	   :use-tls t :nick-name "Meibel"
	   :user-name "meibel" :real-name "Maybelline"
	   :throttle 1.6 :truncate 419 :anti-kick 5
	   :ignored-users ("user")
	   :authorized-users ("Meibel")
	   :channels ("#emacs" "#vim"))))
  "An alist of IRC connections to establish when running `meib'.
Each element is of type (SERVER-NAME PARAMETERS).
Each optional parameter is of type (PARAMETER VALUE).

The following parameters are recognized:

`:port'

The port to be used to connect to the server.

`:use-tls'

Whether or not to use TLS when establishing the connection.

`:nick-name'

What nickname to use in the IRC server.

`:user-name'

What username (the one in your vhost) to use in the IRC
server.

`:real-name'

What realname (the one when you are /WHOIS-ed) to use in the IRC
server.

`:throttle'

Whether to throttle PRIVMSG's. This is an anti-flooding measure,
and also deters channel bots that kick if an user floods.

`:truncate' 

Whether to truncate PRIVMSG's, and by how many characters. If you
are also using `:anti-kick', the number of invisible characters
is subtracted from this value.

`:anti-kick'

Whether to apply anti-kick measures, i.e. apply a random number
of invisible characters to the beginning of every PRIVMSG.

`:ignored-users'

Users whose PRIVMSG's are ignored by the bot.

`:authorized-users'

Users authorized to use restricted
commands (`meib-restricted-command-alist').

`:channels'

What channels to connect to in the server."
  :group 'meib
  :type '(alist :key-type (string :tag "Address")
		:value-type (plist :options
				   ((:port (integer :tag "Port"))
				    (:use-tls (boolean :tag "Use TLS"))
				    (:nick-name (string :tag "Nickname"))
				    (:user-name (string :tag "Username"))
				    (:real-name (string :tag "Real name"))
				    (:throttle
				     (choice (number :tag "Throttle time (in seconds)")
					     (const :tag "Don't throttle" nil)))
				    (:truncate
				     (choice (integer :tag "Truncate lines to")
					     (const :tag "Don't truncate lines" nil)))
				    (:anti-kick
				     (choice (integer :tag "Random number of invisible characters in interval [0,n)")
					     (const :tag "Don't apply anti-kick measures" nil)))
				    (:ignored-users
				     (repeat :tag "Ignored users"
					     (string :tag "User")))
				    (:authorized-users
				     (repeat :tag "Users allowed to use authorized commands"
					     (string :tag "User")))
				    (:channels (repeat :tag "Channels" (string :tag "Channel")))))))

(defcustom meib-privmsg-callbacks
  '(meib-process-command)
  "Callbacks exceuted on a PRIVMSG command.
The callbacks take in the arguments (PROCESS MESSAGE ARGUMENTS).

PROCESS is an usual Emacs process, to send strings to it use
`meib-send-string', but it is suggested to use more high-level
functions such as `meib-privmsg'. Those commands use the server
queue, which prevents the bot from flooding.

MESSAGE is a plist of IRC message components, which is described
in detail in `meib-process-irc-message'.

ARGUMENTS is a list of strings, the arguments to the command."
  :group 'meib
  :type '(repeat (function :tag "Callback")))

(defcustom meib-command-alist
  '(("help" . meib-command-help)
    ("permission" . meib-permission)
    ("spook" . meib-spook)
    ("covid-19" . meib-covid-19)
    ("quotes" . meib-quotes)
    ("love" . meib-love))
  "The callbacks to commands processed by `meib-process-command'.
They are in the form of (COMMAND-NAME . FUNCTION).

FUNCTION takes in the arguments (PROCESS MESSAGE ARGUMENTS). For a
more in-depth explanation of MESSAGE, consult
`meib-privmsg-callbacks'. For an explanation of ARGUMENTS, consult
`meib-parse-command'."
  :group 'meib
  :type '(repeat (cons :tag "Command" (string :tag "Name") (function :tag "Callback"))))

(defcustom meib-restricted-command-alist
  '(("clear-queue" . meib-command-clear-queue)
    ("hilite" . meib-highlight))
  "The callbacks to commands processed by `meib-process-command'.
They are in the form of (COMMAND-NAME . FUNCTION).

This list contains commands restricted to certain users. For a more
in-depth description of the alist, see `meib-command-alist'."
  :group 'meib
  :type '(repeat (cons :tag "Command" (string :tag "Name") (function :tag "Callback"))))

(defcustom meib-command-regexp "¿\\(.+\\)\\?"
  "Regexp that matches a command.
For now, the only group should be the actual command, i.e. `help
arg1 arg2', which is then split by spaces."
  :group 'meib
  :type 'regexp)

(defcustom meib-restricted-command-regexp "¡\\(.+\\)\\!"
  "Regexp that matches a restricted command.
See `meib-command-regexp'."
  :group 'meib
  :type 'regexp)

(defvar meib-connected-server-alist nil
  "An alist of connected servers.
Also contains information about the server, i.e. the process,
buffer, the queue and queue timer (for each channel), the
nickname of the bot, the channels the bot is in, the usernames in
each channel, and so on.  The information is a plist in the
form (PARAMETER VALUE).")

(defun meib-command-clear-queue (process message arguments)
  "Clear the message queue."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (channel (car (plist-get message :arguments))))
    (plist-put connected-server-plist :queue nil)
    (meib-privmsg process channel "Cleared the message queue.")))

(defun meib-command-help (process message arguments)
  "Go through all commands and list them.
If an argument is specified, then show the specified command's
documentation."
  (let* ((channel (car (plist-get message :arguments)))
	 (arg1 (car arguments))
	 (command-function (cdr (assoc arg1 meib-command-alist)))
	 (restricted-command-function (cdr (assoc arg1 meib-restricted-command-alist)))
	 (command-list (mapconcat (lambda (command) (car command)) meib-command-alist " "))
	 (restricted-command-list (mapconcat (lambda (command) (concat "/!\\" (car command) "/!\\"))
					     meib-restricted-command-alist " "))
	 (documentation (when (or command-function restricted-command-function)
			  (replace-regexp-in-string
			   "\n" " " (documentation (or command-function restricted-command-function))))))
    (if (and arg1 (or command-function restricted-command-function))
	(meib-privmsg process channel (format "%s --- %s" arg1 documentation))
      (meib-privmsg process channel (format "Available commands: %s"
					    (concat command-list " " restricted-command-list))))))

(defun meib-privmsg (process receiver message)
  "Send a MESSAGE PRIVMSG to RECIEVER via PROCESS.
This function also truncates MESSAGE to the value set in
`meib-server-alist', and appends invisible characters at the
end (if it was set in `meib-server-alist').

If MESSAGE is longer than the truncate value, then send several
split messages."
  (let* ((address (plist-get (cdr (assoc process meib-connected-server-alist)) :address))
	 (server-plist (cdr (assoc-string address meib-server-alist)))
	 (throttle (plist-get server-plist :throttle))
	 (truncate (plist-get server-plist :truncate))
	 (anti-kick (plist-get server-plist :anti-kick))
	 (messages (meib-format-privmsg receiver message truncate anti-kick)))
    (if throttle
	(progn
	  (meib-send-string-queue process (car messages))
	  (when (cadr messages)
	    (meib-privmsg process receiver (cadr messages))))
      (meib-send-string process (car messages))
      (when (cadr messages)
	(meib-privmsg process receiver (cadr messages))))))

(defun meib-format-privmsg (receiver message &optional truncate anti-kick)
  "Format MESSAGE as a PRIVMSG.
This function applies all the needed operations specified in
`meib-server-alist', i.e. TRUNCATE and ANTIKICK, and also sets
the target as RECEIVER.

If MESSAGE is longer than TRUNCATE, also return the remaining
string, in the form (FORMATTED REMAINING)."
  (let* ((n (random anti-kick))
	 (truncate-width (if anti-kick (- truncate n) truncate))
	 (truncated (if truncate (truncate-string-to-width message truncate-width) message))
	 (rest (if (> (length message) truncate-width) (substring message truncate-width) nil)))
    (list
     (format "PRIVMSG %s :%s%s‍" receiver (if truncate truncated message) (if anti-kick (make-string n ?‌) ""))
     rest)))

;; TODO here:
;; o Handle QUIT and NICK commands nicely.
;; o Make it use font-lock so themes play nicely with it.
(defun meib-debug-print (process message)
  "Print MESSAGE to the PROCESS buffer (or channel buffer)."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (receiver (car (plist-get message :arguments)))
	 ;; A naive check to see if the argument is a channel, I guess.
	 (buffer (plist-get (cdr (assoc-string receiver (plist-get connected-server-plist :channels))) :buffer))
	 (inhibit-read-only t)
	 (command (plist-get message :command))
	 (sender (plist-get message :sender))
	 (arguments (plist-get message :arguments)))
    (when (or buffer (plist-get connected-server-plist :buffer))
      (with-current-buffer (or buffer (plist-get connected-server-plist :buffer))
	(goto-char (point-max))
	(when sender
	  (insert (propertize (meib-nick-name-from-full-name sender) 'face '(:foreground "blue" :weight bold)) " "))
	(when (not (string= command "PRIVMSG"))
	  (insert (propertize command 'face '(:weight bold)) " "))
	(when arguments
	  (dolist (argument (if buffer (cdr arguments) arguments))
	    (insert argument " ")))
	(newline)))))

(defun meib-process-command (process message)
  "Process MESSAGE and see if it contains a command.
If it does, execute the command callback corresponding to the
command."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (server-plist (cdr (assoc (plist-get connected-server-plist :address) meib-server-alist)))
	 (authorized-users (plist-get server-plist :authorized-users))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender)))
	 (channel-name (car (plist-get message :arguments)))
	 (command-with-args
	  (meib-parse-command (cadr (plist-get message :arguments)) meib-command-regexp))
	 (restricted-command-with-args
	  (meib-parse-command (cadr (plist-get message :arguments)) meib-restricted-command-regexp))
	 (command-function (cdr (assoc (car command-with-args) meib-command-alist)))
	 (restricted-command-function (cdr (assoc (car restricted-command-with-args) meib-restricted-command-alist))))
    (if command-function
	(funcall command-function process message (cdr command-with-args))
      (when restricted-command-function
	(if (member nick-name authorized-users)
	    (funcall restricted-command-function process message (cdr restricted-command-with-args))
	  (meib-privmsg process channel-name "You can't use this command."))))))

(defun meib-parse-command (input regexp)
  "Process INPUT and see if it has a command.
If it is, return (COMMAND . ARGUMENTS), otherwise return nil."
  ;; Make it use a real regex matching the command... soon...
  (when (string-match regexp input)
    (split-string (match-string 1 input) " " t)))
;; (let ((n-matches (1- (/ (length (match-data)) 2))))
;;   (mapcar (lambda (i) (match-string i input))
;; 			       (number-sequence 0 n-matches) ""))))


(defun meib nil
  "Connect to the servers in `meib-server-alist'.
Note that this command doesn't require any arguments, as bots are
usually meant to be configured."
  (interactive)
  (dolist (server meib-server-alist)
    (let* ((address (car server))
	   (server-plist (cdr server))
	   (port (or (plist-get server-plist :port) meib-default-port))
	   (use-tls (or (plist-get server-plist :use-tls) meib-default-use-tls))
	   (nick-name (or (plist-get server-plist :nick-name) meib-default-nick-name))
	   (user-name (or (plist-get server-plist :user-name) meib-default-user-name))
	   (real-name (or (plist-get server-plist :real-name) meib-default-real-name)))
      (meib-connect address port use-tls nick-name user-name real-name))))

(defun meib-connect (address port use-tls nick-name user-name real-name)
  "Create a MEIB buffer and connect to ADDRESS at PORT.
Set USE-TLS to true if you want the connection to use TLS.  The
other arguments (NICK-NAME, USER-NAME, REAL-NAME), are described
in detail in `meib-server-alist'.

If already connected to SERVER, display a message."
  (if (assoc (get-process (format "meib/%s" address)) meib-connected-server-alist)
      (message "Already connected to server %s." address)
    (let* ((server-plist (cdr (assoc-string address meib-server-alist)))
	   (throttle (plist-get server-plist :throttle))
	   (buffer (meib-create-buffer (format "*meib/%s*" address)))
	   (process (open-network-stream
		     (format "meib/%s" address) buffer address port
		     :type (if use-tls 'tls 'plain))))
      (set-process-filter process 'meib-filter)
      (set-process-sentinel process 'meib-sentinel)
      (meib-send-string process (format "NICK %s" nick-name))
      (meib-send-string process (format "USER %s 0 * :%s" user-name real-name))
      (if throttle
	  (push
	   `(,process :buffer ,buffer :address ,address :nick-name ,nick-name :queue nil
		      :queue-timer ,(run-with-timer throttle throttle 'meib-queue-timer-function process throttle)
		      :channels nil)
	   meib-connected-server-alist)
	(push `(,process :buffer ,buffer :address ,address :nick-name ,nick-name :channels nil)
	      meib-connected-server-alist)))))


(defun meib-queue-timer-function (process throttle)
  "Call the first function in the PROCESS queue.
Also reset the PROCESS queue timer with a delay of THROTTLE."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (queue (plist-get connected-server-plist :queue))
	 (queue-timer (plist-get connected-server-plist :queue-timer)))
    (when queue
      (cancel-timer queue-timer)
      (plist-put connected-server-plist
		 :queue-timer (run-with-timer throttle throttle 'meib-queue-timer-function process throttle))
      (apply (car (last queue)))
      (plist-put connected-server-plist :queue (butlast queue)))))

(defun meib-create-buffer (name)
  "Create a MEIB buffer with NAME."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (meib-mode))
    buffer))

(defvar meib-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'meib-privmsg-interactive)
    map))

;; TODO: this is pretty naive, maybe we should use (if it exists)
;; buffer properties or something? This really should work in a
;; different way. We shouldn't be operating on buffer names. This is
;; probably the most horrible code in this file.
(defun meib-privmsg-interactive (message)
  "Prompts for MESSAGE and sends it to channel."
  (interactive "sMessage: ")
  (let* ((server-name (car (split-string (cadr (split-string (buffer-name (current-buffer)) "/")) "@")))
	 (process (get-process (concat "meib/" server-name)))
	 (channel (substring (cadr (split-string (buffer-name (current-buffer)) "@")) 0 -1))
	 (nick-name (plist-get (cdr (assoc process meib-connected-server-alist)) :nick-name)))
    ;; If it's called interactively, we can also parse the bots
    ;; message.
    (meib-privmsg process channel message)
    (meib-process-command process `(:sender ,nick-name :command "PRIVMSG" :arguments (,channel ,message)))))

(define-derived-mode meib-mode special-mode "MEIB"
  "Mode used by the MEIB buffers.

\\{meib-mode-map}"
  (use-local-map meib-mode-map)
  (setq mode-name "MEIB")
  (setq major-mode 'meib-mode))

(defun meib-filter (process output)
  "Called when PROCESS recieves OUTPUT."
  (let* ((lines (split-string output "[\r\n]" t)))
    (when (= ?\n (aref output (1- (length output))))
      (dolist (line lines)
	(meib-handle-server-response process (meib-process-irc-message line))))))

;; In large copied from `rcirc.el'.
(defun meib-process-irc-message (message)
  "Atomize MESSAGE into useful bits.
MESSAGE thus becomes a plist of:

`:sender'

The sender of the message.

`:command'

The command (i.e. PRIVMSG, JOIN, etc).

`:arguments'

The arguments to the command. Most commonly the first argument is
a channel or an user, and the second argument is the text."
  (if (string-match "^\\(:\\([^ ]+\\) \\)?\\([^ ]+\\) \\(.+\\)$" message)
      (let* ((sender (match-string 2 message))
             (command (match-string 3 message))
             (arguments (match-string 4 message)))
	(string-match "^\\([^:]*\\):?\\(.+\\)?$" arguments)
	(let* ((args1 (match-string 1 arguments))
               (args2 (match-string 2 arguments))
               (arguments (delq nil (append (split-string args1 " " t) (list args2)))))
	  `(:sender ,sender :command ,command :arguments ,arguments)))
    (message "Unhandled command: %s" message) nil))

(defun meib-handle-server-response (process message)
  "Handle MESSAGE from PROCESS."
  (when (not (null message))
    (meib-debug-print process message)
    (let ((handler (intern-soft (concat "meib-handler-" (plist-get message :command)))))
      (when (fboundp handler)
	(funcall handler process message)))))

(defun meib-handler-PING (process message)
  "Handle MESSAGE from PROCESS if it's a PING.
After a PING, we send a PONG back to the process."
  (meib-send-string process (format "PONG %s" (car (plist-get message :arguments)))))

(defun meib-handler-001 (process message)
  "Handle MESSAGE from PROCESS if it's a 001.
Afer a 001 command, we join all channels of the process."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (address (plist-get connected-server-plist :address))
	 (server-plist (cdr (assoc-string address meib-server-alist)))
	 (channels (plist-get server-plist :channels)))
    (dolist (channel channels)
      (meib-send-string process (format "JOIN %s" channel)))))

(defun meib-handler-353 (process message)
  "Handle MESSAGE from PROCESS if it's a 353.
After a 353 command, we take the list of users and add it to our
list of users."
  (let* ((channel-name (caddr (plist-get message :arguments)))
  	 (message-names
  	  (split-string (replace-regexp-in-string "[+%&@~]" "" (cadddr (plist-get message :arguments))) " " t)))
    (dolist (name message-names)
      (meib-add-channel-user process channel-name name))))

(defun meib-handler-JOIN (process message)
  "Handle MESSAGE from PROCESS if it's a JOIN.
After a JOIN command, we add the user to our list of users."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender))))
    (meib-add-channel-user process channel-name nick-name)))

(defun meib-handler-PART (process message)
  "Handle MESSAGE from PROCESS if it's a PART.
After a PART command, we remove the user from our list of
users."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender))))
    (meib-remove-channel-user process channel-name nick-name)))

(defun meib-handler-KICK (process message)
  "Handle MESSAGE from PROCESS if it's a KICK.
After a KICK command, we do the same thing as in
`meib-handler-PART'."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (nick-name (cadr (plist-get message :arguments))))
    (meib-remove-channel-user process channel-name nick-name)))

(defun meib-handler-QUIT (process message)
  "Handle MESSAGE from PROCESS if it's a QUIT.
After a QUIT command, we do the same thing as in
`meib-handler-NICK', except we don't change the nick name, but
remove it instead."
  (let* ((channels (plist-get (cdr (assoc process meib-connected-server-alist)) :channels))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender))))
    (dolist (channel channels)
      (meib-remove-channel-user process (car channel) nick-name))))

(defun meib-handler-NICK (process message)
  "Handle MESSAGE from PROCESS if it's a NICK.
After a NICK command, we change the username in ALL channels."
  (let* ((channels (plist-get (cdr (assoc process meib-connected-server-alist)) :channels))
	 (new-nick-name (car (plist-get message :arguments)))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender))))
    (dolist (channel channels)
      (when (member nick-name (plist-get (cdr channel) :users))
	(meib-remove-channel-user process (car channel) nick-name)
	(meib-add-channel-user process (car channel) new-nick-name)))))

(defun meib-handler-PRIVMSG (process message)
  "Handle MESSAGE from PROCESS if it's a PRIVMSG.
In here, we just call the callbacks in
`meib-privmsg-callbacks'. These do whatever they want, from
handling commands, to parsing URLs."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (server-plist (cdr (assoc (plist-get connected-server-plist :address) meib-server-alist)))
	 (ignored-users (plist-get server-plist :ignored-users))
	 (sender-nick-name (meib-nick-name-from-full-name (plist-get message :sender))))
    ;; We don't want the bot to see ignored users' PRIVMSGs.
    (when (not (member sender-nick-name ignored-users))
      (dolist (callback meib-privmsg-callbacks)
	(funcall callback process message)))))

(defun meib-add-channel-user (process channel-name nick-name)
  "Adds NICK-NAME to CHANNEL-NAME in the PROCESS plist.
If NICK-NAME is us, also create the channel."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (channels (plist-get connected-server-plist :channels))
	 (channel (assoc-string channel-name channels)) ;I think that's useless?
	 (channel-plist (cdr channel)))
    (when (not (member nick-name (plist-get channel-plist :users)))
      (if (string= nick-name (plist-get connected-server-plist :nick-name))
	  (progn
	    (let ((buffer (meib-create-buffer (concat "*meib/" (plist-get connected-server-plist :address) "@" channel-name "*"))))
	      (plist-put connected-server-plist :channels (append channels `((,channel-name :buffer ,buffer :users (,nick-name)))))))
	(plist-put channel-plist :users (append (plist-get channel-plist :users) `(,nick-name)))))))

(defun meib-remove-channel-user (process channel-name nick-name)
  "Removes NICK-NAME from CHANNEL-NAME in the PROCESS plist.
If NICK-NAME is us, remove the whole channel."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (channels (plist-get connected-server-plist :channels))
	 (channel-plist (cdr (assoc-string channel-name channels))))
    (if (string= nick-name (plist-get connected-server-plist :nick-name))
	(plist-put connected-server-plist :channels (delete (assoc-string channel-name channels) channels))
      (plist-put channel-plist :users (delete nick-name (plist-get channel-plist :users))))))

(defun meib-nick-name-from-full-name (full-name)
  "Gets only the nickname from FULL-NAME.
That means it'll get `user' from `user!~user@vhost.example'"
  (car (split-string full-name "!")))

(defun meib-send-string (process string)
  "Send STRING to PROCESS.
Appends an `\r\n'."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (nick-name (plist-get connected-server-plist :nick-name)))
    (meib-debug-print process (plist-put (meib-process-irc-message string) :sender nick-name))
    (process-send-string process (concat string "\r\n"))))

(defun meib-send-string-queue (process string)
  "Send STRING to PROCESS via the PROCESS queue."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (queue (plist-get connected-server-plist :queue)))
    (plist-put connected-server-plist :queue (append `((meib-send-string ,process ,string)) queue))))

(defun meib-sentinel (process sentinel)
  "Called when PROCESS receives SENTINEL."
  (let ((sentinel (replace-regexp-in-string "\n" "" sentinel)))
    (message "Disconnecting. Received sentinel: %s." sentinel)
    (meib-disconnect process)))

(defun meib-disconnect (process)
  "Disconnect from PROCESS."
  (if (not (assoc process meib-connected-server-alist))
      (message "Already disconnected from process %s." process)
    (let ((connected-server-plist (assoc process meib-connected-server-alist))
	  (queue-timer (plist-get connected-server-plist :queue-timer)))
      (when (timerp queue-timer)
	(cancel-timer queue-timer))
      (when (not (null process))
      	(delete-process process))
      (with-current-buffer (plist-get (cdr connected-server-plist) :buffer)
	(setq mode-line-process ":disconnected"))
      (setq meib-connected-server-alist (delq connected-server-plist meib-connected-server-alist)))))

(provide 'meib)

;;; meib.el ends here
