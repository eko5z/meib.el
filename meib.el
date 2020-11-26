;;; meib.el --- MEIB Emacs I(RC) Bot.

;;; Commentary:
;; The code is inspired (and in some parts copied) from
;; rcirc.el. Although the code is also vastly different, with a
;; different architecture.
;;
;; And I'm not boasting or anything, but this bot is state-of-the-art.
;; 
;; TODO:
;; 
;; o Responding to CTCP messages.

;;; Code:

(require 'meib-permission (concat (file-name-directory (buffer-file-name)) "meib-permission.el"))
(require 'meib-spook (concat (file-name-directory (buffer-file-name)) "meib-spook.el"))
(require 'meib-highlight (concat (file-name-directory (buffer-file-name)) "meib-highlight.el"))
(require 'meib-covid-19 (concat (file-name-directory (buffer-file-name)) "meib-covid-19.el"))
(require 'meib-quotes (concat (file-name-directory (buffer-file-name)) "meib-quotes.el"))
(require 'meib-love (concat (file-name-directory (buffer-file-name)) "meib-love.el"))
(require 'meib-doctor (concat (file-name-directory (buffer-file-name)) "meib-doctor.el"))

(defgroup meib nil
  "MEIB Emacs I(RC) Bot."
  :link '(url-link "https://github.com/emssej/meib.el")
  :link '(variable-link meib-server-alist)
  :link '(variable-link meib-command-alist)
  :version "26.3"
  :prefix "meib-"
  :group 'applications)

;; All the properties here are mandatory, so I don't know if we
;; shouldn't use a different structure...  And on another note, maybe
;; in the future we should support authentication other than NickServ,
;; but really, I don't think anyone uses those fringe servers that
;; don't use NickServ. It's an easy implementation, though.
(defcustom meib-server-alist
  (quote (("irc.freenode.net" :port 7000
	   :use-tls t :nick-name "Meibel"
	   :user-name "meibel" :real-name "Maybelline"
	   :authentication (("Meibel" . "password"))
	   :channels ("#gnu" "#fsf" "#emacs")
	   :throttle 1.6 :truncate 419 :anti-kick 6 :rejoin 3
	   :ignored-users ("someone")
	   :authorized-users ("Meibel"))))
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

`:authentication'

Passwords associated with nicks to use when identifying with
NickServ. It's in the form of (NICK . PASSWORD).

`:throttle'

Whether to throttle PRIVMSG's.  This is an anti-flooding measure,
and also deters channel bots that kick if an user floods.

`:truncate'

Whether to truncate PRIVMSG's, and by how many characters.  If you
are also using `:anti-kick', the number of invisible characters
is subtracted from this value.

`:anti-kick'

Whether to apply anti-kick measures, i.e. apply a random number
of invisible characters to the beginning of every PRIVMSG.

`:rejoin'

If non-nil, the amount of seconds after which the bot should
rejoin a channel after being kicked.

`:ignored-users'

Users whose PRIVMSG's are ignored by the bot.

`:authorized-users'

Users authorized to use restricted
commands (`meib-restricted-command-alist').

`:channels'

What channels to connect to in the server."
  :group 'meib
  :type (quote (alist :key-type (string :tag "Address")
		      :value-type (plist :tag "Options"
					 :options
					 ((:port (integer :tag "Port"))
					  (:use-tls (boolean :tag "Use TLS"))
					  (:nick-name (string :tag "Nick name"))
					  (:user-name (string :tag "User name"))
					  (:real-name (string :tag "Real name"))
					  (:authentication
					   (choice (alist :tag "Authenticate"
							  :key-type (string :tag "Nick name")
							  :value-type (string :tag "Password"))
						   (const :tag "Don't authenticate" nil)))
					  (:channels (repeat :tag "Channels" (string :tag "Channel")))
					  (:throttle
					   (choice (number :tag "Throttle time (in seconds)")
						   (const :tag "Don't throttle" nil)))
					  (:truncate
					   (choice (integer :tag "Truncate lines (in number of characters)")
						   (const :tag "Don't truncate lines" nil)))
					  (:anti-kick
					   (choice (integer :tag "Random number of invisible characters in interval [0,n)")
						   (const :tag "Don't apply anti-kick measures" nil)))
					  (:rejoin
					   (choice (number :tag "Rejoin time (in seconds)")
						   (const :tag "Don't rejoin channels" nil)))
					  (:ignored-users
					   (repeat :tag "Ignored users"
						   (string :tag "User")))
					  (:authorized-users
					   (repeat :tag "Users allowed to use authorized commands"
						   (string :tag "User"))))))))
  
(defcustom meib-privmsg-callbacks
  '(meib-process-command meib-dot-bots-reply)
  "Callbacks exceuted on a PRIVMSG command.
The callbacks take in the arguments (PROCESS MESSAGE).

PROCESS is an usual Emacs process, to send strings to it use
`meib-send-string', but it is suggested to use more high-level
functions such as `meib-privmsg'.  Those commands use the server
queue, which prevents the bot from flooding.

MESSAGE is a plist of IRC message components, which is described
in detail in `meib-process-irc-message'."
  :group 'meib
  :type '(repeat (function :tag "Callback")))

(defcustom meib-command-alist
  '(("help" . meib-command-help)
    ("permission" . meib-permission)
    ("spook" . meib-spook)
    ("covid-19" . meib-covid-19)
    ("quotes" . meib-quotes)
    ("love" . meib-love)
    ("doctor" . meib-doctor))
  "The callbacks to commands processed by `meib-process-command'.
They are in the form of (COMMAND-NAME . FUNCTION).

FUNCTION takes in the arguments (PROCESS SENDER RECEIVER ARGUMENTS).  For a
more in-depth explanation of MESSAGE, consult
`meib-privmsg-callbacks'.  For an explanation of ARGUMENTS, consult
`meib-parse-command'."
  :group 'meib
  :type '(repeat (cons :tag "Command" (string :tag "Name") (function :tag "Callback"))))

(defcustom meib-restricted-command-alist
  '(("clear-queue" . meib-command-clear-queue)
    ("hilite" . meib-highlight))
  "The callbacks to commands processed by `meib-process-command'.
They are in the form of (COMMAND-NAME . FUNCTION).

This list contains commands restricted to certain
users (`:authorized-users' in `meib-server-alist').

For a more in-depth description of this alist, see
`meib-command-alist'."
  :group 'meib
  :type '(repeat (cons :tag "Command" (string :tag "Name") (function :tag "Callback"))))

(defcustom meib-command-regexp
  (rx ?\¿ (group (group (1+ (any word alphanumeric punct))) (0+ (any blank) (group (1+ (any word alphanumeric punct))))) ?\?)
  "Regexp that matches a command.
The group that the regexp matches should be the command name and
the arguments, in the form \"command arg1 ... argn\". "
  :group 'meib
  :type (quote (choice (const :tag "Spanish question: `¿command arg1 ... argn?'"
			      "¿\\(\\([[:word:][:alnum:][:punct:]]+\\)\\(?:[[:blank:]]\\([[:word:][:alnum:][:punct:]]+\\)\\)*\\)\\?")
		       (regexp :tag "Custom"))))

(defcustom meib-restricted-command-regexp
  (rx ?\¡ (group (group (1+ (any word alphanumeric punct))) (0+ (any blank) (group (1+ (any word alphanumeric punct))))) ?\!)
  "Regexp that matches a restricted command.
See `meib-command-regexp'."
  :group 'meib
  :type (quote (choice (const :tag "Spanish exclamation: `¡command arg1 ... argn!'"
			      "¡\\(\\([[:word:][:alnum:][:punct:]]+\\)\\(?:[[:blank:]]\\([[:word:][:alnum:][:punct:]]+\\)\\)*\\)!")
		       (regexp :tag "Custom"))))

(defcustom meib-speak-through-actions t
  "Whether or not the bot should speak through ACTIONs."
  :group 'meib
  :type 'boolean)

(defvar meib-connected-server-alist nil
  "An alist of connected servers.
Also contains information about the server, i.e. the process,
buffer, the queue and queue timer (for each channel), the
nickname of the bot, the channels the bot is in, the usernames in
each channel, and so on.  The information is a plist in the
form (PARAMETER VALUE).")

(defun meib-process-command (process message)
  "Process MESSAGE and see if it contains a command.
If it does, call (or don't) the corresponding callback with
arguments PROCESS and MESSAGE, as well as the received arguments."
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
	(funcall command-function process (plist-get message :sender) (car (plist-get message :arguments)) (cdr command-with-args))
      (when restricted-command-function
	(if (member nick-name authorized-users)
	    (funcall restricted-command-function process (plist-get message :sender) (car (plist-get message :arguments)) (cdr restricted-command-with-args))
	  (meib-privmsg process channel-name "You can't use this command."))))))

(defun meib-parse-command (input regexp)
  "Process INPUT according to REGEXP and see if it has a command.
If it is, return (COMMAND ARG1 ... ARGN), otherwise return nil."
  (when (string-match regexp input)
    (split-string (match-string 1 input) " " t)))

(defun meib-dot-bots-reply (process message)
  "Process MESSAGE and see if it's \".bots\".
If it is, reply with the name of the bot and the source."
  (let ((channel-name (car (plist-get message :arguments)))
	(text (cadr (plist-get message :arguments))))
    (when (string= text ".bots")
      (meib-privmsg process channel-name "MEIB Emacs I(RC) Bot, AGPLv3---https://github.com/emssej/meib.el/"))))

(defun meib-command-clear-queue (process sender receiver arguments)
  "Clear the message queue."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist))))
    (plist-put connected-server-plist :queue nil)
    (meib-privmsg process receiver "Cleared the message queue.")))

(defun meib-command-help (process sender receiver arguments)
  "Go through all commands and list them.
If an argument is specified, then show the specified command's
documentation."
  (let* ((arg1 (car arguments))
	 (command-function (cdr (assoc arg1 meib-command-alist)))
	 (restricted-command-function (cdr (assoc arg1 meib-restricted-command-alist)))
	 (command-list (mapconcat (lambda (command) (car command)) meib-command-alist " "))
	 (restricted-command-list (mapconcat (lambda (command) (concat "/!\\" (car command) "/!\\"))
					     meib-restricted-command-alist " "))
	 (documentation (when (or command-function restricted-command-function)
			  (replace-regexp-in-string
			   "\n" " " (documentation (or command-function restricted-command-function))))))
    (if (and arg1 (or command-function restricted-command-function))
	(meib-privmsg process receiver (format "%s --- %s" (mp arg1 t) (mp documentation nil t)))
      (meib-privmsg process receiver (format "Available commands: %s"
					     (concat command-list " " restricted-command-list))))))

;; I'm not sure if the characters used are exactly the de-facto
;; standard---but I use rcirc, and that's what it seems to parse. The
;; order of the arguments is specific in this way because I think bold
;; and italic are the most used properties.
(defalias 'mp 'meib-propertize)		;Is this a good alias?

(defun meib-propertize (text &optional bold italic fg bg underline)
  "Attach IRC text properties to TEXT.
If FG is non-nil, use it as the text foreground color. If BG is
non-nil, use it as the text background color. A color is a number
from 0–15.
If BOLD is non-nil, make the text bold.
If ITALIC is non-nil, make the text italic.
If UNDERLINE is non-nil, underline the text."
  (concat (when fg (string ?\C-c)) (when fg (number-to-string fg))
	  (when (and fg bg) (string ?\,)) (when (and fg bg) (number-to-string bg))
	  (when bold (string ?\C-b)) (when italic (string ?\C-v))
	  (when underline (string ?\C-_)) text (string ?\C-o)))

(defun meib-privmsg (process receiver message)
  "Send MESSAGE to RECEIVER via PROCESS.
This function also truncates MESSAGE to the value set in
`meib-server-alist', and appends invisible characters at the
end (if it was set in `meib-server-alist').

If MESSAGE is longer than the truncate value, then send several
split messages."
  (let* ((address (plist-get (cdr (assoc process meib-connected-server-alist)) :address))
	 (server-plist (cdr (assoc-string address meib-server-alist)))
	 (messages (meib-format-privmsg receiver message (plist-get server-plist :truncate)
					(plist-get server-plist :anti-kick) meib-speak-through-actions)))
    (if (plist-get server-plist :throttle)
	(progn
	  (meib-send-string-queue process (car messages))
	  (when (cadr messages)
	    (meib-privmsg process receiver (cadr messages))))
      (meib-send-string process (car messages))
      (when (cadr messages)
	(meib-privmsg process receiver (cadr messages))))))

(defun meib-format-privmsg (receiver message &optional truncate anti-kick actionp)
  "Format MESSAGE as a PRIVMSG.
This function applies all the needed operations specified in
`meib-server-alist', i.e. TRUNCATE and ANTI-KICK, and also sets
the target as RECEIVER.

If MESSAGE is longer than TRUNCATE, also return the remaining
string, in the form (FORMATTED REMAINING).

If ACTIONP is non-nil, send the message as an ACTION."
  (let* ((n (* (/ (random anti-kick) 2) 2))
	 (truncate-width (if anti-kick (- truncate n (if actionp 9 0)) truncate))
	 (truncated (if truncate (truncate-string-to-width message truncate-width) message))
	 (rest (if (> (length message) truncate-width) (substring message truncate-width) nil)))
    (list
     (format "PRIVMSG %s :%s%s%s‍%s"
	     (meib-nick-name-from-full-name receiver) (if actionp "ACTION " "")
	     (if truncate truncated message) (if anti-kick (make-string n ?\C-b) "")
	     (if actionp ""))
     rest)))

(defun meib nil
  "Connect to the servers in `meib-server-alist'.
Note that this command doesn't require any arguments, as bots are
usually meant to be configured."
  (interactive)
  (dolist (server meib-server-alist)
    (let* ((address (car server))
	   (server-plist (cdr server))
	   (port (plist-get server-plist :port))
	   (use-tls (plist-get server-plist :use-tls))
	   (nick-name (plist-get server-plist :nick-name))
	   (user-name (plist-get server-plist :user-name))
	   (real-name (plist-get server-plist :real-name)))
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
      (meib-change-nick-name process nick-name)
      (meib-send-string process (format "USER %s 0 * :%s" user-name real-name))
      (if throttle
	  (push
	   `(,process :buffer ,buffer :address ,address :nick-name ,nick-name :queue nil
		      :queue-timer ,(run-with-timer throttle throttle 'meib-queue-timer-function process throttle)
		      :channels nil)
	   meib-connected-server-alist)
	(push `(,process :buffer ,buffer :address ,address :nick-name ,nick-name :channels nil)
	      meib-connected-server-alist)))))

(defun meib-change-nick-name (process nick-name)
  "Change nick in PROCESS to NICK-NAME."
  (meib-send-string process (format "NICK %s" nick-name)))

(defun meib-get-nick-authentication (process nick-name)
  "Returns the authentication of NICK-NAME in PROCESS.
If there is none, return nil."
  (let* ((address (plist-get (cdr (assoc process meib-connected-server-alist)) :address))
	 (server-plist (cdr (assoc-string address meib-server-alist)))
	 (password (cdr (assoc-string nick-name (plist-get server-plist :authentication)))))
    password))

(defun meib-authenticate (process nick-name)
  "Authenticate NICK-NAME in PROCESS with its password.
This function returns nil if the nick name is not in the
authentication list. Otherwise, it returns the password.

The authentication information is contained in
`meib-server-alist' under `:authentication'."
  (let* ((password (meib-get-nick-authentication process nick-name)))
    (when password
      (meib-send-string process (format "PRIVMSG NickServ :identify %s" password)))
    password))
  
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

(defvar meib-privmsg-interactive-channel-history nil)
(defvar meib-privmsg-interactive-message-history nil)

(defun meib-privmsg-interactive (channel message)
  "Prompts for MESSAGE and sends it to CHANNEL."
  (interactive
   (let* ((channels (plist-get (cdr (assoc (get-buffer-process (current-buffer)) meib-connected-server-alist)) :channels))
	  (channel-names (mapcar (lambda (channel) (car channel)) channels))
	  (last-channel (car meib-privmsg-interactive-channel-history)))
     (list
      (completing-read (format "Channel (default %s): " last-channel) channel-names
		       (lambda (input) (member input channel-names)) 'confirm nil
		       'meib-privmsg-interactive-channel-history last-channel)
      (read-string "Message: " nil 'meib-privmsg-interactive-message-history))))
  (let* ((process (get-buffer-process (current-buffer)))
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

The arguments to the command.  Most commonly the first argument is
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
    (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	   (server-plist (cdr (assoc (plist-get connected-server-plist :address) meib-server-alist)))
	   (ignored-users (plist-get server-plist :ignored-users))
	   (sender-nick-name (when (plist-get message :sender) (meib-nick-name-from-full-name (plist-get message :sender)))))
      ;; We don't want the bot to see ignored users' PRIVMSGs.
      (when (or (not (member sender-nick-name ignored-users))
		(not sender-nick-name))
	(meib-debug process message)
	(let ((handler (intern-soft (concat "meib-handler-" (plist-get message :command)))))
	  (when (fboundp handler)
	    (funcall handler process message)))))))

(defun meib-debug (process message)
  "Print MESSAGE to the PROCESS buffer."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (command (plist-get message :command))
	 (sender (plist-get message :sender))
	 (arguments (plist-get message :arguments))
	 (buffer (plist-get connected-server-plist :buffer))
	 (inhibit-read-only t))
    (when buffer
      (when (not (or (string= command "PONG") (string= command "PING")))
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (insert (format "%s %s %s" command sender (mapconcat 'identity arguments " ")))
	  (newline))))))

(defun meib-handler-PING (process message)
  "Handle MESSAGE from PROCESS if it's a PING.
After a PING, we send a PONG back to the process."
  (meib-send-string process (format "PONG %s" (car (plist-get message :arguments)))))

(defun meib-handler-001 (process message)
  "Handle MESSAGE from PROCESS if it's a 001.
Afer a 001 command, we join all channels of the process."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (nick-name (plist-get connected-server-plist :nick-name)))
    ;; Authenticate, but only join if it's impossible to do so. Then,
    ;; in `meib-handler-MODE' we connect only after we've registered.
    (when (not (meib-authenticate process nick-name))
      (meib-join-channels process))))

(defun meib-join-channels (process)
  "Join all channels specified in `meib-connected-server-alist'."
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
After a KICK command, we remove the user from our list of users.
If the rejoin option is set in `meib-server-alist', then we also
try to rejoin."
  (let* ((rejoin (plist-get (cdr (assoc-string (plist-get (cdr (assoc process meib-connected-server-alist)) :address) meib-server-alist)) :rejoin))
	 (channel-name (car (plist-get message :arguments)))
	 (nick-name (cadr (plist-get message :arguments))))
    (meib-remove-channel-user process channel-name nick-name)
    (when rejoin
      (run-with-timer rejoin nil (lambda (process channel-name) (meib-send-string process (format "JOIN %s" channel-name))) process channel-name))))

(defun meib-handler-QUIT (process message)
  "Handle MESSAGE from PROCESS if it's a QUIT.
After a QUIT command, we do the same thing as in
`meib-handler-NICK', except we don't change the nick name, but
remove it instead."
  (let* ((channels (plist-get (cdr (assoc process meib-connected-server-alist)) :channels))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender))))
    (dolist (channel channels)
      (meib-remove-channel-user process (car channel) nick-name))))

(defun meib-handler-MODE (process message)
  "Handle MESSAGE from PROCESS if it's a MODE.
This is used mainly for checking if our nick is registered, in
order to join channels *after* we've been registered."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (mode (cadr (plist-get message :arguments)))
	 (nick-name (car (plist-get message :arguments))))
    (when (and (string= mode "+r")
	       (string= nick-name (plist-get connected-server-plist :nick-name)))
      (meib-join-channels process))))

(defun meib-handler-NICK (process message)
  "Handle MESSAGE from PROCESS if it's a NICK.
After a NICK command, we change the username in ALL channels."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (channels (plist-get connected-server-plist :channels))
	 (new-nick-name (car (plist-get message :arguments)))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender))))
    ;; If it's us, we also try to authenticate.
    (when (string= nick-name (plist-get connected-server-plist :nick-name))
      (meib-authenticate process new-nick-name))
    (dolist (channel channels)
      (when (member nick-name (plist-get (cdr channel) :users))
	(meib-remove-channel-user process (car channel) nick-name)
	(meib-add-channel-user process (car channel) new-nick-name)))))

(defun meib-handler-PRIVMSG (process message)
  "Handle MESSAGE from PROCESS if it's a PRIVMSG.
In here, we just call the callbacks in
`meib-privmsg-callbacks'.  These do whatever they want, from
handling commands, to parsing URLs."
  (dolist (callback meib-privmsg-callbacks)
    (funcall callback process message)))

(defun meib-add-channel-user (process channel-name nick-name)
  "Add NICK-NAME to CHANNEL-NAME in the PROCESS plist.
If NICK-NAME is us, also create the channel."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (channels (plist-get connected-server-plist :channels))
	 (channel (assoc-string channel-name channels)) ;I think that's useless?
	 (channel-plist (cdr channel)))
    (when (not (member nick-name (plist-get channel-plist :users)))
      (if (string= nick-name (plist-get connected-server-plist :nick-name))
	  (plist-put connected-server-plist :channels (append channels `((,channel-name :users (,nick-name)))))
	(plist-put channel-plist :users (append (plist-get channel-plist :users) `(,nick-name)))))))

(defun meib-remove-channel-user (process channel-name nick-name)
  "Remove NICK-NAME from CHANNEL-NAME in the PROCESS plist.
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
  (let* ((nick-name (plist-get (cdr (assoc process meib-connected-server-alist)) :nick-name)))
    (meib-debug process (plist-put (meib-process-irc-message string) :sender nick-name))
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
    (let* ((connected-server-plist (assoc process meib-connected-server-alist))
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
