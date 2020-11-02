;;; meib.el --- My Emacs I(RC) Bot.

;;; Commentary:
;; The code is largely inspired (and in some parts copied) from
;; rcirc.el.
;; 
;; TODO:
;;
;; o Regex-matching for the command.
;;

;;; Code:

(require 'meib-permission (concat (file-name-directory (buffer-file-name)) "meib-permission.el"))

(defgroup meib nil
  "MEIB Emacs I(RC) Bot."
  :link '(variable-link meib-command-alist)
  :link '(variable-link meib-server-alist)
  :link '(url-link "https://github.com/emssej/meibel")
  :version "26.3"
  :prefix "meib-"
  :group 'applications)

(defcustom meib-server-alist
  (quote (("irc.rizon.net" :port 9999
	   :use-tls t :nick-name "Meibel"
	   :user-name "meibel" :real-name "Maybelline"
	   :throttle 2 :truncate 419 :anti-kick 5
	   :channels ("#/tech/" "#8chan"))))
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
				    (:channels (repeat :tag "Channels" (string :tag "Channel")))))))

(defcustom meib-privmsg-callbacks
  '(meib-process-command meib-debug-print)
  "Callbacks exceuted on a PRIVMSG command.
The callbacks take in the arguments (PROCESS MESSAGE).

PROCESS is an usual Emacs process, to send strings to it use
`meib-send-string', but it is suggested to use more high-level
functions such as `meib-privmsg'. Those commands use the server
queue, which prevents the bot from flooding.

MESSAGE is a plist of IRC message components, which is described
in detail in `meib-process-irc-message'."
  :group 'meib
  :type '(repeat (function :tag "Callback")))

(defcustom meib-command-alist
  '(("help" . meib-command-help)
    ("permission" . meib-permission))
  "The callbacks to commands processed by `meib-process-command'.
They are in the form of (COMMAND-NAME . FUNCTION).

FUNCTION takes in the arguments (PROCESS MESSAGE ARGUMENTS). For a
more in-depth explanation of MESSAGE, consult
`meib-privmsg-callbacks'. For an explanation of ARGUMENTS, consult
`meib-parse-command'."
  :group 'meib
  :type '(repeat (cons :tag "Command" (string :tag "Name") (function :tag "Callback"))))

(defcustom meib-command-regexp "¿\\(.+\\)\\?"
  "Regexp that matches a command.
For now, the only group should be the actual command, i.e. `help
arg1 arg2', which is then split by spaces."
  :group 'meib
  :type 'regexp)

(defconst meib-default-address "irc.freenode.net")
(defconst meib-default-port 9999)
(defconst meib-default-use-tls t)
(defconst meib-default-nick-name "Meibel")
(defconst meib-default-user-name "meibel")
(defconst meib-default-real-name "Maybelline")

(defvar meib-connected-server-alist nil
  "An alist of connected servers.

Also contains information about the server, i.e. the process,
buffer, the queue and queue timer (for each channel), the
nickname of the bot, the channels the bot is in, the usernames in
each channel, and so on.  The information is a plist in the
form (PARAMETER VALUE).")

(defun meib-command-help (process message arguments)
  "Go through all commands and list them.
If an argument is specified, then show the specified command's
documentation."
  (let* ((channel (car (plist-get message :arguments)))
	 (arg1 (car arguments))
	 (command-function (cdr (assoc arg1 meib-command-alist))))
    (if (and arg1 command-function)
	(meib-privmsg
	 process channel (format "%s --- %s" arg1 (replace-regexp-in-string "\n" " " (documentation command-function))))
      (meib-privmsg process channel
		    (format "Available commands: %s"
			    (mapconcat (lambda (command) (car command)) meib-command-alist " "))))))

(defun meib-privmsg (process receiver message)
  "Send a MESSAGE PRIVMSG to RECIEVER via PROCESS.
This function also truncates MESSAGE to the value set in
`meib-server-alist', and appends invisible characters at the
end (if it was set in `meib-server-alist')."
  (let* ((address (plist-get (cdr (assoc process meib-connected-server-alist)) :address))
	 (server-plist (cdr (assoc-string address meib-server-alist)))
	 (throttle (plist-get server-plist :throttle))
	 (truncate (plist-get server-plist :truncate))
	 (anti-kick (plist-get server-plist :anti-kick))
	 (command (meib-format-privmsg receiver message truncate anti-kick)))
    (if throttle
	(meib-send-string-queue process command)
      (meib-send-string process command))))

(defun meib-format-privmsg (receiver message &optional truncate anti-kick)
  "Format the MESSAGE.
This function applies all the needed operations specified in
`meib-server-alist', i.e. TRUNCATE and ANTIKICK, and also sets
the targe as RECEIVER."
  (format "PRIVMSG %s :%s%s‍"
	  receiver
	  (if truncate
	      (truncate-string-to-width message (if anti-kick (- truncate anti-kick) truncate))
	    message)
	  (if anti-kick
	      (make-string (* (/ (random anti-kick) 2) 2) ?‌)) ;Zero width non-joiner.
	  ""))

(defun meib-debug-print (process message)
  "Print MESSAGE to the PROCESS buffer."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (buffer (plist-get connected-server-plist :buffer))
	 (inhibit-read-only t))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "[%s] %s: %s"
		      (propertize (car (plist-get message :arguments))
				  'face '(:foreground "red" :slant italic))
		      (propertize (meib-nick-name-from-full-name
				   (plist-get message :sender))
				  'face '(:foreground "blue" :weight bold))
		      (cadr (plist-get message :arguments))))
      (newline))))

(defun meib-process-command (process message)
  "Process MESSAGE and see if it contains a command.
If it does, execute the command callback corresponding to the
command."
  (let* ((command-with-args (meib-parse-command (cadr (plist-get message :arguments))))
	 (command (car command-with-args))
	 (arguments (cdr command-with-args))
	 (command-function (cdr (assoc command meib-command-alist))))
    (when command-function
      (funcall command-function process message (cdr command-with-args)))))

(defun meib-parse-command (input)
  "Process INPUT and see if it has a command.
If it is, return (COMMAND . ARGUMENTS), otherwise return nil."
  ;; Make it use a real regex matching the command... soon...
  (when (string-match meib-command-regexp input)
    (split-string (match-string 1 input) " " t)))
    ;; (let ((n-matches (1- (/ (length (match-data)) 2))))
    ;;   (mapcar (lambda (i) (match-string i input))
    ;; 			       (number-sequence 0 n-matches) ""))))
	
  
(defun meib (arg)
  "Connect to the servers in `meib-server-alist' or ARG.
If `meib-server-alist' is nil, also prompt for server
information. Note that if this is used with ARG, then throttling,
truncating, and anti-kick measures are not enabled."
  (interactive "P")
  (if (or arg (null meib-server-alist))	;If the user hasn't set their
					;alist, prompt him.
      (let* ((address (completing-read
		       "Address: " (or meib-server-alist meib-default-address)))
	     (server-plist (cdr (assoc-string address meib-server-alist)))
	     (port (completing-read
		    "Port: " (number-to-string (or (plist-get server-plist :port) meib-default-port))))
	     (use-tls (y-or-n-p "TLS? "))
	     (nick-name (completing-read
			 "Nickname: " (or (plist-get server-plist :nick-name) meib-default-nick-name)))
	     (user-name (completing-read
			 "Username: " (or (plist-get server-plist :user-name) meib-default-user-name)))
	     (real-name (completing-read
			 "Real name: " (or (plist-get server-plist :real-name) meib-default-real-name))))
	(meib-connect server (string-to-number port) use-tls nick-name user-name real-name))
    (dolist (server meib-server-alist)
      (let* ((address (car server))
	     (server-plist (cdr server))
	     (port (or (plist-get server-plist :port) meib-default-port))
	     (use-tls (or (plist-get server-plist :use-tls) meib-default-use-tls))
	     (nick-name (or (plist-get server-plist :nick-name) meib-default-nick-name))
	     (user-name (or (plist-get server-plist :user-name) meib-default-user-name))
	     (real-name (or (plist-get server-plist :real-name) meib-default-real-name)))
	(meib-connect address port use-tls nick-name user-name real-name)))))

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

(define-derived-mode meib-mode special-mode "MEIB"
  "Mode used by the MEIB buffers."
  (setq mode-name "MEIB")
  (setq major-mode 'meib-mode))

(defun meib-filter (process output)
  "Called when PROCESS recieves OUTPUT."
  (let ((server (assoc process meib-connected-server-alist))
	(lines (split-string output "[\r\n]" t)))
    (when (= ?\n (aref output (1- (length output))))
      (dolist (line lines)
	(meib-handle-server-response
	 process (meib-process-irc-message (replace-regexp-in-string "" "" output)))))))

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
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (message-channel (caddr (plist-get message :arguments)))
	 (channel (assoc-string message-channel (plist-get connected-server-plist :channels)))
	 (message-names (split-string (replace-regexp-in-string
				       "[+%&@]" "" (cadddr (plist-get message :arguments))) " " t)))
    (dolist (name message-names)
      (when (not (member name (plist-get (cdr channel) :users)))
	(plist-put (cdr channel) :users (append (plist-get (cdr channel) :users) `(,name)))))))

(defun meib-handler-JOIN (process message)
  "Handle MESSAGE from PROCESS if it's a JOIN.
After a JOIN command, we add the user to our list of users. If
it's our bot that JOINs the channel, we create a new queue timer
for the channel."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (message-channel (car (plist-get message :arguments)))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender)))
	 (channel (assoc-string message-channel (plist-get connected-server-plist :channels))))
    ;; If it's us that join, CHANNEL is definitely null, so no need to
    ;; check. 
    (when (string= nick-name (plist-get connected-server-plist :nick-name))
      (plist-put
       connected-server-plist
       :channels (append (plist-get connected-server-plist :channels) `((,message-channel :users (,nick-name))))))
    (plist-put (cdr channel) :users (append (plist-get (cdr channel) :users) `(,nick-name)))))

(defun meib-handler-PART (process message)
  "Handle MESSAGE from PROCESS if it's a PART.
After a PART command, we remove the user from our list of
users. If it's our bot that JOINs the channel, we remove the
queue timer for the channel."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (message-channel (car (plist-get message :arguments)))
	 (nick-name (meib-nick-name-from-full-name (plist-get message :sender)))
	 (channel (assoc-string message-channel (plist-get connected-server-plist :channels))))
    (when (string= nick-name (plist-get connected-server-plist :nick-name))
      (setq channel nil))		;Good enough.
    (plist-put (cdr channel) :users (delete nick-name (plist-get (cdr channel) :users)))))

(defun meib-handler-QUIT (process message)
  "Handle MESSAGE from PROCESS if it's a QUIT.
After a QUIT command, we do the same thing as in
`meib-handler-PART'."
  (meib-handler-PART process message))

(defun meib-handler-KICK (process message)
  "Handle MESSAGE from PROCESS if it's a KICK.
After a KICK command, we do the same thing as in
`meib-handler-PART'."
  (meib-handler-PART process message))
  
(defun meib-handler-NICK (process message)
  "Handle MESSAGE from PROCESS if it's a NICK.
After a NICK command, we change the username in ALL channels."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (message-new-username (car (plist-get message :arguments)))
	 (username (meib-nick-name-from-full-name (plist-get message :sender))))
    (dolist (channel (plist-get connected-server-plist :channels))
      (plist-put (cdr channel) :users (append (plist-get (cdr channel) :users) `(,message-new-username)))
      (plist-put (cdr channel) :users (delete username (plist-get (cdr channel) :users))))))

(defun meib-handler-PRIVMSG (process message)
  "Handle MESSAGE from PROCESS if it's a PRIVMSG.
In here, we just call the callbacks in
`meib-privmsg-callbacks'. These do whatever they want, from
handling commands, to parsing URLs."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (nick-name (plist-get connected-server-plist :nick-name)))
    ;; We don't want the bot to respond to itself.
    (when (not (string= (meib-nick-name-from-full-name (plist-get message :sender)) nick-name))
      (dolist (callback meib-privmsg-callbacks)
	(funcall callback process message)))))

(defun meib-nick-name-from-full-name (full-name)
  "Gets only the nickname from FULL-NAME.
That means it'll get `user' from `user!~user@vhost.example'"
  (car (split-string full-name "!")))
  
(defun meib-send-string (process string)
  "Send STRING to PROCESS.
Appends an `\r\n'."
  (process-send-string process (concat string "\r\n")))

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
    (let ((connected-server-plist (assoc process meib-connected-server-alist)))
      (when (timerp (plist-get connected-server-plist :queue-timer))
	(cancel-timer (plist-get connected-server-plist :queue-timer)))
      (when (not (null process))
      	(delete-process process))
      (with-current-buffer (plist-get (cdr connected-server-plist) :buffer)
	(setq mode-line-process ":disconnected"))
      (setq meib-connected-server-alist (delq connected-server-plist meib-connected-server-alist)))))

(provide 'meib)

;;; meib.el ends here
