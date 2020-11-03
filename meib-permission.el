;;; meib-permission.el --- MEIB permission command.

;;; Commentary:
;; TODO:
;;
;; o Perhaps make `meib-permission-users' have a default field?

;;; Code:

(defgroup meib-permission nil
  "MEIB permission command."
  :prefix "meib-permission-"
  :group 'meib)

(defcustom meib-permission-users
  '(("user1" . "is cool, therefore has a permission to speak."))
  "The users that have a permission to speak.
The permissions are in the form (USER DESCRIBING-PHRASE)."
  :group 'meib-permission
  :type '(repeat (cons :tag "Permission" (string :tag "Name")
		       (string :tag "Phrase used to describe the person"))))

(defcustom meib-permission-default
  "is someone I don't know, therefore has no permission to speak."
  "The default descriptor of an user not in
`meib-permission-users.'"
  :group 'meib-permission
  :type 'string)

(defun meib-permission (process message command-with-args)
  "Checks if someone has a permission to speak.
If no argument is provided, checks if YOU have a permission to
speak."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (channel-users (plist-get (cdr (assoc-string channel-name (plist-get (cdr (assoc process meib-connected-server-alist)) :channels))) :users))
	 (arg1 (car command-with-args))
	 (name (if arg1 arg1 (plist-get message :sender)))
	 (nick-name (meib-nick-name-from-full-name name))
	 (permission (cdr (assoc nick-name meib-permission-users))))
    (meib-privmsg process channel-name (format "Checking if %s has a permission to speak..." nick-name))
    (meib-privmsg process channel-name (format
					"According to my database, %s%s %s" nick-name
				        (if (member nick-name channel-users) "" " is not here, but")
					(if permission permission meib-permission-default)))))

(provide 'meib-permission)

;;; meib-permission.el ends here
