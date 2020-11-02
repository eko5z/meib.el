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
  '(("user1" . "has a permission to speak.")
    ("user2" . "has a permission to speak."))
  "The users that have a permission to speak.
The permissions are in the form (PERMISSION DESCRIBING-PHRASE)."
  :group 'meib-permission
  :type '(repeat (cons :tag "Permission" (string :tag "Name")
		       (string :tag "Phrase used to describe the person"))))

(defcustom meib-permission-default
  "has no permission to speak."
  "The default descriptor of an user not in
`meib-permission-users.'"
  :group 'meib-permission
  :type 'string)

(defun meib-permission (process message command-with-args)
  "Checks if someone has a permission to speak."
  (let* ((channel (car (plist-get message :arguments)))
	 (arg1 (car command-with-args))
	 (name (if arg1 arg1 (plist-get message :sender)))
	 (permission (cdr (assoc (meib-nick-name-from-full-name name) meib-permission-users))))
    (meib-privmsg process channel (format "Checking if %s has a permission to speak..."
					  (meib-nick-name-from-full-name name)))
    (meib-privmsg process channel (format "%s %s" (meib-nick-name-from-full-name name)
					  (if permission permission meib-permission-default)))))

(provide 'meib-permission)

;;; meib-permission.el ends here
