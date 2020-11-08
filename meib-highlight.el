;;; meib-highlight.el --- MEIB highlight command.

;;; Commentary:
;; 

;;; Code:

(defgroup meib-highlight nil
  "MEIB highlight command."
  :prefix "meib-highlight-"
  :group 'meib)

(defcustom meib-highlight-users-per-line 3
  "Number of users to highlight per line."
  :group 'meib-highlight
  :type 'integer)

(defun meib-highlight-n-at-a-time (remaining-users n func)
  "Iterate over N elements of REMAINING-USERS, and apply FUNC."
  (when remaining-users
    (funcall func (last remaining-users n))
    (meib-highlight-n-at-a-time (butlast remaining-users n) n func)))

(defun meib-highlight (process message arguments)
  "Highlights all the users of the channel."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (channel-users
	  (plist-get (cdr (assoc-string channel-name (plist-get connected-server-plist :channels))) :users)))
    (meib-highlight-n-at-a-time
     channel-users meib-highlight-users-per-line
     (lambda (users)
       (meib-privmsg process channel-name (string-join users " "))))))

(provide 'meib-highlight)

;;; meib-highlight.el ends here
