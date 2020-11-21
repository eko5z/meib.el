;;; meib-doctor.el --- MEIB doctor command.

;;; Commentary:
;; 

;;; Code:

(require 'doctor)

(defgroup meib-doctor nil
  "MEIB doctor command."
  :prefix "meib-doctor-"
  :group 'meib)

(defconst meib-doctor-buffer-name-format " *meib-doctor/%s:%s*"
  "The first string is the server address, the second the
nickname.")

(defun meib-doctor (process sender receiver arguments)
  "Consult a rogerian psychotherapist.
If you provide only one argument, `reset', then the
psychotherapist session is reset."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (address (plist-get connected-server-plist :address))
	 (buffer-name (format meib-doctor-buffer-name-format address sender))
	 (buffer (get-buffer-create buffer-name))
	 (arg (string-join arguments " ")))
    (if (and (= (length arguments) 1)
	     (string= (car arguments) "reset"))
	(with-current-buffer buffer
	  (set-buffer-modified-p nil)
	  (kill-buffer)
	  (meib-privmsg process receiver "Why are you leaving? Hmph. My secretary will send you a bill."))
      (with-current-buffer buffer-name
	(when (not (eq major-mode 'doctor-mode))
	  (doctor-mode))
	(insert arg)
	(doctor-ret-or-read 1)
	(doctor-ret-or-read 1)
	(let ((response (replace-regexp-in-string "\n" " " (car (last (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n\n" t))))))
	  (meib-privmsg process receiver response))))))
	
(provide 'meib-doctor)

;;; meib-doctor.el ends here
