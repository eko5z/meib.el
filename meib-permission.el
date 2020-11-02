;;; meib-permission.el --- MEIB permission command.

;;; Commentary:
;; 

;;; Code:

(defun meib-permission (process message command-with-args)
  "Checks if someone has a permission to speak."
  (let* ((channel (car (plist-get message :arguments)))
	 (arg1 (cadr command-with-args)))
    ))

(provide 'meib-permission)

;;; meib-permission.el ends here
