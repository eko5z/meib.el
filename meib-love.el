;;; meib-love.el --- MEIB love command.

;;; Commentary:
;; 

;;; Code:

(defgroup meib-love nil
  "MEIB love command."
  :prefix "meib-love-"
  :group 'meib)

(defun meib-love (process sender receiver arguments)
  "Checks how much two people are compatible with each other.
If one argument is provided, check if the person loves
themself. If two, check if one loves another."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (arg1 (car arguments))
	 (arg2 (cadr arguments)))
    (if (not arg2)
	(progn
	  (random (md5 (downcase arg1)))
	  (meib-privmsg process receiver (format "%s loves themself in... %s%%" (mp arg1 nil t) (mp (number-to-string (random 100)) t nil 13))))
      (random (md5 (downcase (concat arg1 arg2))))
      (meib-privmsg process receiver (format "%s loves %s in... %s%%" (mp arg1 nil t) (mp arg2 nil t)(mp (number-to-string (random 100)) t nil 13))))))
    
(provide 'meib-love)

;;; meib-love.el ends here
