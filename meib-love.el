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
themself. If two, check if one loves another. If more, check if
they love themselves as a group."
  (let* ((connected-server-plist (cdr (assoc process meib-connected-server-alist)))
	 (arg1 (car arguments))
	 (arg2 (cadr arguments)))
    (if (> (length arguments) 2)
	(let ((names (concat (mapconcat (lambda (name) name) (butlast arguments) ", ") " and " (car (last arguments)))))
	  (random (md5 (downcase (concat arg1 arg2))))
	  (meib-privmsg process receiver (format "%s love themselves in... %d%%" names (random 100))))
      (if (not arg2)
	  (progn
	    (random (md5 (downcase arg1)))
	    (meib-privmsg process receiver (format "%s loves themself in... %d%%" arg1 (random 100))))
	(random (md5 (downcase (concat arg1 arg2))))
	(meib-privmsg process receiver (format "%s loves %s in... %d%%" arg1 arg2 (random 100)))))))
    
(provide 'meib-love)

;;; meib-love.el ends here
