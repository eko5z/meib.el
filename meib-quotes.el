;;; meib-quotes.el --- MEIB quotes command.

;;; Commentary:
;; 
;; o Add The Shining quotes.

;;; Code:

(defgroup meib-quotes nil
  "MEIB quotes command."
  :prefix "meib-quotes-"
  :group 'meib)

(defcustom meib-quotes-collection-alist
  (quote
   (("american psycho"
     ("Well... I am the American Psycho after all." . "Bateman to Van Patten at the brink of his death")
     ("Wait, Harold. What do you mean? I'm not the American Psycho?" . "Bateman to Harold")
     ("Let's see the American Psycho's card." . "Paul Allen to Bryce")
     ("I'm into uh... well murders and executions mostly." . "Bateman to Daisy")
     ("Well, we have to end apartheid for one. And slow down the nuclear arms race. Stop terrorism and world hunger. We have to provide food and shelter for the homeless, and oppose racial discrimination and promote civil rights, while also promoting equal rights for women. We have to encourage a return to traditional moral values. Most importantly, we have to promote general social concern and less materialism in young people." . "Bateman")
     ("Did you know that Ted Bundy's first dog was a collie named Lassie?" . "Bateman to Jean")
     ("Did you know that Whitney Houston's debut LP, called simply Whitney Houston had 4 number one singles on it? Did you know that, Christie?" . "Bateman to Christie")
     ("My need to engage in homicidal behavior on a ma-ha-ssive scale cannot be corrected but I have no other way to fulfill my needs." . "Bateman to Evelyn")
     ("Duct tape. I need it for... taping something." . "Bateman")
     ("No just uh... cool it with the anti-Semitic remarks." . "Bateman")
     ("I have to return some videotapes." . "Bateman")
     ("You're not terribly important to me." . "Bateman")
     ("No can do. I've got an 8:30 res at Dorsia---great. Sea urchin ceviche." . "Allen to Bryce")
     ("Is that a gram?" . "Bryce")
     ("I can't believe that Bryce prefers Van Patten's card to mine." . "Bateman")
     ("Let's see Paul Allen's card." . "Bateman")
     ("Look at that subtle off-white coloring. The tasteful thickness of it. Oh my god, it even has a watermark." . "Bateman")
     ("TRY GETTING A RESERVATION AT DORSIA NOW YOU FUCKING STUPID BASTARD! YOU, FUCKING BASTARD!" . "Bateman"))))
  "Collections of quotes used by `meib-quotes'.
A collection is in the form (STRING . (QUOTE1 ...))."
  :type '(repeat (cons :tag "Collection" (string :tag "Name") (repeat :tag "Quotes" (cons (string :tag "Quote") (string :tag "Author"))))))

(defun meib-quotes (process message arguments)
  "Shows a random quote from a given collection.
Collections available: `american psycho'."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (arg1 (downcase (string-join arguments " ")))
	 (quote-collection (cdr (assoc arg1 meib-quotes-collection-alist)))
	 (quote-full (nth (random (length quote-collection)) quote-collection))
	 (quote_ (car quote-full))
	 (author (cdr quote-full)))
    (if quote_
	(meib-privmsg process channel-name (format "\"%s\" --- %s" quote_ author))
      (let* ((random-collection (nth (random meib-quotes-collection-alist) meib-quotes-collection-alist))
	     (collection-name (car random-collection))
	     (random-quote (nth (length (cdr random-collection)) random-collection))
	     (quote_ (car random-quote))
	     (author (cdr random-quote)))
	(meib-privmsg process channel-name (format "\"%s\" --- %s (%s)" quote_ author (capitalize collection-name)))))))

(provide 'meib-quotes)

;;; meib-quotes.el ends here
