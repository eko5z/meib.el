;;; meib-spook.el --- MEIB M-x spook and similars.

;;; Commentary:
;;

;;; Code:

(require 'subr-x)

(defgroup meib-spook nil
  "MEIB M-x spook and similars."
  :prefix "meib-spook-"
  :group 'meib)

(defcustom meib-spook-collection-alist
  `(("spook" nil ,(append (snarf-spooks) nil))
    ("trump" t ("fake news" "MSM" "bigly" "sad" "snowflake" "drain the swamp"
    		"virtue signaling" "sleepy Joe" "Obama" "Biden" "Crooked Hillary"
    		"patriot" "establishment" "corrupt" "Make America Great Again"
    		"The American Dream" "leftist" "radical" "China" "Chinese flu"
    		"Second Amendment" "First Amendment" "globalist" "big tech"
    		"covfefe" "crooked" "big league" "dopey" "goofy" "disaster"
    		"chaos" "clown" "terror" "the African Americans" "the blacks"
    		"the Latinos" "the Women" "believe me" "many, many friends"
    		"let's face it" "terrorist" "coronavirus" "El Corona" "low-energy"
		"best" "brilliant" "fabulous" "fantastic" "honest" "incredible"
		"powerful" "strong" "unbelievable" "boring" "complete and total"
		"disgusting" "dishonest" "dumb" "pathetic" "overrated"
		"ridiculous" "huge" "massive" "big" "major" "many" "numerous"
		"staggering" "vast" "absolutely" "badly" "extremely" "frankly"
		"incredibly" "totally" "truly" "unbelievably" "incompetence"
		"loser" "lowlife" "moron" "phonies" "crooks" "I must tell you"
		"in the whole world" "likes of which" "never seen before"
		"ever" "not gonna happen" "that I can tell you" "the Mexicans"
		"the Chinese" "the North Koreans" "North Korea" "silent majority"
		"many such cases" "vote" "hoax" "conspiracy" "GDP" "33.1%"
		"11.4 million" "jobs" "dollars" "a small loan" "taxes"
		"the suburbs" "CNN" "American" "anti-American" "workers"
		"the press" "ban" "Great Red Wave" "Russia" "Putin"
		"Kim Jong-Un" "the Hispanic Americans" "lockdown" "economy"
		"vaccine" "4 more years" "healthcare" "the Biden Lockdown"
		"Mueller" "MAGA" "the Asian Americans" "folks" "impeach")))
  "Collections of words used by meib-spook.
Collections are in the form (COLLECTION USE-RANDOM-VERBS (WORDS))."
  :group 'meib-spook
  :type '(repeat (list (string :tag "Collection") (boolean :tag "Use random verbs") (repeat (string :tag "Word")))))

(defvar meib-spook-verbs '(" is" " are" " were" " was" "," "." "!" "?" " I"
			   " you" " they" " we" "'s" "'re" " I'm" ";" ":" " no"
			   "'ll" " will" " shall"))

(defun meib-spook-shuffle (words &optional n)
  "Shuffles around WORDS and returns N random words."
  (mapcar (lambda (i) (nth (random (length words)) words)) (number-sequence 0 (or n 50))))

(defun meib-spook-shuffle-with-verbs (words &optional n)
  "Suffles around WORDS and returns N random words.
The WORDS are mixed in with `meib-spook-verbs'."
  (mapcar (lambda (i)
	    (if (> (random 100) 40)
		(nth (random (length words)) words)
	      (concat (nth (random (length words)) words)
		      (nth (random (length meib-spook-verbs)) meib-spook-verbs))))
	  (number-sequence 0 (or n 50))))

(defun meib-spook (process message arguments)
  "Generates a random mumble of words from a collection.
The default collection is `spook'. Available collections:
`spook', `trump'."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (arg1 (car arguments))
	 (database (assoc arg1 meib-spook-collection-alist)))
    (if database
	(let* ((words (caddr database))
	       (with-verbs (cadr database)))
	  (if with-verbs
	      (meib-privmsg process channel-name (string-join (meib-spook-shuffle-with-verbs words (random 30)) " "))
	    (meib-privmsg process channel-name (string-join (meib-spook-shuffle words (random 30)) " "))))
      (let* ((words (caddr (assoc "spook" meib-spook-collection-alist))))
	(meib-privmsg process channel-name (string-join (meib-spook-shuffle words (random 30)) " "))))))

(provide 'meib-spook)

;;; meib-spook.el ends here
