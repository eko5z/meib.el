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
   (("fredy perlman"
     ("Every time we take a step we're surrounded by the ideological birds of prey who feed on our possibilities, fill themselves with concepts of our desires and reenslave us with beautiful combinations of words which seem to depict the world we failed to realize." . "Letters of Insurgents (1976)")
     ("Trade is very old. In the state of nature, trade is something people do to their enemies. They don't trade with kin." . "Against His-Story, Against Leviathan! (1983)")
     ("The tame, the domesticated, try to monopolize the word freedom; they'd like to apply it to their own condition. They apply the word 'wild' to the free." . "Against His-Story, Against Leviathan! (1983)")
     ("A time-and-motion engineer watching a bear near a berry patch would not know when to punch his clock. Does the bear start working when he walks to the berry patch, when he picks the berry, when he opens his jaws? If the engineer has half a brain he might say the bear makes no distinction between work and play. If the engineer has an imagination he might say that the bear experiences joy from the moment the berries turn deep red, and that none of the bear’s motions are work." . "Against His-Story, Against Leviathan! (1983)")
     )
    ("jean baudrillard"
     ("The need to speak, even if one has nothing to say, becomes more pressing when one has nothing to say, just as the will to live becomes more urgent when life has lost its meaning." . "The Ecstasy of Communication (1987)")
     ("It is the real, and not the map, whose vestiges persist here and there in the deserts that are no longer those of the Empire, but ours: The desert of the real itself." . "Simulacra and Simulation (1981)")
     ("When the real is no longer what it was, nostalgia assumes its full meaning." . "Simulacra and Simulation (1981)")
     ("This world wants to be childish in order to make us believe that the adults are elsewhere, in the 'real' world, and to conceal the fact that true childishness is everywhere - that it is that of the adults themselves who come here to act the child in order to foster illusions as to their real childishness." . "Simulacra and Simulation (1981)")
     ("People no longer look at each other, but there are institutes for that. They no longer touch each other, but there is contactotherapy. They no longer walk, but they go jogging, etc. Everywhere one recycles lost faculties, or lost bodies, or lost sociality, or the lost taste for food." . "Simulacra and Simulation (1981)")
     ("Forgetting extermination is part of extermination, because it is also the extermination of memory, of history, of the social, etc. This forgetting is as essential as the event in any case unlocatable by us, inaccessible to us in its truth. This forgetting is still too dangerous, it must be effaced by an artificial memory (today, everywhere, it is artificial memories that effect the memory of man, that efface man in his own memory). This artificial memory will be the restaging of extermination - but late, much too late for it to be able to make real waves and profoundly disturb something, and especially, especially through medium that is itself cold, radiating forgetfulness, deterrence, and extermination in a still more systematic way, if that is possible, than the camps themselves." . "Simulacra and Simulation (1981)")
     ("One has never said better how much 'humanism', 'normality', 'quality of life' were nothing but the vicissitudes of profitability." . "Simulacra and Simulation (1981)")
     ("The simulacrum now hides, not the truth, but the fact that there is none, that is to say, the continuation of Nothingness." . "Radical Thought (1994)")
     ("So-called 'realist' photography does not capture the 'what is.' Instead, it is preoccupied with what should not be, like the reality of suffering for example." . "Photography, or the Writing of Light (2000)")
     ("It is perhaps not a surprise that photography developed as a technological medium in the industrial age, when reality started to disappear. It is even perhaps the disappearance of reality that triggered this technical form. Reality found a way to mutate into an image." . "Photography, or the Writing of Light (2000)"))
    ("american psycho"
     ("Well... I am the American Psycho after all." . "Bateman to Van Patten at the brink of his death")
     ("Wait, Harold. What do you mean? I'm not the American Psycho?" . "Bateman to Harold")
     ("Let's see the American Psycho's card." . "Paul Allen to Bryce") ;I had to.
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

(defun meib-quotes (process sender receiver arguments)
  "Shows a random quote from a given collection.
Collections available: `american psycho', `fredy perlman', `jean
baudrillard'."
  (let* ((arg1 (downcase (string-join arguments " ")))
	 (quote-collection (cdr (assoc arg1 meib-quotes-collection-alist)))
	 (quote-full (nth (random (length quote-collection)) quote-collection))
	 (quote_ (car quote-full))
	 (author (cdr quote-full)))
    (if quote_
	(meib-privmsg process receiver (format "%s --- %s" (mp quote_ nil t) (mp author t)))
      (let* ((random-collection (nth (random (length meib-quotes-collection-alist)) meib-quotes-collection-alist))
	     (collection-name (car random-collection))
	     (random-quote (nth (length (cdr random-collection)) random-collection))
	     (quote_ (car random-quote))
	     (author (cdr random-quote)))
	(meib-privmsg process receiver (format "%s --- %s (%s)" (mp quote_ nil t) (mp author t) (mp (capitalize collection-name) t t)))))))

(provide 'meib-quotes)

;;; meib-quotes.el ends here
