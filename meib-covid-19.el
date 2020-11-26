;;; meib-covid-19.el --- MEIB covid-19 command.

;;; Commentary:
;; Completely asynchronous. Quite cool, I must say.

;;; Code:

(require 'json)

(defgroup meib-covid-19 nil
  "MEIB covid-19 command."
  :prefix "meib-covid-19-"
  :group 'meib)

(defconst meib-covid-19-api-url "https://disease.sh/v3/covid-19/"
  "The URL to the COVID-19 API.")

(defun meib-covid-19-group-number (number)
  "Add spaces to NUMBER and return it as a string."
  (let ((num (number-to-string number)))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat 
		 (match-string 1 num) " "
		 (match-string 2 num))))
    num))

(defun meib-covid-19-url-callback (status process channel-name)
  (let* ((contents (cadr (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n\n")))
	 (json (json-read-from-string contents))
	 (country (cdr (assoc 'country json)))
	 (population (cdr (assoc 'population json)))
	 (cases (cdr (assoc 'cases json)))
	 (cases-today (cdr (assoc 'todayCases json)))
	 (deaths (cdr (assoc 'deaths json)))
	 (deaths-today (cdr (assoc 'todayDeaths json)))
	 (one-case-per (cdr (assoc 'oneCasePerPeople json)))
	 (one-death-per (cdr (assoc 'oneDeathPerPeople json))))
    (if cases
	(meib-privmsg
	 process channel-name
	 (format "In %s (%s people), there are %s (+%s) cases, and %s (+%s) deaths. 1 in %s are infected, 1 in %s are dead."
		 (mp (if country country "the World") nil t) (mp (meib-covid-19-group-number population) t)
		 (mp (meib-covid-19-group-number cases) t nil 3) (mp (meib-covid-19-group-number cases-today) t t 3)
		 (mp (meib-covid-19-group-number deaths) t nil 5) (mp (meib-covid-19-group-number deaths-today) t t 5)
		 (mp (meib-covid-19-group-number one-case-per) t nil 3) (mp (meib-covid-19-group-number one-death-per) t nil 5)))
      (meib-privmsg process channel-name "Either the country doesn't exist, or there was an error."))))

(defun meib-covid-19 (process sender receiver arguments)
  "Returns information about the COVID-19 pandemic.
If an argument is provided, return information about that
country. It's best if the argument is ISO2 or ISO3 in format."
  (let* ((arg1 (car arguments)))
    (if arg1
	(url-retrieve (concat meib-covid-19-api-url "countries/" (downcase arg1))
		      'meib-covid-19-url-callback `(,process ,receiver) t t)
      (url-retrieve (concat meib-covid-19-api-url "all")
		    'meib-covid-19-url-callback `(,process ,receiver) t t))))

(provide 'meib-covid-19)

;;; meib-covid-19.el ends here
