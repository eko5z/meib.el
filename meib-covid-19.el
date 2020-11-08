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
  (let ((num (number-to-string number))
	(op " "))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat 
		 (match-string 1 num) op
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
	 (recovered (cdr (assoc 'recovered json)))
	 (recovered-today (cdr (assoc 'todayRecovered json)))
	 (one-case-per (cdr (assoc 'oneCasePerPeople json)))
	 (one-death-per (cdr (assoc 'oneDeathPerPeople json))))
    (if cases
	(meib-privmsg
	 process channel-name
	 (format "In %s (%s people), there are %s (+%s) cases, %s (+%s) deaths, and %s (+%s) recovered. 1 in %s are infected, 1 in %s are dead."
		 (if country country "the World") (meib-covid-19-group-number population)
		 (meib-covid-19-group-number cases) (meib-covid-19-group-number cases-today)
		 (meib-covid-19-group-number deaths) (meib-covid-19-group-number deaths-today)
		 (meib-covid-19-group-number recovered) (meib-covid-19-group-number recovered-today)
		 (meib-covid-19-group-number one-case-per) (meib-covid-19-group-number one-death-per)))
      (meib-privmsg process channel-name "Either the country doesn't exist, or there was an error."))))

(defun meib-covid-19 (process message arguments)
  "Returns information about the COVID-19 pandemic.
If an argument is provided, return information about that
country. It's best if the argument is ISO2 or ISO3 in format."
  (let* ((channel-name (car (plist-get message :arguments)))
	 (arg1 (car arguments)))
    (if arg1
	(url-retrieve (concat meib-covid-19-api-url "countries/" (downcase arg1))
		      'meib-covid-19-url-callback `(,process ,channel-name) t t)
      (url-retrieve (concat meib-covid-19-api-url "all")
		    'meib-covid-19-url-callback `(,process ,channel-name) t t))))

(provide 'meib-covid-19)

;;; meib-covid-19.el ends here
