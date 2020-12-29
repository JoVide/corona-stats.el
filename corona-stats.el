(require 'url)
(require 'json)

;;(outline-minor-mode t)

;; Om man vill spara en rå stats:
;;(url-copy-file "https://opendata.ecdc.europa.eu/covid19/casedistribution/json/" "./raw-stats.json" ) 


(defun my-corona-stats ()
  "Hämtar och visar uppdaterad coronastatistik.

Hämtar data från opendata.ecdc.europa.eu."
  (interactive)
  
  ;; Konstanter
  (setq stats-url "https://opendata.ecdc.europa.eu/covid19/casedistribution/json/")
  (setq file-path "./stats.json")
  (setq countries-list (list "Sweden" "Norway")) ; Lägg till länder här

  ;; Gör en hash-table för varje land:
  (setq countries-table (get-countries-table countries-list))

  ;; ## Gör eventuellt en totalt för hela världen eller olika världsdelar också

  ;; ## Kolla om vi har internet ens ##

  ;; ## lägg till koll som kolla om filen redan finns ##
  ;; ## uppdaterar måndagar, kan kolla det ##

  ;; ## Om inte redan uppdaterad finns: ##
  ;; Hämta json från stats-url
  (set-buffer (retrieve-corona-json stats-url))
  (replace-and-save-buffer (strip-json) file-path)

  ;; Plocka ut info från json
  (setq whole-json (get-whole-json file-path))
  (put-json-in-hash-table countries-table)

  (total-death-country countries-table)
  (death-this-week countries-table)
  (death-previous-week countries-table)
  (death-this-month countries-table)
  (death-previous-month countries-table)

  (total-cases-country countries-table)
  (cases-this-week countries-table)
  (cases-previous-week countries-table)
  (cases-this-month countries-table)
  (cases-previous-month countries-table)
  

  (print-all-but-json countries-table)
  ;(message "corona-stats-done")
  )

;;; ==================================================================
;;; SETUP
;;; ==================================================================
(defun get-countries-table (countries-list)
  "Returnerar en hash-table av countries-list."

  (setq countries-table (list))
  (dolist (country countries-list)
	(setq temp-hash-table (make-hash-table))
	(puthash 'country country temp-hash-table)
    (add-to-list 'countries-table temp-hash-table)
	)

  countries-table

  ;; Andra försök jag önskar hade fungerat:
  ;(print (arrayp (make-array '3))t)

  ;(setq countries-table (list))
  ;(dolist (country countries-list)
	;(add-to-list 'countries-table (list country (make-hash-table)))
	;)

  ;(dolist (country countries-list)
	;(dolist (element country)
	  ;(print element t)
	  ;)
	;(print (stringp country) t)
	;)

  )
(defun put-json-in-hash-table (countries-table)
  "Lägger json för varje land i hash-table"
  
  (dolist (country-hash countries-table)
	(setq country-name (gethash 'country country-hash))
	(setq country-json (get-country-as-json whole-json country-name))
	(puthash 'json country-json country-hash)
	)
  )

  
(defun print-all-but-json (countries-table)
  "För att printa allt utom json."

  (dolist (country-hash countries-table)
	
	(maphash (lambda (key value) (if (equal key 'json) () (print (format "%s : %s" key value) t))) country-hash)
	)
  )


;;; ==================================================================
;;; HÄMTA OCH STRIP JSON:
;;; ==================================================================

(defun retrieve-corona-json (url)
  "Hämta statistik i form av json från url."
  (setq request-result (url-retrieve-synchronously stats-url))
  )


(defun get-json-head ()
  "Få ut starten på json och byt ut [ med {."

  (goto-char (point-min))
  (re-search-forward "\\[")
  (delete-region (- (point) 1) (point))
  (insert "{")

  (goto-char (point-min))
  (re-search-forward "{" nil nil 3)
  (backward-char)
  (buffer-substring-no-properties (point-min) (point))
  )


(defun get-json-end ()
  "Få ut slutet på json och byt ut ] med }."

  (goto-char (point-max))
  (re-search-backward "\\]")
  (forward-char)
  (delete-region (- (point) 1) (point))
  (insert "}")

  (goto-char (point-max))
  (re-search-backward "}" nil nil 3)
  (forward-char)
  (buffer-substring-no-properties (point) (point-max))
  )


(defun get-country-from-json (country)
  "Hämta del av json för ett specifikt land.

Se också till att varje land får sin egen key i json."

  (setq country-start (get-country-start country))
  (setq country-end (get-country-end country))
  (setq data (buffer-substring-no-properties country-start country-end))

  
  ;;(concat "{\n" "\"" country "\"" " : [\n" data "\n]\n}")
  ;;(concat "\n\t" "\"" country "\"" " : [\n\t" data "\n\t]")
  ;;(concat "{\n" "\"" country "\"" " : [\n" data "\n]\n}")
  (concat "\"" country "\"" " : [\n" data "\n]")
  )


(defun get-country-start (country)
  "Hitta startpunkten för ett land"

  (goto-char (point-min))
  (re-search-forward country)
  (re-search-backward "{")
  (point)
  )


(defun get-country-end (country)
  "Hitta slutpunkten för ett land."

  (goto-char (point-max))
  (re-search-backward country)
  (re-search-forward "}")
  (point)
  )


(defun strip-json ()
  "Sortera bort allt man inte vill ha från json."

  (setq json-head (get-json-head))
  (setq json-end (get-json-end))
  (setq country-separation ",\n")

  ;; ## Ordna så det blir en lista med flera länder ##
  (setq sweden (get-country-from-json "Sweden"))
  (setq norway (get-country-from-json "Norway"))

  (message "%S" json-end)
  (concat json-head norway country-separation sweden json-end)
  )


(defun replace-and-save-buffer (stripped file-path)
  "Ta bort info i buffer och spara stripped json."
  (erase-buffer)

  (goto-char (point-min))
  (insert stripped)
  (write-file file-path nil)
  (kill-buffer)
  )

;;; ==================================================================
;;; PLOCKA UT STATISTIK FRÅN JSON:
;;; ==================================================================

(defun get-whole-json (file-path)
  "Läs in från file-path som json"
  (setq stats-buffer (find-file file-path))
  (set-buffer stats-buffer)

  (goto-char (point-min))
  (re-search-forward "{")
  (backward-char)
  (setq json (json-parse-buffer))

  (kill-buffer stats-buffer)

  json
  )


(defun get-country-as-json (whole-json country)
  "Returnerar bara country från whole-json."
 
  (gethash country (gethash "records" whole-json))
  )


(defun json-from-hash-table (countries-table country)
  "Returnerar json till country."
  
  (dolist (table countries-table)
	(if (equal country (gethash 'country table))
		(setq return (gethash 'json table))
	  )
  )
  return
  )

	

(defun total-death-country (countries-table)
  "Totalt antal dödsfall hittils, per land."

  (dolist (country-hash countries-table)
	(setq json (json-from-hash-table countries-table (gethash 'country country-hash)))
	(setq total-death 0)

	(dotimes (index (length json))
	  (setq week (aref json index))
	  (setq total-death (+ total-death (gethash "deaths_weekly" week)))
		)

	(puthash 'total-death total-death country-hash)
	)
 )


(defun death-nth-week-back (countries-table country n)
  "Döda n veckor tillbaka."

  (setq json (json-from-hash-table countries-table country))
  (setq week (aref json n))
  (gethash "deaths_weekly" week)

 )


;; För att köra lätt:

(defun death-this-week (countries-table)
  "Döda senaste veckan."

  (dolist (country-hash countries-table)
	(setq death-this-week (death-nth-week-back countries-table (gethash 'country country-hash) 0))
	(puthash 'death-this-week death-this-week country-hash)
	)

  )


(defun death-previous-week (countries-table)
  "Döda förra veckan."

  (dolist (country-hash countries-table)
	(setq death-this-week (death-nth-week-back countries-table (gethash 'country country-hash) 1))
	(puthash 'death-previous-week death-this-week country-hash)
	)
  )


(defun death-this-month (countries-table)
  "Antal döda senaste månaden."

  (dolist (country-hash countries-table)
	(setq death-total 0)
	(dotimes (index 4)
	  (setq death-this-week (death-nth-week-back countries-table (gethash 'country country-hash) index))
	  (setq death-total (+ death-total death-this-week))
	  )
	(puthash 'death-this-month death-total country-hash)
	)
  )


(defun death-previous-month (countries-table)
  "Antal döda förra månaden."

  (dolist (country-hash countries-table)
	(setq death-total 0)
	(dotimes (index 4)
	  (setq death-this-week (death-nth-week-back countries-table (gethash 'country country-hash) (+ index 4)))
	  (setq death-total (+ death-total death-this-week))
	  )
	(puthash 'death-previous-month death-total country-hash)
	)
  )



(defun total-cases-country (countries-table)
  "Totalt antal fall hittils, per land."

  (dolist (country-hash countries-table)
	(setq json (json-from-hash-table countries-table (gethash 'country country-hash)))
	(setq total-cases 0)

	(dotimes (index (length json))
	  (setq week (aref json index))
	  (setq total-cases (+ total-cases (gethash "cases_weekly" week)))
		)

	(puthash 'total-cases total-cases country-hash)
	)
 )


(defun cases-nth-week-back (countries-table country n)
  "Antall fall n veckor tillbaka."

  (setq json (json-from-hash-table countries-table country))
  (setq week (aref json n))
  (gethash "cases_weekly" week)

 )



(defun cases-this-week (countries-table)
  "Antal fall senaste veckan."

  (dolist (country-hash countries-table)
	(setq cases-this-week (cases-nth-week-back countries-table (gethash 'country country-hash) 0))
	(puthash 'cases-this-week cases-this-week country-hash)
	)

  )


(defun cases-previous-week (countries-table)
  "Antal fall senaste veckan."

  (dolist (country-hash countries-table)
	(setq cases-this-week (cases-nth-week-back countries-table (gethash 'country country-hash) 1))
	(puthash 'cases-previous-week cases-this-week country-hash)
	)

  )


(defun cases-this-month (countries-table)
  "Antal fall den senaste månaden."

  (dolist (country-hash countries-table)
	(setq cases-total 0)
	(dotimes (index 4)
	  (setq cases-this-week (cases-nth-week-back countries-table (gethash 'country country-hash) index))
	  (setq cases-total (+ cases-total cases-this-week))
	  )
	(puthash 'cases-this-month cases-total country-hash)
	)
  )


;; För att köra lätt:
(defun cases-previous-month (countries-table)
  "Antal fall den senaste månaden."

  (dolist (country-hash countries-table)
	(setq cases-total 0)
	(dotimes (index 4)
	  (setq cases-this-week (cases-nth-week-back countries-table (gethash 'country country-hash) (+ index 4)))
	  (setq cases-total (+ cases-total cases-this-week))
	  )
	(puthash 'cases-previous-month cases-total country-hash)
	)
  )


;; För att köra lätt:
(my-corona-stats)
