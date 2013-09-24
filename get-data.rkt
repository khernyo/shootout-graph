
#lang racket

(require net/url
	 (planet neil/html-parsing:2:0)
	 (planet clements/sxml2:1:=3))

(define summary-data-url
  (string->url "http://benchmarksgame.alioth.debian.org/u32/summarydata.php"))

(define (get-summary-data url)
  (call/input-url url get-pure-port html->xexp))

(define div-summarydata
  ((sxpath '(// (div (@ (equal? (id "summarydata"))))))
   (get-summary-data summary-data-url)))

(define p-summarydata
  ((sxpath '(// (p 1))) div-summarydata))

(define summarydata-lines
  ((select-kids string?) p-summarydata))

(call-with-output-file "shootout-summary.csv"
  #:exists 'truncate/replace
  (lambda (csv-file)
    (display (string-join summarydata-lines "\n") csv-file)
    (newline csv-file)))
