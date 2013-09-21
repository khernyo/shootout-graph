;; Based on http://gmarceau.qc.ca/files/shootout.ss by Guillaume Marceau
;; Originally published at:
;; http://blog.gmarceau.qc.ca/2009/05/speed-size-and-dependability-of.html

#lang scheme
(require (planet neil/csv:1:5/csv)
         #;(planet soegaard/sqlite:1:2/sqlite)
         srfi/1
         (rename-in srfi/26 [cut //])
         scheme/match
         slideshow/pict
         scheme/gui/base
         (prefix-in t: (planet soegaard/galore:4/table))
         (prefix-in s: (planet soegaard/galore:4/set)))

(define (maybe-string->number v)
  (or (string->number v) v))

(define (.. t k [failure-thunk (lambda () #f)] [success-function (lambda (v) v)])
  (t:lookup k t failure-thunk success-function))
(define (!! t k v) (t:insert k v t))
(define (!!/f t k fn [failure-thunk (lambda () #f)])
  (t:insert k (fn (t:lookup k t failure-thunk)) t))

(define (filter-empty-rows lst)
  (filter-not (// equal? <> '("")) lst))

(define (open-file filename)
  (define data-lst
    (csv->list (open-input-file filename)))
  
  (define headers (map string->symbol (first data-lst)))
  (define data (map (// map maybe-string->number <>)
                    (filter-empty-rows (rest data-lst))))
  
  (map t:sexp->eq (map (// zip headers <>) data)))

(define (row->exclude row) (list (.. row 'test) (.. row 'lang) (.. row 'id)))

(define data (open-file "C:\\Mes documents\\diddles\\shootout-summary.csv"))

(define (collect data name)
  (for/fold ([result (t:make-equal)])
    ([b (remove-duplicates (map (// .. <> name) data))])
    (!! result b (filter (lambda (row) (equal? (.. row name) b)) data))))


(define benchmarks (collect data 'name))

(define (normalize row name factor)
  (!!/f row name (// / <> factor)))

(define (normalize-sub1 row name factor)
  (!!/f row name (lambda (v) (- (/ v factor) 1))))

(define (x-field rows min/max name)
  (apply min/max (map (// .. <> name) rows)))

(define (normalize-accross rows name)
  (define mx (x-field rows min name))
  (map (// normalize-sub1 <> name mx) rows))

(define (normalize-benchmark data)
  (normalize-accross (normalize-accross data '|cpu(s)|) '|size(B)|))

(define scale-cpu-factor 1/80)

(define normalized (append* (t:values (t:map/value normalize-benchmark benchmarks))))

(define percentile-cpu-in-frame 0.80)
(define percentile-size-in-frame 0.90)
(define (->int v) (inexact->exact (floor v)))

(define (percentile lst p less-than #:key [key (lambda (v) v)])
  (list-ref (sort lst less-than #:key key)
            (->int (* p (length lst)))))

(define (percentile-field lst p name)
  (.. (percentile lst p < #:key (// .. <> name)) name))

(define (normalize-against-percentile rows p name)
  (define factor (percentile-field rows p name))
  (map (// normalize <> name factor) rows))

(define rescaled-to-fit (normalize-against-percentile
                         (normalize-against-percentile normalized percentile-cpu-in-frame '|cpu(s)|)
                         percentile-size-in-frame '|size(B)|))

(define languages (collect rescaled-to-fit 'lang))

(define width 95)
(define height width)
(define margin 20)
(define half-margin (/ margin 2))
(define front-color "DimGray")
(define context-color "Thistle")
(define dot-size 0.02)
(define star-width 0.03)
(define text-size 10)

(define (average lst) (/ (apply + lst) (length lst)))

(define (d-line x1 y1 x2 y2)
  (pin-over (blank 1 1)
            x1 y1
            (linewidth star-width (pip-line (- x2 x1) (- y2 y1) 0))))

(define (d-star data)
  (let ([ax (average (map first data))]
        [ay (average (map second data))])
    (for/fold ([result (blank 1 1)])
      ([row data])
      (match row
        [(list x1 y1)
         (pin-over result 0 0 (d-line x1 y1 ax ay))]))))

(define (d-dots data)
  (for/fold ([result (blank 1 1)])
    ([row data])
    (match row
      [(list x1 y1)
       (pin-over result x1 y1 (disk dot-size))])))

(define (scale-to pic width height)
  (if (or (= 0 (pict-width pic))
          (= 0 (pict-height pic)))
      (blank width height)
      (scale pic
             (/ width (pict-width pic))
             (/ height (pict-height pic)))))

(define (margin&frame pic width height margin)
  (lt-superimpose (frame (clip #;(inset (scale-to pic width height) margin)
                               (scale-to pic width height))
                         #:color context-color)))

(define (to-xy data)
  (for/list ([row data])
    (list (.. row '|cpu(s)|) (- 1 (.. row '|size(B)|)))))


(define (d-language data dd-context width height margin)
  (margin&frame (pin-over dd-context 0 0 (colorize (d-star (to-xy data)) front-color)) width height margin))

(define (language-speed lang-name) (average (map (// .. <> '|cpu(s)|) (.. languages lang-name))))
(define (language-size lang-name) (average (map (// .. <> '|size(B)|) (.. languages lang-name))))

(define sorted-by-speed
  (map first (sort (t:to-sexp (t:map/key language-speed languages))
                   < #:key second #:cache-keys? #t)))

(define (square-list lst)
  (let ([s (inexact->exact (ceiling (sqrt (length lst))))])
    (let loop ([lst lst] [size (length lst)] [result empty])
      (if (<= size s)
          (reverse (cons lst result))
          (loop (drop lst s) (- size s) (cons (take lst s) result))))))

(define squared-by-size
  (for/list ([col (square-list sorted-by-speed)])
    (sort col < #:key language-size #:cache-keys? #t)))

(dc-for-text-size (new bitmap-dc% [bitmap (make-object bitmap% 64 64)]))

(define (add-text str pic)
  (vc-append half-margin (text str null text-size) pic))

(define (square-to-grid square margin)
  (apply hb-append margin
         (map (// apply vc-append margin <>)
              (map reverse square))))

(define (draw)
  (define dd-context (colorize (d-dots (to-xy rescaled-to-fit)) context-color))
  (define dd-languages (t:map add-text (t:map/value (// d-language <> dd-context width height margin) languages)))
  (square-to-grid (map (// map (// .. dd-languages <>) <>) squared-by-size) margin))

(draw)

(define functionals
  (s:list->equal '("ATS"
                   "Clean"
                   "Erlang HiPE"
                   "F# Mono"
                   "Haskell GHC"
                   "Lisp SBCL"
                   "Lua"
                   "Lua LuaJIT"
                   "Mozart/Oz"
                   "OCaml"
                   "Scala"
                   "Scheme PLT"
                   "Smalltalk VisualWorks")))

(define c-and-friends
  (s:list->equal '("C GNU gcc"
                   "C++ GNU g++")))

(define imperatives
  (s:list->equal '("Ada 2005 GNAT"
                   "C# Mono"
                   "Fortran Intel"
                   "Java 6 -Xint"
                   "Java 6 -server"
                   "*Java 6 steady state"
                   "JavaScript TraceMonkey"
                   "JavaScript V8"
                   "Lisaac"
                   "PHP"
                   "Pascal Free Pascal"
                   "Perl"
                   "Python"
                   "Python 3"
                   "Python pypy"
                   "Ruby"
                   "Ruby 1.9"
                   "Ruby JRuby")))

#|
(define functionals
  (s:list->equal '("cmucl"
                   "sbcl"
                  "swiprolog"
                  "scala"
                  "ghc"
                  "clean"
                  "fsharp"
                  "chicken"
                  "ruby"
                  "stalin"
                  "squeak"
                  "ats"
                  "nice"
                  "ocaml"
                  "mlton"
                  "erlang"
                  "mercury"
                  "smlnj"
                  "gambit"
                  "cal"
                  "lua"
                  "luajit"
                  "vw"
                  "ikarus"
                  "oz"
                  "hipe"
                  "mzscheme")))

(define imperatives
  (s:list->equal '("fpascal"
                   "javaxint"
                   "javaclient"
                   "jruby"
                   "ifc"
                   "pike"
                   "ooc"
                   "yarv"
                   "gforth"
                   "csharp"
                   "perl"
                   "groovy"
                   "rhino"
                   "znn"
                   "gcj"
                   "icon"
                   "iron"
                   "tcl"
                   "gst"
                   "php"
                   "yap"
                   "se"
                   "io"
                   "gnat"
                   "occam"
                   "java14"
                   "javascript"
                   "g95"
                   "pnet"
                   "stx"
                   "fbasic"
                   "java"
                   "python"
                   "regina"
                   "javaxx"
                   "psyco"
                   "rebol"
                  "bigforth"
                  "lisaac"
                  "csharpaot"
                  "python3"
                  "pypy"
                  "javasteady"
                  "v8"
                  "tracemonkey")))

(define c-and-friends
  (s:list->equal '("cint"
                   "gcc"
                   "icc"
                   "icpp"
                   "gpp"
                   "dlang")))
|#

(define square-size 35)

(define green-rect (colorize (filled-rectangle 1 1) "PaleGreen"))
(define blue-rect (colorize (filled-rectangle 1 1) "SkyBlue"))
(define gray-rect (colorize (filled-rectangle 1 1) "Gray"))

(define (d-small-color-star lang)
  (add-text lang
            (d-language (.. languages lang)
                        (cond [(s:member? lang functionals) green-rect]
                              [(s:member? lang imperatives) gray-rect]
                              [(s:member? lang c-and-friends) blue-rect]
                              [else (error (format "missing language: ~a" lang))])
                        square-size square-size 0)))

(inset (square-to-grid (map (// map d-small-color-star <>) squared-by-size) (/ margin 2))
       margin)

