#lang racket
(require srfi/13/string)

(define (string-reverse s)
  (let loop ([v (string->list s)]
             [answer '()])
    (if (empty? v)
        (list->string answer)
        (loop (cdr v) (cons (car v) answer)))))

(define append
  ; (append n lst): N X L -> L
  ; appends a value to end of list
  (lambda (n lst)
    (foldr cons (list n) lst)))
(define (trim s)
  ;  "It returns a string without all spaces, newlines and tabs in its extremes, this functions doesn't make nothing about the middle of the string. Example: 
  ;(string-trim "   hi, I'm here!  ") => \"hi, I'm here!\""
  (string-trim s))

(define (remove-spaces-newlines-tabs s)
  ;  "It returns a string without all spaces, newlines and tabs for given string. Example: 
  ;(remove-spaces-newlines-tabs "   hi, I'm here!  ") => \"hi,I'mhere!\""
  (string-replace s " " ""))

(define (make-string-from-chars char-list)
  ;  "It returns a string for given char list. Example:
  ;(make-string-from-chars '(#\s #\o #\m #\e)) => \"some\""
  (list->string char-list))

(define (properties-without-spaces s)
  ;;  "It returns a string without spaces, newlines and tabs in the properties for given css. Example: 
  ;;(properties-without-spaces "margin: .67em 0;
  ;;    padding: 5px; ") => \"margin:.67em 0;padding:5px;\""
  (let ([char-list '()]
        [char-remove-p #f]) 
    (for ([c s])
      (when (or (equal? c #\:)
                (equal? c #\;))
        (set! char-list (append c char-list))
        (set! char-remove-p #t))
      (when (and char-remove-p
                 (not (or (equal? c #\:)
                          (equal? c #\;)))
                 (not (or (equal? c #\Space)
                          (equal? c #\Newline)
                          (equal? c #\Tab))))
        (set! char-remove-p #f))
      (unless char-remove-p
        (set! char-list (append c char-list))))
    (trim (make-string-from-chars char-list))))

(define (remove-css-comments-recursive css [keep-license-p #f] [start-search 0])
  ;"It removes all comments of the css code, if :keep-license-p t the comments that have \"license\" (non-case-sensitive) are kept. Use :start-search <number> to remove the comments only after a start position string."
  (let* ([css-before-start-search
          (substring css 0 start-search)]
         [css-after-start-search
          (substring css start-search)]
         [start-comment
          (string-contains css-after-start-search "/*")]
         [end-comment
          (string-contains css-after-start-search "*/")])
    (if (or (equal? start-comment #f) (equal? end-comment #f))
        css
        (let* ([css-before-start-comment
                (or (substring css-after-start-search 0 start-comment) "")]
               [css-after-end-comment
                (or (substring css-after-start-search (+ end-comment 2)) "")]
               [css-without-comment-with-before-code
                (string-append 
                 css-before-start-search
                 css-before-start-comment
                 css-after-end-comment)]
               [css-without-comment
                (string-append 
                 css-before-start-comment
                 css-after-end-comment)]
               [has-license-p
                (string-contains (string-upcase (substring css-after-start-search
                                                           start-comment
                                                           end-comment))
                                 "LICENSE")])
          (if keep-license-p
              (if has-license-p
                  (remove-css-comments-recursive css
                                                 keep-license-p
                                                 (+ 2 (length css-before-start-search) end-comment))
                  (remove-css-comments-recursive css-without-comment-with-before-code
                                                 keep-license-p))
              (remove-css-comments-recursive css-without-comment
                                             keep-license-p))))))


(define (class-name-without-spaces s)
  ; "It returns a string without spaces, newlines and tabs for a given css class name. Example: 
  ;(class-name-without-spaces \"div p,
  ;  div p + p, h1,
  ;   #myid \") => \"div p,div p+p,h1,#myid\""
  (local ([define (without-spaces s)
            (let ([char-list '()]
                  [char-remove-p #f])
              (for ([c s])
                (when (or (equal? c #\,)
                          (equal? c #\+)
                          (equal? c #\>)
                          (equal? c #\<)
                          (equal? c #\~)
                          (equal? c #\/))
                  (set! char-list (append c char-list))
                  (set! char-remove-p #t))
                (when (and char-remove-p
                           (not (or (equal? c #\,)
                                    (equal? c #\+)
                                    (equal? c #\>)
                                    (equal? c #\<)
                                    (equal? c #\~)
                                    (equal? c #\/)))
                           (not (or (equal? c #\Space)
                                    (equal? c #\Newline)
                                    (equal? c #\Tab))))
                  (set! char-remove-p #f))
                (unless char-remove-p
                  (set! char-list (append c char-list))))
              (trim (make-string-from-chars char-list)))])
    (if (or (string-contains s "+")
            (string-contains s ">")
            (string-contains s "<")
            (string-contains s "~"))
        (string-reverse (without-spaces (string-reverse (without-spaces s))))
        (without-spaces s))))

(define (minify-css-class css [keep-license-p #f])
  ;  "It returns the minified class string for a given css class string. If :keep-license-p t the comments that have \"license\" (non-case-sensitive) are kept. Example: 
  ;(minify-css-class \".myclass { margin: .67em 0; /* some comment */
  ;    padding: 5px; } \") => \".myclass{margin:.67em 0;padding:5px;}\" "
  (let* ([class-name
          (trim
           (remove-css-comments-recursive
            (class-name-without-spaces (trim (substring css
                                                        0
                                                        (string-contains css "{"))))
            keep-license-p))]
         [class-value
          (trim
           (remove-css-comments-recursive
            (substring css
                       (+ (string-contains css "{") 1)
                       (- (string-length css) 2))
            keep-license-p))])
    (string-append class-name "{" (properties-without-spaces class-value) "}")))

(define (end-bracket-index css start-bracket)
  ;"It returns the closing bracket index (here called end-bracket) of a given onpening bracket index (here called start-bracket) of one string (here called css)."
  (let ([open-bracket-level 1]
        [end-bracket 0]
        [css-before-start-bracket (substring css 0 (+ start-bracket 1))]
        [css-after-start-bracket (substring css (+ start-bracket 1))])
    (for ([c css-after-start-bracket])
      (cond ((equal? c #\{)
             (set! open-bracket-level (add1 open-bracket-level)))
            ((equal? c #\})
             (set! open-bracket-level (sub1 open-bracket-level))))
      (if (equal? open-bracket-level 0)
          (end-bracket-index css
                             (+ end-bracket
                                (string-length css-before-start-bracket)))
          (set! end-bracket (add1 end-bracket))))
    end-bracket))

(define (split-css-in-classes css [classes ""])
  ; "It returns a list with splited classes strings of a given css string. If the css has media-queries instead of a string is returned a list where the first is the media query, and the rest is the string classes. Example: 
  ;(split-css-in-classes "body{color:red;}@media (min-width:30em){body{color:green;}div{font-size:1em;}}") 
  ;=> (\"body{color:red;}\"
  ;    (\"@media (min-width:30em)\"
  ;    (\"body{color:green;}\"
  ;    \"div{font-size:1em;}\")))"
  (let ([start-bracket
         (string-contains css "{")]
        [end-bracket
         (string-contains css "}")])
    (print css)
    (if (not (and start-bracket end-bracket))
        css
        ;(split-css-in-classes (string-reverse classes))
        (let* ([css-before-start-bracket
                (substring css 0 start-bracket)]
               [media-query-p
                (string-contains css-before-start-bracket "@media")])
          (if media-query-p
              (let* ([css-between-media-query
                      (substring css
                                 (+ start-bracket 1)
                                 (end-bracket-index css start-bracket))]
                     [media-query-classes
                      (split-css-in-classes css-between-media-query)]
                     [css-after-media-query
                      (substring css (+ 1 (end-bracket-index css start-bracket)))])
                (append classes (list css-before-start-bracket
                                      media-query-classes))
                (split-css-in-classes css-after-media-query
                                      classes))
              (let ([css-between-brackets
                     (substring css (+ 1 start-bracket) end-bracket)]
                    [css-after-end-bracket
                     (substring css (+ 1 end-bracket))])
                (append classes (list (string-append
                                       css-before-start-bracket "{"
                                       css-between-brackets "}")))
                (split-css-in-classes css-after-end-bracket
                                      classes)))))))

;(define (minify-css css [keep-license-p #f])
;  "It minify a css code removing spaces, newlines, tabs, comments. If want keep the license comment use :keep-license-p t."
;  (reduce (lambda (acc curr)
;	    (format nil "~a~a" acc curr))
;	  (split-css-in-classes css)
;	  :key (lambda (class)
;		 (if (atom class)
;		     (minify-css-class class
;				       :keep-license-p keep-license-p)
;		     (format nil "~a{~a}"
;			     (properties-without-spaces (first class))
;			     (reduce (lambda (acc curr)
;				       (format nil "~a~a" acc curr))
;				     (second class)
;				     :key (lambda (class)
;					    (minify-css-class class
;							      :keep-license-p keep-license-p))))))))