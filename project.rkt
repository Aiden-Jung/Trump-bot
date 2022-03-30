#lang racket

(require csc151)
; https://github.com/grinnell-cs/csc151.git#9689bf8

(require json)
(require net/url)

; data from:
; https://github.com/bpb27/trump_tweet_data_archive\

;;; Procedure:
;;;   split
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Divide a string by word and create a list of strings each of which contains an individual word.
;;; Produces:
;;;;  result, a list of strings
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   There are still some unclear words in result because some of the words have punctuations inside.

(define split
  (let ([find-cutpoint (lambda (str-lst)
                         (let* ([delimiters (list #\space #\! #\? #\, #\newline #\( #\) #\ #\- #\: #\\ #\" #\/ #\; #\[ #\])]
                                [indicies (map1 (section index-of <> str-lst) delimiters)]
                                [reduced (filter (lambda (x) (not (= x -1))) indicies)])
                           (if (null? reduced)
                               -1
                               (reduce min reduced))))]
        [word-clean (let* ([last-char-dot? (lambda (str-lst)
                                             (let ([len (length str-lst)])
                                               (and (not (= 0 len))
                                                    (equal? #\. (list-ref str-lst (sub1 len))))))]
                           [first-char-dot? (lambda (x) (equal? #\. (list-ref x 0)))]
                           [count-dots (lambda (str-lst)
                                         (let ([len (length str-lst)])
                                           (let kernel ([i 0]
                                                        [count 0])
                                             (cond [(<= len i) count]
                                                   [(equal? #\. (list-ref str-lst i))
                                                    (kernel (add1 i) (add1 count))]
                                                   [else
                                                    (kernel (add1 i) count)]))))]
                           [rear-ellipsis? (let ([just-dots? (lambda (str-lst)
                                                               (= (count-dots str-lst) (length str-lst)))])
                                             (lambda (str-lst)
                                               (let ([len (length str-lst)])
                                                 (and (< 1 (count-dots str-lst))
                                                      (just-dots? (drop str-lst (- len 2)))))))]
                           [front-ellipsis? (lambda (str-lst)
                                              (and (< 1 (length str-lst))
                                                   (equal? (car str-lst) #\.)
                                                   (equal? (car str-lst) (cadr str-lst))))]
                           [remove-rear-ellipsis (letrec ([ellipsis-index (lambda (str-lst)
                                                                            (let ([i (index-of #\. str-lst)])
                                                                              (if (equal? (list-ref str-lst i) (list-ref str-lst (add1 i)))
                                                                                  i
                                                                                  (+ 2 i (ellipsis-index (drop str-lst (add1 i)))))))])
                                                   (lambda (str-lst)
                                                     (if (rear-ellipsis? str-lst)
                                                         (take str-lst (ellipsis-index str-lst))
                                                         str-lst)))]
                           [remove-front-ellipsis (lambda (str-lst)
                                                    (let ([rev (reverse str-lst)])
                                                      (reverse (take rev (index-of #\. rev)))))])
                      (lambda (str-lst)
                        (list->string (cond [(and (= 1 (count-dots str-lst))
                                                  (first-char-dot? str-lst))
                                             (drop str-lst 1)]
                                            [(and (= 1 (count-dots str-lst))
                                                  (last-char-dot? str-lst))
                                             (take str-lst (sub1 (length str-lst)))]
                                            [(front-ellipsis? str-lst)
                                             (remove-front-ellipsis str-lst)]
                                            [(rear-ellipsis? str-lst)
                                             (remove-rear-ellipsis str-lst)]
                                            [else str-lst]))))])
    (lambda (str)
      (let ([lst (string->list str)])
        (let kernel ([so-far null]
                     [rest lst])
          (let ([cutpoint (find-cutpoint rest)])
            (if (= -1 cutpoint)
                (reverse (let ([word (word-clean rest)])
                           (if (equal? "" word)
                               so-far
                               (cons word so-far))))
                (let ([word (word-clean (take rest cutpoint))])
                  (if (equal? "" word)
                      (kernel so-far (drop rest (add1 cutpoint)))
                      (kernel (cons word so-far)
                              (drop rest (add1 cutpoint))))))))))))

;;; Procedure:
;;;   avg-word-ct
;;; Parameters:
;;;   lst, a list of lists whose first elements are strings 
;;; Purpose:
;;;   Calculate the average number of words in all of the first elements of the lists in lst
;;; Produces:
;;;;  result, a real non-negative number
;;; Preconditions:
;;;   Each list in lst should be formatted as ‘("text" "date")
;;; Postconditions:
;;;   result could be different from the actual average because split cannot wipe out all the unncessary texts.

(define avg-word-ct
  (let ([arith-mean
         (lambda (lst)
           (/ (reduce + lst) (length lst)))])
    (lambda (lst)
      (inexact->exact (round (arith-mean (map1 (o length split car) lst)))))))

;;; Procedure:
;;;   get-tweets
;;; Parameters:
;;;   path, a string that names a file
;;; Purpose:
;;;   Import a file of Donald Trump tweets and extract only the texts and date of the tweets
;;; Produces:
;;;;  result, list
;;; Preconditions:
;;;   [path has to be valid]
;;; Postconditions:
;;;   if the path is valid, the result should be formatted as ("text" "date")

(define get-tweets
  (let ([extract-tweet
         (lambda (hash)
           (let ([txt (hash-ref hash 'full_text #f)]
                 [date (hash-ref hash 'created_at #f)])
             (if (and txt date)
                 (list txt date)
                 null)))])
    (lambda (path)
      (let* ([file-port (call-with-input-file path read-json)])
        (let kernel ([so-far null]
                     [lst file-port])
          (if (null? lst)
              so-far
              (let ([twt (extract-tweet (car lst))])
                (if (null? twt)
                    (kernel so-far (cdr lst))
                    (kernel (append so-far (list twt)) (cdr lst))))))))))

;;; Procedure:
;;;   random-list
;;; Parameters:
;;;   tallies, a list 
;;; Purpose:
;;;   generate random list of words in string 
;;; Produces:
;;;   result, a list 
;;;Description: random-list randomly genrates a list of words from lists of words used in Trump tweets.
;;;             the lengh of the result will be based on the average length of trump tweete within the range of plus or minus five
;;;             words


(define random-list
  (let* ([tweets (get-tweets "master_2017.json")]
         [fetch (lambda (tallies index)
                  (let kernel ([lst tallies]
                               [i index])
                    (if (null? (cdr lst))
                        (car lst)
                        (let ([first (car lst)]
                              [rest (cdr lst)])
                          (if (<= i (cadr first))
                              (car first)
                              (kernel rest (- i (cadr first))))))))]
         [random-word (lambda (tallies)
                        (let* ([total-count (reduce + (map1 cadr tallies))]
                               [index (random total-count)])
                          (fetch tallies index)))]
         [arith-mean (lambda (lst)
                       (/ (reduce + lst) (length lst)))]
         [avg-word-ct (lambda (lst)
                        (inexact->exact (round (arith-mean (map1 (o length split car) lst)))))])
    (lambda (tallies)
      (let ([sent-len (+ (random 10) (- (avg-word-ct tweets) 5))])
        (let kernel ([count sent-len]
                     [so-far null])
          (if (= 0 count)
              so-far
              (kernel (sub1 count)
                      (cons (random-word tallies) so-far))))))))

(define tweets (get-tweets "master_2017.json"))
;;;Description: tweets import hash tables from the downloaded file
;;;and format the content in the form of '("text""date")

(define tallies
  (let ([clean-tallies (let* ([has-alphabet? (lambda (str)
                                               (let ([lst (string->list str)])
                                                 (not (null? (filter (section or <> #f) (map1 char-alphabetic? lst))))))]
                              [has-digit? (lambda (str)
                                            (let ([lst (string->list str)])
                                              (not (null? (filter (section or <> #f) (map1 char-numeric? lst))))))]
                              [good-word? (lambda (str)
                                            (and (= -1 (index-of str (list "t.co" "&amp" "https" "RT")))
                                                 (not (and (has-digit? str) (has-alphabet? str)))))])
                         (lambda (tallies)
                           (filter (lambda (x) (good-word? (car x))) tallies)))])
    (clean-tallies (sort (tally-all (reduce append (map1 split (map1 car tweets))))
                         (lambda (x y) (>= (cadr x) (cadr y)))))))

;;; Procedure:
;;;   categorize
;;; Parameters:
;;;   word, a string 
;;; Purpose:
;;;   categorize a word based on its part of speech  
;;; Produces:
;;;  result, a list of string 
;;;Description: The procedure cataegorizes a word and return its part of speech. Catergorization is based on the part of speech
;;;             provide by "https://simple.wiktionary.org/wiki/"

(define categorize
  (lambda (word)
    (let ([fix (lambda (lst)
                 (let ([lst-clear (filter (negate (disjoin (section equal? <> #\") (section equal? <> #\\))) lst)])
                   (let kernel ([so-far null]
                                [remaining lst-clear]
                                [i (index-of #\, lst-clear)])
                     (if (= i -1)
                         (reverse (cons (list->string remaining) so-far))
                         (kernel (cons (list->string (take remaining i)) so-far)
                                 (drop remaining (add1 i))
                                 (index-of #\, (drop remaining (add1 i))))))))]
          [substring-by-index (lambda (lst chr1 chr2)
                                (drop (take lst (index-of chr2 lst)) (add1 (index-of chr1 lst))))])
      (let* ([str (bytes->string/utf-8
                   (port->bytes
                    (get-pure-port (string->url (string-append "https://simple.wiktionary.org/wiki/" word)))))]
             [lst (string->list str)]
             [certain-word (string->list "wgCategories")])
        (let kernel ([so-far (list word)]
                     [remaining lst])
          (let* ([i (index-of #\w remaining)]
                 [lst-front-clear (if (= i -1)
                                      (list)
                                      (drop remaining i))])
            (when (not (= i -1))
              (if (equal? certain-word (take (drop remaining i) 12))
                  (cons word (fix (substring-by-index lst-front-clear #\[ #\])))
                  (kernel so-far (drop remaining (add1 i)))))))))))
(define is-noun?
  (lambda (lst)
    (or
     (and (not (void? lst))
          (equal? "" (cadr lst)))
     (and (not (void? lst))
          (not (= -1 (index-of "Nouns" lst)))))))

(define is-verb?
  (lambda (lst)
    (and (not (void? lst))
         (not (= -1 (index-of "Verbs" lst))))))

(define is-adjective?
  (lambda (lst)
    (and (not (void? lst))
         (not (= -1 (index-of "Adjectives" lst))))))

(define is-adverb?
  (lambda (lst)
    (and (not (void? lst))
         (not (= -1 (index-of "Adverbs" lst))))))

(define is-preposition?
  (lambda (lst)
    (and (not (void? lst))
         (not (= -1 (index-of "Prepositions" lst))))))

(define is-conjunction?
  (lambda (lst)
    (and (not (void? lst))
         (not (= -1 (index-of "Conjunctions" lst))))))

;;; Procedure:
;;;   create-sentence
;;; Parameters:
;;;   tally, a list of lists 
;;; Purpose:
;;;   create a random sentence based on the part of speech of each word
;;; Produces:
;;;  result, a list of strings 
;;;Description: judge the part of speech and pick them randomly in the specific order to generate a list of a string.

(define create-sentence
  (lambda (tally)
    (let* ([random-elt
            (lambda (lst)
              (if (null? lst)
                  ""
                  (list-ref lst (random (length lst)))))]
           [noun (random-elt (map car (filter is-noun? (map categorize (random-list tally)))))]
           [noun2 (random-elt (map car (filter is-noun? (map categorize (random-list tally)))))]
           [noun3 (random-elt (map car (filter is-noun? (map categorize (random-list tally)))))]
           [noun4 (random-elt (map car (filter is-noun? (map categorize (random-list tally)))))]
           [verb (random-elt (map car (filter is-verb? (map categorize (random-list tally)))))]
           [verb2 (random-elt (map car (filter is-verb? (map categorize (random-list tally)))))]
           [adjective (random-elt (map car (filter is-adjective? (map categorize (random-list tally)))))]
           [adjective2 (random-elt (map car (filter is-adjective? (map categorize (random-list tally)))))]
           [conjunction (if (equal? "" (random-elt (map car (filter is-conjunction? (map categorize (random-list tally))))))
                            (random-elt (list "but" "and" "or" "yet" "so" "until" "for" "although" "even if" "even though" "while" "as soon as" "after"
                                              "before" "in case that" "unless" "if" "only if" "since" "so that" "as" "as if"))
                            (random-elt (map car (filter is-conjunction? (map categorize (random-list tally))))))]
           [punctuation (random-elt (list #\? #\! #\. #\space))]
           [template (reverse (list noun verb adjective noun2 conjunction noun3 verb2 adjective2 noun4))]
           [lst (string->list (foldl (lambda
                                         (x y)
                                       (string-append x " " y))
                                     ""
                                     template))])
      (list->string (append (cons (char-upcase (car lst)) (cdr (map char-downcase lst))) (list punctuation))))))


