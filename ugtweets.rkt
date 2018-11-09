#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)
(require racket/system)

(provide concat-tweets analyse-sentiments get-tweets-mood)

;;Sample usage
;;-------------------------
;;(get-tweets-mood "/path/to/massmine/directory/massmine --task=twitter-search --query=* --geo=1.3707,32.3032,300km --count=100")


;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array.


(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json)])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json)))))
;; Helper function to normalize case, remove URLs, remove punctuation, and remove spaces
;; from each tweet. This function takes a list of words and returns a
;; preprocessed subset of words/tokens as a list

(define (clean-text x)
   
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))

;; Retrieve tweets using the massmine cmd input

(define (fetch-tweets massmine-cmd)
  (let ([stringout (with-output-to-string (lambda ()(system massmine-cmd)))])
    (string->jsexpr (with-input-from-string stringout (λ () (json-lines->json-array))))
    
    ))


;; Each tweet includes a lot of metadata. For this analysis we’ll keep only the text
;; then discard retweets as well

(define (tweetlist massmine-cmd)
  (define tweets (fetch-tweets massmine-cmd))
  (let ([tmp (map (λ (x) (list  (hash-ref x 'text))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)
     ))

;; tweetlist returns is a list of lists of strings. Tail recursion is used to extract each string and append
;; it into one large string.

(define (concat-tweets massmine-cmd)
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (clean-text (first(first tlist1))))))]
                   )
             )
           ](joined1 (tweetlist massmine-cmd) "")) )


;; To analyse sentiments in our tweets, we extract each unique word
;; and the number of times it occurred

;; Using the nrc lexicon, we can label each (non stop-word) with an
;; emotional label.

(define (analyse-sentiments tweets-text)
  (let* ([words (document->tokens tweets-text #:sort? #t)]
        [sentiment (list->sentiment words #:lexicon 'nrc)])
    
;; sentiment, created above, consists of a list of triplets of the pattern
;; (token sentiment freq) for each token in the document. Many words will have 
;; the same sentiment label, so we aggregrate (by summing) across such tokens.
    
  (display (take sentiment 5))
  (let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
;;; We then visualize this result as a barplot (discrete-histogram)
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))
 
 ))

;; All definitions are finally consolidated into one procedural abstraction called get-tweets-mood
(define (get-tweets-mood massmine-cmd)
  (let ([tweets-string (concat-tweets massmine-cmd)])
  (analyse-sentiments tweets-string)))


