#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)
(require racket/system)


;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array. For very large data sets, loading
;;; everything into memory like this is heavy handed. For data this small,
;;; working in memory is simpler

(provide joined-tweets sentiment-analysis analyse-tweets)


;; Read the tweets from the twurl request provided, the tweets are under the key 'results'

(define (read-tweets-twurl twurl-request)
  (let ([stringout (with-output-to-string (lambda ()(system twurl-request)))])
    ;(hash-ref (with-input-from-string stringout (λ () (read-json))) 'results)
    stringout
    ))


;; Extract the actual tweet text from each tweet
;;; hash. Finally, remove retweets.

(define (tweetlist twurl-request)
  (define tweets (read-tweets-twurl twurl-request))
  (let ([tmp (map (λ (x) (list  (hash-ref x 'text))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)
     ))

;;; Helper function to normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. This function takes a list of words and returns a
;;; preprocessed subset of words/tokens as a list

(define (clean-text x)
   
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))

;;tweetlist returns is a list of lists of strings. Tail recursion is used to extract each string and append
;; it into one large string.
(define (joined-tweets twurl-request)
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (clean-text (first(first tlist1))))))]
                   )
             )
           ](joined1 (tweetlist twurl-request) "")) )


;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label.

(define (sentiment-analysis tweet-text)
  (let* ([words (document->tokens tweet-text #:sort? #t)]
        [sentiment (list->sentiment words #:lexicon 'nrc)])
;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.     
  (display (take sentiment 5))
  (let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))
 
 ))

;;analyse-tweets is the overall abstraction that takes a valid twurl request as an
;;argument to draw a histogram of sentiments
(define (analyse-tweets twurl-request)
  (let ([ff (joined-tweets twurl-request)])
    ;(display ff)
  (sentiment-analysis ff))
  )


;;Example calls
;;(analyse-tweets "/usr/local/bin/twurl /1.1/tweets/search/30day/dev.json?query=DailyMonitor search api&place_country=UK&fromDate=201710090000&toDate=201711090000")
;;(analyse-tweets "/usr/local/bin/twurl /1.1/tweets/search/30day/dev.json?query=DailyMonitor search api&place_country=UK&fromDate=201710090000&toDate=201711090000")
; (analyse-tweets "/usr/local/bin/twurl /1.1/tweets/search/30day/dev.json?query=15939084 search api&place_country=UK&fromDate=201710090000&toDate=201711090000")

;"/usr/local/bin/twurl /1.1/tweets/search/30day/dev.json?query=realDonaldTrump search api&place_country=UK&fromDate=201710090000&toDate=201711090000"
