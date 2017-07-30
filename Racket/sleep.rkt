#lang racket

(define (display-usage)
  (display "Usage: ")
  (display (find-system-path 'run-file))
  (display " <number of seconds>")
  (newline))
  
(if (= (vector-length (current-command-line-arguments)) 1)
    (let ((sleep-time 
           (string->number (vector-ref (current-command-line-arguments) 0))))
      (if sleep-time
          (sleep sleep-time)
          (display-usage)))
    (display-usage))
