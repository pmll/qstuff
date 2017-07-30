#lang racket

(define (make-task time heading? description) (list time heading? description))
(define (task-time task) (car task))
(define (task-heading? task) (cadr task))
(define (task-description task) (caddr task))

(define (line->task line)
  (define line-length (string-length line))
  (define (line->time line)
    (cond ((> line-length 4) (substring line 0 5))
          (else "00:00")))
  (define (line->heading? line)
    (cond ((> line-length 8) (string=? (substring line 6 9) "<~>"))
          (else #f)))
  (define (line->description line)
    (cond ((line->heading? line)
           (cond ((> line-length 9) (substring line 9))
                 (else "")))
          (else (cond ((> line-length 6) (substring line 6))
                      (else "")))))
  (make-task (line->time line)
             (line->heading? line)
             (line->description line)))

(define (lines->tasks lines) (map line->task lines))

(define (tasks->structured-tasks tasks)
  (define (tasks->sub-tasks tasks)
    (cond ((null? tasks) '())
          ((task-heading? (car tasks)) '())
          ((string=? (task-time (car tasks)) "00:00")
           (tasks->sub-tasks (cdr tasks)))
          (else (cons (car tasks) (tasks->sub-tasks (cdr tasks))))))
  (cond ((null? tasks) '())
        ((and (task-heading? (car tasks)) 
              (not (null? (tasks->sub-tasks (cdr tasks)))))
         (cons (cons (car tasks) (tasks->sub-tasks (cdr tasks)))
               (tasks->structured-tasks (cdr tasks))))
        (else (tasks->structured-tasks (cdr tasks)))))

(define (display-structured-tasks structured-tasks)
  (define (display-head-task head-task)
    (display (task-description head-task))
    (newline))
  (define (display-sub-task sub-task)
    (display "    ")
    (display (task-time sub-task))
    (display " ")
    (display (task-description sub-task))
    (newline))
  (define (display-structured-task structured-task)
    (display-head-task (car structured-task))
    (for-each display-sub-task (cdr structured-task)))
  (for-each display-structured-task structured-tasks))

(define (valid-arguments?)
  (cond ((not (= (vector-length (current-command-line-arguments)) 1))
         (display "Usage: ")
         (display (find-system-path 'run-file))
         (display " <task tracker file>")
         #f)
        ((not (file-exists? (vector-ref (current-command-line-arguments) 0)))
         (display "File: ")
         (display (vector-ref (current-command-line-arguments) 0))
         (display " not found")
         #f)
        (else #t)))

(when (valid-arguments?)
  (display-structured-tasks
    (tasks->structured-tasks
      (lines->tasks
        (file->lines (vector-ref (current-command-line-arguments) 0))))))
