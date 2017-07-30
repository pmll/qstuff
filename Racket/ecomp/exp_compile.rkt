#lang racket

;; experiMENTAL compiler

;; Invoke compilation

;; command line arguments:
;;   compile configuration file
;;   source file to compile
;;   output file for compiler messages (optional)

(require racket/system)
(require ffi/com)
(require "parse-nvp.rkt")

(define q-clsid (string->clsid "{83E73733-7754-11D4-B840-000000000000}"))

;; configuration file option names
(define ecs-option "ecs")
(define ems-option "ems")
(define group-option "group")
(define app-option "application")
(define json-option "json")
(define quick-option "quicklib")
(define root-option "modules-root")
(define lang-option "source-lang")
(define mandatory-options (list ecs-option ems-option group-option))
(define available-options (append mandatory-options
                                  (list app-option json-option quick-option
                                        root-option lang-option)))

(define (cli-argument n)
  (if (> (vector-length (current-command-line-arguments)) n)
      (vector-ref (current-command-line-arguments) n)
      #f))

(define (display-error . msg-parts)
  (display "Error: ")
  (for-each display msg-parts)
  (newline))

(define (list->string-list lst)
  (cond ((null? lst) "")
        ((null? (cdr lst)) (car lst))
        (else (string-append (car lst) ", " (list->string-list (cdr lst))))))

(define (valid-arguments?)
  (let ((number-of-args (vector-length (current-command-line-arguments))))
    (cond ((or (< number-of-args 2) (> number-of-args 3))
           (display "Usage: ")
           (display (find-system-path 'run-file))
           (display " <config-file> <source-file> [<messages-file>]")
           (newline)
           #f)
          ((not (file-exists? (cli-argument 0)))
           (display-error "Configuration file: "
                          (cli-argument 0)
                          " does not exist")
           #f)
          ((not (file-exists? (cli-argument 1)))
           (display-error "Source file: "
                          (cli-argument 1)
                          " does not exist")
           #f)
          (else #t))))

(define (valid-configuration? config)
  (define (config-file-exists? config-option file-label)
    (cond ((or (not (config-value config config-option))
               (file-exists? (config-value config config-option)))
           #t)
          (else (display-error file-label
                               ": "
                               (config-value config config-option)
                               " not found")
                #f)))
  (cond ((not (null? (config-duplicates config)))
         (display-error "Option duplication in configuration file: "
                        (list->string-list (config-duplicates config)))
         #f)
        ((not (null? (config-missing-names config mandatory-options)))
         (display-error "Configuration does not contain all mandatory options: "
                        (list->string-list
                          (config-missing-names config mandatory-options)))
         #f)
        ((not (null? (config-additional-names config available-options)))
         (display-error "Invalid in configuration: "
                        (list->string-list
                          (config-additional-names config available-options)))
         #f)
        ((and (config-value config json-option)
              (not (config-value config quick-option)))
         (display-error "Configuration contains JSON file without quicklib DLL")
         #f)
        ((not (config-file-exists? ecs-option "ECS executable")) #f)
        ((not (config-file-exists? ems-option "EMS file")) #f)
        ((not (config-file-exists? json-option "JSON file")) #f)
        ((not (config-file-exists? quick-option "Quicklib DLL")) #f)
        ((and (config-value config root-option)
              (not (directory-exists? (config-value config root-option))))
         (display-error "Root directory for modules: "
                        (config-value config root-option)
                        " not found")
         #f)
        ((and (config-value config lang-option)
              (not (language->compiler-com-attribute (config-value
                                                      config
                                                      lang-option))))
         (display-error "Invalid source language: "
                        (config-value config lang-option))
         #f)
        (else #t)))

(define (reg-server ecs) 
  (cond ((system (string-append ecs " /regserver")) #t)
        (else (display-error "Failed to register ecs com server")
              #f)))

(define (file-extension file-name) (last (string-split file-name ".")))

(define (language-from-filename file-name)
  (case (file-extension file-name)
    ;; actually, what is the point of getting language from an include..?
    (("qe" "qei") "ecstract3")
    (("qe2" "qe2i") "ecstract2")
    (("qp" "qpp") "pascal")
    (("qr" "qri") "rw")
    (else #f)))

(define (report-writer? source-lang)
  (let ((sl (string-downcase source-lang)))
        (or (string=? sl "rw") (string=? sl "reportwriter") (string=? sl "report-writer"))))

(define (source-language config source-file)
  (cond ((config-value config lang-option)
         (config-value config lang-option))
        ((language-from-filename source-file)
         (language-from-filename source-file))
        (else (display-error "Cannot determine source code language")
              #f)))

(define (compile source-lang ems group app quicklib json modules-root source)
  (let ((q-com (com-create-instance q-clsid 'local)))
    (com-invoke q-com "Define" "ecs-ems" ems)
    (com-invoke q-com "Define" "ecs-ems-group" group)
    (com-invoke q-com "Define" "ecs-ems-application" app)
    (com-invoke q-com
                "Define" 
                (language->compiler-com-attribute source-lang)
                (string-append "\"" source "\""))
    (when (report-writer? source-lang)
      (com-invoke q-com "Define" "ecs-debug-to-output-text" "Y")
      (com-invoke q-com "Define" "ecs-debug-hide-time" "Y"))
    (when quicklib
      (com-invoke q-com "Define" "ecs-quick-dll" quicklib))
    (when json
      (com-invoke q-com "Define" "ecs-quick-json" json))
    (when modules-root
      (com-invoke q-com "Define" "ecs-modules-root" modules-root))
    (com-invoke q-com "RunProcess")
    (let ((compile-messages (com-get-property q-com "OutputText")))
      (if compile-messages
          (begin
            (display compile-messages)
            (newline))
          (display-error "Unknown compiler failure"))
      (com-release q-com))))

(define (language->compiler-com-attribute source-lang)
  (case (string-downcase source-lang)
    (("ecstract3" "e3" "ecstract") "ecs-ecstract-text")
    (("ecstract2" "e2") "ecs-ecstract-source")
    ; don't actually know how to compile pascal, so don't use this...
    (("pascal") "compile-pascal")
    ; haven't quite figured out rw yet either...
    (("rw" "reportwriter" "report-writer") "ecs-ecstract-rw-source")
    (else #f)))

(define (display-compile-options config source-lang source)
  (display
    (format "Compile ~a source file ~a using:\nECS - ~a\nEMS - ~a\nGroup - ~a\n"
            source-lang
            source
            (config-value config ecs-option)
            (config-value config ems-option)
            (config-value config group-option)))
  (when (config-value config json-option)
    (display (format "JSON - ~a\nQuicklib - ~a\n"
                     (config-value config json-option)
                     (config-value config quick-option))))
  (when (config-value config root-option)
    (display (format "Modules root - ~a\n" (config-value config root-option)))))

(when (valid-arguments?)
  (let ((config (file->config (cli-argument 0)))
        (source (cli-argument 1))
        (output-file (cli-argument 2)))
    (when (and (valid-configuration? config)
               (reg-server (config-value config ecs-option)))
      (let ((source-lang (source-language config source)))
        (when source-lang
          (define (call-compile)
            (compile source-lang
                     (config-value config ems-option)
                     (config-value config group-option)
                     (if (config-value config app-option)
                         (config-value config app-option)
                         "PPMS")
                     (config-value config quick-option)
                     (config-value config json-option)
                     (config-value config root-option)
                     source))
          (display-compile-options config source-lang source)
          (if output-file
            (with-output-to-file output-file
                                 call-compile
                                 #:exists 'replace)
            (call-compile)))))))

