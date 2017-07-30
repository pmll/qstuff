#lang racket

; how to redirect errors so as not to get annoying dos box when running as
; gui exe?

; fixme: entering XML into the large text boxes is awkward - need to try
; the editor canvases to see how they behave - they might be far more usable
; (and may supply load/save functionality for free!)

(require racket/gui/base)
(require ffi/com)

; GUI settings
(define app-title "COMmunicator")
(define label-width 100)
(define text-width 500)
(define small-text-width 250)
(define large-text-height 240)
(define margin-width 20)

; COM settings
(define q-clsid (string->clsid "{83E73733-7754-11D4-B840-000000000000}"))


(define (reg-server ecs) (system (string-append ecs " /regserver")))

(define (remove-crs str) (string-replace str "\r" ""))

(define (send-input ecs ems group app prog input user pwd ecs-process)
  (cond ((not (file-exists? ecs)) (values "ECS executable not found" "" #f))
        ((not (file-exists? ems)) (values "EMS file not found." "" #f))
        ((not (reg-server ecs)) (values "Failed to register ECS COM server." "" #f))
        (else
         (let ((temp-file (make-temporary-file)))
           (with-handlers ((exn:fail?
                            (lambda (e) 
                              (let ((debug (file->string temp-file)))
                                ;(delete-file temp-file)  can't do it from here, is there some equivalent to final?
                                (values "Error during COM, see debug" 
                                        (remove-crs debug) 
                                        #f)))))
             (let ((q-com (com-create-instance q-clsid 'local)))
               (com-invoke q-com "DebugBegin" (path->string temp-file))
               (com-invoke q-com "Define" "ecs-ems" ems)
               (com-invoke q-com "Define" "ecs-ems-group" group)
               (com-invoke q-com "Define" "ecs-ems-application" app)
               (com-invoke q-com "Define" "ecs-process" ecs-process)
               (com-invoke q-com "Define" "ecs-filename" prog)
               (com-invoke q-com "Define" "ecs-username" user)
               (com-invoke q-com "Define" "ecs-password" pwd)
               (com-invoke q-com "LogIn")
               (com-set-property! q-com "InputText" input)
               (com-invoke q-com "RunProcess")
               (com-invoke q-com "DebugEnd")
               (let ((output (com-get-property q-com "OutputText"))
                     (debug (file->string temp-file)))      
                 (com-release q-com)
                 (delete-file temp-file)
                 (if output
                     (values (remove-crs output)
                             (remove-crs debug)
                             #t)
                     (values "Unknown error, see debug"
                             (remove-crs debug)
                             #f)))))))))

(define (file-string->dir fs)
  (let-values (((d f mbd) (split-path (string->path fs))))
    (if (path-for-some-system? d)
        d
        (string->path "c:\\"))))

(define (make-text-box field-label parent-container orientation type init-val)
  (let ((container-pane 
         (cond ((eq? orientation 'horizontal)
                (new horizontal-pane%
                     (parent parent-container)
                     (horiz-margin margin-width)))
               ((eq? orientation 'vertical)
                (new vertical-pane%
                     (parent parent-container)
                     (horiz-margin margin-width)
                     (alignment (list 'left 'center))))
               (else parent-container))))
    (new message% 
         (label field-label)
         (parent container-pane)
         (min-width label-width))
    (new text-field%
         (label #f)
         (parent container-pane)
         (min-width (case type
                      ((small password) small-text-width)
                      (else text-width)))
         (min-height (if (eq? type 'big) large-text-height 0))
         (init-value init-val)
         (style (case type
                  ((password) (list 'single 'password))
                  ((big) (list 'multiple 'hscroll))
                  (else (list 'single)))))))
  
(define (make-file-field field-label parent-window init-val file-filter)
  (let* ((container-pane (new horizontal-pane% 
                              (parent parent-window)
                              (horiz-margin margin-width)))
         (the-text (make-text-box field-label 
                                  container-pane 
                                  'none 
                                  'standard 
                                  init-val)))
    (new button%
         (label "...")
         (parent container-pane)
         (callback (lambda (button event)
                     (let ((file (get-file (string-append "Choose "
                                                          field-label
                                                          " file")
                                           #f
                                           (file-string->dir (send the-text
                                                                   get-value))
                                           #f
                                           #f
                                           null
                                           (list file-filter))))
                       (when file
                         (send the-text set-value (path->string file)))))))
    the-text))

(define frame (new frame% (label app-title)))

(define tab-frame 
  (new tab-panel%
       (choices (list "Send" "Debug"))
       (parent frame)
       (callback (lambda (tab-panel event)
                   (let ((current-tab (send tab-panel 
                                            get-item-label
                                            (send tab-panel get-selection))))
                     (if (string=? current-tab "Send")
                         (begin
                           (send tab-panel delete-child debug-panel) 
                           (send tab-panel add-child send-panel))
                         (begin
                           (send tab-panel delete-child send-panel) 
                           (send tab-panel add-child debug-panel))))))))

(define send-panel (new vertical-panel%
                        (parent tab-frame)))

(define debug-panel (new vertical-panel%
                         (parent tab-frame)
                         (style (list 'deleted))))
  
(define text-field-ecs (make-file-field "ECS" send-panel "ecs.exe"
                                        (list "Executable" "*.exe")))

(define text-field-ems (make-file-field "EMS" send-panel "ecs.ems"
                                        (list "EMS File" "*.ems")))

(define small-fields (new horizontal-pane%
                          (parent send-panel)))

(define small-fields-left (new vertical-pane%
                               (parent small-fields)))

(define small-fields-right (new vertical-pane%
                                (parent small-fields)))

(define text-field-group (make-text-box "Group"
                                       small-fields-left
                                       'horizontal
                                       'small
                                       "PPMPM"))

(define text-field-app (make-text-box "Application"
                                      small-fields-left
                                      'horizontal
                                      'small
                                      "PPMS"))

(define text-field-user (make-text-box "Login User"
                                       small-fields-right
                                       'horizontal
                                       'small
                                       "fdd"))

(define text-field-pwd (make-text-box "Password"
                                      small-fields-right
                                      'horizontal
                                      'password
                                      ""))

(define text-field-prog (make-text-box "Program"
                                       small-fields-left
                                       'horizontal
                                       'small
                                       "CSRPPMS:"))

(define text-field-ecs-process (make-text-box "ECS Process"
                                              small-fields-right
                                              'horizontal
                                              'small
                                              "export-table"))

(define text-field-input (make-text-box "Input"
                                        send-panel
                                        'vertical
                                        'big
                                        ""))

(define text-field-output (make-text-box "Output"
                                         send-panel
                                         'vertical
                                         'big
                                         ""))

(define text-field-debug (make-text-box "Debug"
                                        debug-panel
                                        'vertical
                                        'big
                                        ""))

(define button-pane (new horizontal-pane% 
                         (parent send-panel)
                         (horiz-margin margin-width)
                         (alignment (list 'right 'center))))

(define button-send 
  (new button%
       (label "Send")
       (parent button-pane)
       (callback (lambda (button event)
                   (let-values (((output debug success)
                                 (send-input
                                  (send text-field-ecs get-value)
                                  (send text-field-ems get-value)
                                  (send text-field-group get-value)
                                  (send text-field-app get-value)
                                  (send text-field-prog get-value)
                                  (send text-field-input get-value)
                                  (send text-field-user get-value)
                                  (send text-field-pwd get-value)
                                  (send text-field-ecs-process get-value))))
                     (if success
                         (send text-field-output set-value output)
                         (begin
                           (send text-field-output set-value "**ERROR**")
                           (message-box "Error" output)))
                     (send text-field-debug set-value debug)
                     (send text-field-output focus))))))

(define button-exit (new button%
                         (label "Exit")
                         (parent button-pane)
                         (callback (lambda (button event)
                                     (send frame show #f)))))

(send frame show #t)
