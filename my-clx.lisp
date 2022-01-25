;;; -*-Mode: lisp; -*-
;;;
;;;
;;; A Sample system with POINT LINE RECTANGLE
;;;   using CLOS and clx
;;;

(ql:quickload '(:clx :clx/demo :clue) :silent t) ; => (:CLX :CLX/DEMO :CLUE)
(defpackage :my-clx
  (:use :cl :xlib))			; => #<PACKAGE "MY-CLX">
(in-package :my-clx)			; => #<PACKAGE "MY-CLX">
(xlib-demo/demos:demo)			; => NIL

;; X システムとインターフェイスで接続できるようにするには、最初にサーバーに接続しなくてはなりません。 
;; 下のいずれかで良さそう。
(open-default-display)	     ; => #<DISPLAY :0 (The X.Org Foundation R12011000)>
(open-display "" :display 0) ; => #<DISPLAY :0 (The X.Org Foundation R12011000)>

(defvar *display* nil)			; => *DISPLAY*
(defvar *screen* nil)			; => *SCREEN*
(defvar *root-window* nil)		; => *ROOT-WINDOW*
(defvar *window* nil)			; => *WINDOW*
(defvar *gcontext*)			; => *GCONTEXT*
(defvar *white-pixel* nil)		; => *WHITE-PIXEL*
(defvar *black-pixel* nil)		; => *BLACK-PIXEL*

(defun try (&optional (host ""))
  (setq *display* (open-display host))
  (setf (display-after-function *display*) #'display-finish-output)
  (setq *screen* (car (display-roots *display*)))
  (setq *root-window* (screen-root *screen*))
  (setq *black-pixel* (screen-black-pixel *screen*))
  (setq *white-pixel* (screen-white-pixel *screen*))
  (setq *window*
        (create-window :parent *root-window*
                            ;; these are ignored.
                            :x 400
                            :y 3
                            :width 100
                            :height 30
                            ;; this is ignored if bs not supported.
                            :backing-store :always
                            :border *white-pixel*
                            :background *black-pixel*
                            :event-mask (make-event-mask :button-press)))
  ;; Set up main window's properties.
  (setf (wm-name *window*) "Demo -- Draw1") ;tilte bar
  (setf (wm-icon-name *window*) "Demo -- Draw2")
  (setf (wm-normal-hints *window*)
        (make-wm-size-hints :x 400 :y 3 :width 100 :height 30))
  (map-window *window*)
  (setq *gcontext* (create-gcontext :foreground *white-pixel*
                                         :background *black-pixel*
                                         :drawable *window*))
  (display-finish-output *display*))	; => TRY
(try)					; => NIL


;; class POINT

(defclass point ()
  ((x :initform 0 :initarg x :writer move-x)
   (y :initform 0 :initarg y :writer move-y))) ; => #<STANDARD-CLASS MY-CLX::POINT>

;; class LINE

(defclass line (point)
  ((length :initform 1 :initarg length :writer change-length)
   (direction :initform 0 :initarg direction :writer change-direction))) ; => #<STANDARD-CLASS MY-CLX::LINE>
;;(describe 'line)                                                         ; => MY-CLX::LINE

;; class RECTANGLE
(defclass rectangle (line)
  ((height :initform 1 :initarg height :writer change-height))) ; => #<STANDARD-CLASS MY-CLX::RECTANGLE>

;; class CIRCLE

(defclass circle (point)
  ((radius :initform 1 :initarg radius :writer change-radius))) ; => #<STANDARD-CLASS MY-CLX::CIRCLE>
;; (describe 'circle)                                              ; => MY-CLX::CIRCLE
;;   [symbol]

;; CIRCLE names the standard-class #<STANDARD-CLASS MY-CLX::CIRCLE>:
;;   Direct superclasses: POINT
;;   No subclasses.
;;   Not yet finalized.
;;   Direct slots:
;;     RADIUS
;;       Initargs: RADIUS
;;       Initform: 1
;;       Writers: CHANGE-RADIUS


;;;
;;; Generic Function DRAW
;;;

(defmethod draw ((p point))
  (with-slots (x y) p
    (draw-point *window* *gcontext* x y))) ; => #<STANDARD-METHOD MY-CLX::DRAW (POINT) {1007CEE5E3}>

(defmethod draw ((l line))
  (with-slots (x y length direction) l
    (multiple-value-bind (new-x new-y)
        (get-another-end x y length direction)
      (draw-line-internal x y new-x new-y)))) ; => #<STANDARD-METHOD MY-CLX::DRAW (LINE) {1007FEDFB3}>

(defun draw-line-internal (x y xx yy)
  (draw-line *window* *gcontext* x y xx yy)) ; => DRAW-LINE-INTERNAL

(defmethod draw ((r rectangle))
  (with-slots (x y length direction height) r
    (multiple-value-bind (x1 y1)
        (get-another-end x y length direction)
      (multiple-value-bind (x2 y2)
          (get-another-end x1 y1 height (+ direction (/ pi 2)))
        (multiple-value-bind (x3 y3)
            (get-another-end x y height (+ direction (/ pi 2)))
          (draw-lines *window* *gcontext*
                      (list x y x1 y1 x2 y2 x3 y3 x y))))))) ; => #<STANDARD-METHOD MY-CLX::DRAW (RECTANGLE) {1003BF0303}>

(defmethod draw ((c circle))
  (with-slots (x y)))
(defun get-another-end (x y length direction)
  (values (round (+ x (* length (cos direction))))
          (round (+ y (* length (sin direction)))))) ; => GET-ANOTHER-END


;;; draw sample
(setq p1 (make-instance 'point 'x 290 'y 290))		; => #<POINT {1003CC9C93}>
(draw p1)						; => NIL
(setq l1 (make-instance 'line 'x 0 'y 200 'length 300)) ; => #<LINE {1003AB2A63}>
(draw l1)                                               ; => NIL
(setq r1 (make-instance 'rectangle 'x 100 'y 50 'length 50 'direction 0 'height 40)) ; => #<RECTANGLE {1003EA4333}>
(draw r1)                                                             ; => 36
(setq l2 (make-instance 'line 'x 100 'y 0 'length 200 'direction 20)) ; => #<LINE {10040AB8F3}>
(draw l2)                                                             ; => NIL

(defun pop-up-window (life-time &optional (host ""))
  (let* ((display (xlib:open-display host))
         (screen (first (xlib:display-roots display)))
         (root-window (xlib:screen-root screen))
         (my-window (xlib:create-window
                      :parent root-window
                      :x 0
                      :y 0 
                      :width 200
                      :height 300)))
    (xlib:map-window my-window)
    (xlib:display-finish-output display)
    (format t "it should be here ~%")
    (sleep life-time)
    (xlib:destroy-window my-window)
    (xlib:close-display display)
))					; => POP-UP-WINDOW

(pop-up-window 10)			; => it should be here 
NIL
;; (compute-restarts)                      ; => (#<RESTART SWANK::RETRY {7FCB66C6E313}> #<RESTART ABORT {7FCB66C6E5D3}>
;;  ;; #<RESTART ABORT {7FCB66C6EB53}>)
;; (find-restart 'abort)                   ; => #<RESTART ABORT {7FCB66C6E5D3}>
;; (find-restart 'retry)                   ; => NIL
;; (find-restart 'continue)                ; => NIL
;; (invoke-restart-interactively 'abort)   ; =>


;; (char-code #\あ)                       ; => 12354

