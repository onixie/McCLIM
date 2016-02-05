;;; -*- Mode: Lisp; Package: CLIM-W32; -*-

;;;  (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :clim-w32)

(defclass w32-pointer (standard-pointer)
  ((cursor :initform :upper-left
	   :accessor pointer-cursor )
   (x :initform 0
      :accessor w32-pointer-x)
   (y :initform 0
      :accessor w32-pointer-y)))

(defclass w32-port (basic-port)
  ((desktop :initform nil
	    :accessor w32-port-desktop)
   (monitor :initform nil
	    :accessor w32-port-monitor)
   (pointer :accessor w32-port-pointer
	    :initform (make-instance 'w32-pointer))
   (window  :initform nil
	    :accessor w32-port-window)
   (events  :initform nil
	    :accessor w32-port-events)))

(defun parse-w32-server-path (path)
  (pop path)
  (let ((default '(:desktop "Default" :monitor 0)))
    (list :w32
	  :desktop (getf path :desktop (getf default :desktop))
	  :monitor (getf path :monitor (getf default :monitor)))))

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :w32 :port-type) 'w32-port)
(setf (get :w32 :server-path-parser) 'parse-w32-server-path)

(defmethod initialize-instance :after ((port w32-port) &rest initargs)
  (declare (ignore initargs))
  (let ((path (cdr (port-server-path port))))
    (setf (w32-port-desktop port)
	  (or (w32api:open-desktop (getf path :desktop))
	      (w32api:create-desktop (getf path :desktop))))
    (w32api:switch-desktop (w32-port-desktop port))
    (setf (w32-port-monitor port)
	  (nth (getf path :monitor) (w32api:get-all-monitors)))
    (setf (w32-port-window port)
	  (w32api:get-desktop-window))
    (setf (w32-port-pointer port)
	  (make-instance 'w32-pointer :port port)))

  (push (make-instance 'w32-frame-manager :port port)
	(slot-value port 'climi::frame-managers))
  (when climi::*multiprocessing-p*
    (let ((stdout *standard-output*))
      (setf (climi::port-event-process port)
	    (climi::make-process
	     (lambda ()
	       (let ((*standard-output* stdout))
		 (loop
		    (with-simple-restart (restart-event-loop "Restart CLIM's event loop.")
		      (loop (process-next-event port))))))
	     :name (format nil "~S's event process." port))))))

(defmethod print-object ((port w32-port) stream)
  (print-unreadable-object (port stream :identity t :type t)
    (format stream "~S ~S ~S ~S"
	    :desktop (w32api:get-desktop-name (w32-port-desktop port))
	    :monitor (w32api:get-monitor-name (w32-port-monitor port)))))

(defmethod port-set-mirror-region ((port w32-port) mirror mirror-region)
  (w32api:resize-window mirror
			(floor (bounding-rectangle-max-x mirror-region))
			(floor (bounding-rectangle-max-y mirror-region))))

(defmethod port-set-mirror-transformation ((port w32-port) mirror mirror-transformation)
  (w32api:move-window mirror
		      (floor (nth-value 0 (transform-position mirror-transformation 0 0)))
		      (floor (nth-value 1 (transform-position mirror-transformation 0 0)))))

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2. 
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))

(defmethod sheet-mirrored-ancestor ((sheet basic-sheet))
  (unless sheet
    (sheet-mirrored-ancestor (sheet-parent sheet))))

(defun realize-mirror-aux (port sheet &rest args)
  (when (null (climi::port-lookup-mirror port sheet))
    (climi::update-mirror-geometry sheet)
    (let* ((name (first args))
	   (args (rest  args))
	   (window (apply #'w32api:create-window name
			  :x (if (climi::%sheet-mirror-transformation sheet)
				 (round-coordinate (nth-value 0 (transform-position
								 (climi::%sheet-mirror-transformation sheet)
								 0 0)))
				 (getf args :x))
			  :y (if (climi::%sheet-mirror-transformation sheet)
				 (round-coordinate (nth-value 1 (transform-position
								 (climi::%sheet-mirror-transformation sheet)
								 0 0)))
				 (getf args :y))

			  :width (if (climi::%sheet-mirror-region sheet)
				     (round-coordinate (climi::bounding-rectangle-width (climi::%sheet-mirror-region sheet)))
				     (getf args :width))
			  :height (if (climi::%sheet-mirror-region sheet)
				      (round-coordinate (climi::bounding-rectangle-height (climi::%sheet-mirror-region sheet)))
				      (getf args :height))
			  args)))
      (climi::port-register-mirror (port sheet) sheet window)
      (when (getf args :show-p)
        (w32api:show-window window))
      (w32api:message-handler+
       window :WM_PAINT
       (w32api::proc
	 (multiple-value-bind (x1 y1 x2 y2)
	     (w32api:get-update-rectangle window)
	   (push (make-instance 'climi::window-repaint-event
				:sheet sheet
				:region (make-rectangle* x1 y1 x2 y2)
				:timestamp (get-universal-time))
		 (w32-port-events port)))))
      (w32api:message-handler+
       window :WM_CHAR
       (lambda (hWnd Msg wParam lParam)
	 (declare (ignore hWnd Msg))
	 (push (print (make-instance (if (w32api:key-pressed-p lParam)
				   'pointer-button-press-event
				   'pointer-button-release-event)
			       :pointer 0
			       :button (w32api:get-key-character wParam) :x 0 :y 0
			       :graft-x 0
			       :graft-y 0
			       :sheet sheet :modifier-state (w32api:alt-key-p lParam)
			       :timestamp (get-universal-time)))
	       (w32-port-events port))))
      (w32api:message-handler+
       window :WM_DESTROY
       (w32api::proc
	 (push (make-instance 'climi::window-destroy-event
			      :sheet sheet)
	       (w32-port-events port))))))
  (climi::port-lookup-mirror port sheet))

(defmethod realize-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  (realize-mirror-aux port sheet (format nil "~a" sheet)
		      :show-p t
		      :style nil
		      :extended-style nil
		      :desktop (w32-port-desktop port)
		      :parent  (sheet-mirror (sheet-mirrored-ancestor (sheet-parent sheet)))))

(defmethod realize-mirror :after ((port w32-port) (sheet sheet-with-medium-mixin))
  (let ((window (sheet-mirror sheet)))
    (setf (w32-medium-dc (sheet-medium sheet)) (w32api:get-drawing-context window :full t))))

(defmethod realize-mirror ((port w32-port) (sheet climi::top-level-sheet-pane))
  (realize-mirror-aux port sheet (frame-pretty-name (pane-frame sheet)) :desktop (w32-port-desktop port)))

(defmethod destroy-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  (when (climi::port-lookup-mirror port sheet)
    (w32api:destroy-window (climi::port-lookup-mirror port sheet))
    (climi::port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod raise-mirror ((port w32-port) (sheet basic-sheet))
  (declare (ignore port sheet)))

(defmethod raise-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  (w32api:raise-window (sheet-mirror sheet)))

(defmethod bury-mirror ((port w32-port) (sheet basic-sheet))
  (declare (ignore port sheet)))

(defmethod bury-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  (w32api:bury-window  (sheet-mirror sheet)))

(defmethod mirror-transformation ((port w32-port) mirror)
  (multiple-value-bind (x y)
      (w32api:get-window-rectangle mirror)
    (make-translation-transformation x y)))

(defmethod port-set-sheet-region ((port w32-port) (graft graft) region)
  (declare (ignore region)))

(defmethod port-set-sheet-transformation ((port w32-port) (graft graft) transformation)
  (declare (ignore transformation)))

(defmethod port-set-sheet-transformation ((port w32-port) (sheet mirrored-sheet-mixin) transformation)
  (declare (ignore transformation))
  nil)

(defmethod port-set-sheet-region ((port w32-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region))
  nil)

(defmethod port-enable-sheet ((port w32-port) (mirror mirrored-sheet-mixin))
  (w32api:show-window mirror))

(defmethod port-disable-sheet ((port w32-port) (mirror mirrored-sheet-mixin))
  (w32api:hide-window mirror))

(defmethod destroy-port :before ((port w32-port))
  (w32api:switch-desktop (w32api:get-default-desktop))
  (w32api:destroy-desktop port))

;; (defmethod port-motion-hints ((port w32-port) (mirror mirrored-sheet-mixin))
;;   nil)

;; (defmethod (setf port-motion-hints)
;;     (value (port w32-port) (sheet mirrored-sheet-mixin))
;;   value)

(defmethod get-next-event
    ((port w32-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  (pop (w32-port-events port)))

(defmethod make-graft
    ((port w32-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'w32-graft
			      :port port :mirror (w32-port-window port)
			      :orientation orientation :units units)))
    (multiple-value-bind (x y width height)
	(w32api:get-window-rectangle (w32-port-window port))
      (setf (sheet-region graft)
	    (make-bounding-rectangle x y width height)))
    (push graft (climi::port-grafts port))
    graft))

(defmethod make-medium ((port w32-port) sheet)
  (make-instance 'w32-medium :sheet sheet))

(defmethod text-style-mapping
    ((port w32-port) text-style &optional character-set)
  (declare (ignore text-style character-set))
  nil)

(defmethod (setf text-style-mapping)
    (font-name (port w32-port)
     (text-style text-style) &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

(defmethod port-character-width ((port w32-port) text-style char)
  (declare (ignore text-style char))
  nil)

(defmethod port-string-width ((port w32-port) text-style string &key (start 0) end)
  (declare (ignore text-style string start end))
  nil)

(defmethod port-mirror-width ((port w32-port) sheet)
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (multiple-value-bind (x y width height)
	(w32api:get-window-rectangle mirror)
      (declare (ignore x y height))
      width)))

(defmethod port-mirror-height ((port w32-port) sheet)
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (multiple-value-bind (x y width height)
	(w32api:get-window-rectangle mirror)
      (declare (ignore x y width))
      height)))

(defmethod graft ((port w32-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port w32-port) sheet width height)
  (declare (ignore sheet width height))
  ;; FIXME: this isn't actually good enough; it leads to errors in
  ;; WITH-OUTPUT-TO-PIXMAP
  nil)

(defmethod port-deallocate-pixmap ((port w32-port) pixmap)
  #+nil
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer w32-pointer))
  (values (w32-pointer-x pointer) (w32-pointer-y pointer)))

(defmethod pointer-button-state ((pointer w32-pointer))
  nil)

(defmethod port-modifier-state ((port w32-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer w32-pointer))
  nil)

(defmethod port-frame-keyboard-input-focus ((port w32-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus) 
    (focus (port w32-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod (setf port-keyboard-input-focus) (focus (port w32-port))
  focus)

(defmethod port-keyboard-input-focus ((port w32-port))
  nil)

(defmethod port-force-output ((port w32-port))
  nil)

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port w32-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod port-ungrab-pointer ((port w32-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod distribute-event :around ((port w32-port) event)
  (declare (ignore event))
  (call-next-method))

(defmethod set-sheet-pointer-cursor ((port w32-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)        

(defmethod bind-selection ((port w32-port) window &optional time)
  (declare (ignore window time))
  nil)

(defmethod release-selection ((port w32-port) &optional time)
  (declare (ignore time))
  nil)

(defmethod request-selection ((port w32-port) requestor time)
  (declare (ignore requestor time))
  nil)

(defmethod get-selection-from-event ((port w32-port) event)
  (declare (ignore event))
  nil)

(defmethod send-selection ((port w32-port) event string)
  (declare (ignore event string))
  nil)
