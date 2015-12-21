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
	    :accessor w32-port-window)))

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
	(slot-value port 'climi::frame-managers)))

(defmethod print-object ((port w32-port) stream)
  (print-unreadable-object (port stream :identity t :type t)
    (format stream "~S ~S ~S ~S"
	    :desktop (w32api:get-desktop-name (w32-port-desktop port))
	    :monitor (w32api:get-monitor-name (w32-port-monitor port)))))

(defmethod port-set-mirror-region ((port w32-port) mirror mirror-region)
  (multiple-value-bind (x y width height)
      (w32api:get-window-rectangle mirror)
    (declare (ignore width height))
    (w32api:move-window mirror
			x
			y
			(floor (bounding-rectangle-max-x mirror-region))
			(floor (bounding-rectangle-max-y mirror-region)))))
                                   
(defmethod port-set-mirror-transformation ((port w32-port) mirror mirror-transformation)
  (multiple-value-bind (x y width height)
      (w32api:get-window-rectangle mirror)
    (declare (ignore x y))
    (w32api:move-window mirror
			(floor (nth-value 0 (transform-position mirror-transformation 0 0)))
			(floor (nth-value 1 (transform-position mirror-transformation 0 0)))
			width
			height)))

(defmethod realize-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod realize-mirror ((port w32-port) (sheet climi::top-level-sheet-pane))
  (let ((window (w32api:create-window (frame-pretty-name (pane-frame sheet)) :desktop (w32-port-desktop port))))
    (climi::port-register-mirror (port sheet) sheet window)
    (climi::port-lookup-mirror port sheet)))

(defmethod destroy-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  (when (climi::port-lookup-mirror port sheet)
    (w32api:destroy-window (climi::port-lookup-mirror port sheet))
    (climi::port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod raise-mirror ((port w32-port) (sheet basic-sheet))
  (let ((mirror (sheet-mirror sheet)))
    (when (w32api:window-p mirror)
      ;(w32api:raise-window mirror)
      )))

(defmethod bury-mirror ((port w32-port) (sheet basic-sheet))
  (let ((mirror (sheet-mirror sheet)))
    (when (w32api:window-p mirror)
      ;(w32api:bury-window mirror)
      )))

(defmethod mirror-transformation ((port w32-port) mirror)
  (multiple-value-bind (x y)
      (w32api:get-window-rectangle mirror)
    (make-translation-transformation x y)))

(defmethod port-set-sheet-region ((port w32-port) (graft graft) region)
  (declare (ignore region))
  nil)

(defmethod port-set-sheet-transformation ((port w32-port) (graft graft) transformation)
  (declare (ignore transformation))
  nil)

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
  nil)

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
  nil)

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
