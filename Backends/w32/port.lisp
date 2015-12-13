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
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass w32-port (basic-port)
  ((id)
   (pointer :accessor port-pointer :initform (make-instance 'w32-pointer))
   (window :initform nil :accessor w32-port-window)))

(defun parse-w32-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :w32 :port-type) 'w32-port)
(setf (get :w32 :server-path-parser) 'parse-w32-server-path)

(defmethod initialize-instance :after ((port w32-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "W32-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'w32-frame-manager :port port)
	(slot-value port 'climi::frame-managers)))

(defmethod print-object ((object w32-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defmethod port-set-mirror-region ((port w32-port) mirror mirror-region)
  ())
                                   
(defmethod port-set-mirror-transformation
    ((port w32-port) mirror mirror-transformation)
  ())

(defmethod realize-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod destroy-mirror ((port w32-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod mirror-transformation ((port w32-port) mirror)
  ())


(defmethod port-set-sheet-region ((port w32-port) (graft graft) region)
  ())

;; these don't exist
;;;(defmethod port-set-sheet-transformation
;;;    ((port w32-port) (graft graft) transformation)
;;;  ())
;;;
;;;(defmethod port-set-sheet-transformation
;;;    ((port w32-port) (sheet mirrored-sheet-mixin) transformation)
;;;  ())

(defmethod port-set-sheet-region
    ((port w32-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region))
  nil)

(defmethod port-enable-sheet ((port w32-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port w32-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port w32-port))
  nil)

(defmethod port-motion-hints ((port w32-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod (setf port-motion-hints)
    (value (port w32-port) (sheet mirrored-sheet-mixin))
  value)

(defmethod get-next-event
    ((port w32-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  nil)

(defmethod make-graft
    ((port w32-port) &key (orientation :default) (units :device))
  (make-instance 'w32-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

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
  (declare (ignore sheet))
  nil)

(defmethod port-mirror-height ((port w32-port) sheet)
  (declare (ignore sheet))
  nil)

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
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

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
