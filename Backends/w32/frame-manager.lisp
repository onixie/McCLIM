;;; -*- Mode: Lisp; Package: CLIM-W32 -*-

;;; (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(defclass w32-frame-manager (frame-manager)
  ())

(defun maybe-mirroring (concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (subtypep concrete-pane-class 'basic-pane))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
					   (class-name concrete-pane-class)
					   concrete-pane-class))
	   (concrete-mirrored-pane-class (concatenate 'string
						      "W32-"
						      (symbol-name concrete-pane-class-symbol)
						      (symbol-name (gensym "-MIRRORED"))))
	   (concrete-mirrored-pane-class-symbol (find-symbol concrete-mirrored-pane-class
							     :clim-w32)))
      #+(or) (format *debug-io* "use dummy mirrored class ~A~%" concrete-mirrored-pane-class)
      (unless concrete-mirrored-pane-class-symbol
	(setf concrete-mirrored-pane-class-symbol
	      (intern concrete-mirrored-pane-class :clim-w32))
	(eval
	 `(defclass ,concrete-mirrored-pane-class-symbol
	      ,(list* 'clim-standard::standard-multi-mirrored-sheet-mixin
		      concrete-pane-class-symbol
		      (when (subtypep concrete-pane-class 'climi::top-level-sheet-pane)
			'(clim-internals::permanent-medium-sheet-output-mixin)))
	    ()
	    (:metaclass ,(type-of (find-class concrete-pane-class-symbol))))))
      #+(or) (format *debug-io* "create class ~A~%" concrete-mirrored-pane-class-symbol)
      (setf concrete-pane-class (find-class concrete-mirrored-pane-class-symbol))))
  concrete-pane-class)

;;; This is an example of how make-pane-1 might create specialized instances of the
;;; generic pane types based upon the type of the frame-manager. Unlike in the CLX
;;; case, we *do* expect there to be W32 specific panes (eventually!).
(defmethod make-pane-1 ((fm w32-frame-manager) (frame application-frame) type &rest initargs)
  (apply #'make-instance
	 (maybe-mirroring
	  (or (find-symbol (concatenate 'string
					(symbol-name '#:w32-)
					(symbol-name type))
			   :clim-w32)
	      (find-symbol (concatenate 'string
					(symbol-name '#:w32-)
					(symbol-name type)
					(symbol-name '#:-pane))
			   :clim-w32)
	      (find-symbol (concatenate 'string
					(symbol-name type)
					(symbol-name '#:-pane))
			   :climi)
	      type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 initargs))

(defmethod adopt-frame :after ((fm w32-frame-manager) (frame application-frame))
  (let* ((sheet (frame-top-level-sheet frame))
	 (mirror (sheet-direct-mirror sheet)))
    (when (sheet-enabled-p sheet)
      (w32api:show-window mirror))))

(defmethod adopt-frame :before ((fm w32-frame-manager) (frame climi::menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (multiple-value-bind (x y)
        (w32api::get-cursor-position (w32-port-window (port fm)))
      (incf x 10)
      (setf (slot-value frame 'climi::left) x
            (slot-value frame 'climi::top) y))))
