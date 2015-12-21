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

;;; This is an example of how make-pane-1 might create specialized instances of the
;;; generic pane types based upon the type of the frame-manager. Unlike in the CLX
;;; case, we *do* expect there to be W32 specific panes (eventually!).
(defmethod make-pane-1 ((fm w32-frame-manager) (frame application-frame) type &rest initargs)
  (apply #'make-instance
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
	     type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 initargs))

(defmethod adopt-frame :after ((fm w32-frame-manager) (frame application-frame))
  (let* ((sheet (frame-top-level-sheet frame))
	 (mirror (sheet-direct-mirror sheet)))
    (when (sheet-enabled-p sheet)
      (w32api:show-window mirror))))
