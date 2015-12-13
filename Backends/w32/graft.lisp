;;; -*- Mode: Lisp; Package: CLIM-W32 -*-

;;;  (c) copyright 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(defclass w32-graft (graft)
  ())

(defmethod graft-width ((graft w32-graft) &key (units :device))
  (declare (ignore units))
  nil)

(defmethod graft-height ((graft w32-graft) &key (units :device))
  (declare (ignore units))
  nil)
