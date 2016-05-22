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

(defclass w32-medium (basic-medium)
  ((dc :initform nil
       :accessor w32-medium-dc)
   (font :initform nil
	 :accessor w32-medium-font)
   (pen :initform nil
	:accessor w32-medium-pen))
					;((buffering-output-p :accessor medium-buffering-output-p))
  )

(defmethod medium-rgb ((medium w32-medium) (color color))
  (color-rgb color))

(defmethod medium-rgb ((medium w32-medium) (color (eql +foreground-ink+)))
  (color-rgb (medium-foreground medium)))

(defmethod medium-rgb ((medium w32-medium) (color (eql +background-ink+)))
  (color-rgb (medium-background medium)))

(defmethod medium-rgb ((medium w32-medium) (color (eql +flipping-ink+)))
  (color-rgb (medium-background medium)))

(defmethod (setf medium-text-style) :after (text-style (medium w32-medium))
  (when (w32-medium-dc medium)
    (setf (w32-medium-font medium)
	  (text-style-mapping (port (medium-sheet medium)) text-style))
    (w32api.gdi32::SelectObject (w32-medium-dc medium) (w32-medium-font medium))))

(defmethod (setf medium-line-style) :after (line-style (medium w32-medium))
  (flet ((line-style-mapping (line-style)
	   (multiple-value-bind (r g b)
	       (medium-rgb medium (medium-ink medium))
	     (w32api::create-pen :style (cond ((eq (line-style-dashes line-style) nil) :PS_SOLID)
					      ((eq (line-style-dashes line-style) t) :PS_DASH)
					      (t (mapcar #'round-coordinate (line-style-dashes line-style))))
				 :endcap (case (line-style-cap-shape line-style)
					   (:butt :PS_ENDCAP_FLAT)
					   (:square :PS_ENDCAP_SQUARE)
					   (:round :PS_ENDCAP_ROUND)
					   (t :PS_ENDCAP_FLAT))
				 :join (case (line-style-joint-shape line-style)
					 (:miter :PS_JOIN_MITER)
					 (:bevel :PS_JOIN_BEVEL)
					 (:round :PS_JOIN_ROUND)
					 (t :PS_JOIN_MITER))
				 :width (round-coordinate (line-style-thickness line-style))
				 :color (list (round-coordinate (* 255 r))
					      (round-coordinate (* 255 g))
					      (round-coordinate (* 255 b)))))))
    (when (w32-medium-dc medium)
      (when (w32-medium-pen medium)
	(w32api.gdi32::DeleteObject (w32-medium-pen medium)))
      (setf (w32-medium-pen medium)
	    (line-style-mapping line-style))
      (w32api.gdi32::SelectObject (w32-medium-dc medium) (w32-medium-pen medium)))))

(defmethod (setf medium-ink) :after (design (medium w32-medium))
  (setf (medium-line-style medium) (medium-line-style medium)))

(defmethod (setf medium-clipping-region) :after (region (medium w32-medium))
  (declare (ignore region))
  nil)

(defmethod medium-copy-area ((from-drawable w32-medium)
			     from-x from-y width height
                             (to-drawable w32-medium)
			     to-x to-y)
  (declare (ignore from-x from-y width height to-x to-y))
  nil)

#+nil ; FIXME: PIXMAP class
(progn
  (defmethod medium-copy-area ((from-drawable w32-medium)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil)

  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable w32-medium)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil)

  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil))

(defmethod medium-draw-point* ((medium w32-medium) x y)
  (multiple-value-bind (r g b)
      (medium-rgb medium (medium-ink medium))
    (w32api.gdi32::SetPixel (w32-medium-dc medium) (round-coordinate x) (round-coordinate y)
			    (w32api::make-rgb-color (round-coordinate (* 255 r))
						    (round-coordinate (* 255 g))
						    (round-coordinate (* 255 b))))))

(defmethod medium-draw-points* ((medium w32-medium) coord-seq)
  (climi::do-sequence ((x y) coord-seq)
    (medium-draw-point* medium x y)))

(defmethod medium-draw-line* ((medium w32-medium) x1 y1 x2 y2)
  (let ((dc (w32-medium-dc medium)))
    (w32api.gdi32::MoveToEx dc (round-coordinate x1) (round-coordinate y1) (cffi:null-pointer))
    (w32api.gdi32::LineTo dc (round-coordinate x2) (round-coordinate y2))))

;; FIXME: Invert the transformation and apply it here, as the :around
;; methods on transform-coordinates-mixin will cause it to be applied
;; twice, and we need to undo one of those. The
;; transform-coordinates-mixin stuff needs to be eliminated.
(defmethod medium-draw-lines* ((medium w32-medium) coord-seq)
  (climi::do-sequence ((x1 y1 x2 y2) coord-seq)
    (medium-draw-line* medium x1 y1 x2 y2)))

(defmethod medium-draw-polygon* ((medium w32-medium) coord-seq closed filled)
  (let ((pair-seq (loop for (x y)
		     on (map 'list #'identity coord-seq);hack: when do record output, it seems coor-seq might be vector
		     by #'cddr
		     collect (list (round-coordinate x)
				   (round-coordinate y)))))
    (w32api.type::with-points ((points count) pair-seq)
      (if (or closed filled)
	  (multiple-value-bind (r g b)
	      (medium-rgb medium (medium-ink medium))
	    (w32api::with-drawing-object ((w32-medium-dc medium)
					  (if filled
					      (w32api::create-brush :color (list (round-coordinate (* 255 r))
										 (round-coordinate (* 255 g))
										 (round-coordinate (* 255 b))))
					      (w32api::get-stock-object :NULL_BRUSH))
					  :delete-p t)
	      (w32api.gdi32::Polygon (w32-medium-dc medium) points count)))
	  (w32api.gdi32::Polyline (w32-medium-dc medium) points count)))))

(defmethod medium-draw-rectangle* ((medium w32-medium) x1 y1 x2 y2 filled)
  (multiple-value-bind (r g b)
      (medium-rgb medium (medium-ink medium))
    (w32api::with-drawing-object ((w32-medium-dc medium)
				  (if filled
				      (w32api::create-brush :color (list (round-coordinate (* 255 r))
									 (round-coordinate (* 255 g))
									 (round-coordinate (* 255 b))))
				      (w32api::get-stock-object :NULL_BRUSH))
				  :delete-p t)
      (w32api.gdi32::Rectangle (w32-medium-dc medium)
			       (round-coordinate x1) (round-coordinate y1)
			       (round-coordinate x2) (round-coordinate y2)))))

(defmethod medium-draw-rectangles* ((medium w32-medium) position-seq filled)
  (climi::do-sequence ((left top right bottom) position-seq)
    (medium-draw-rectangle* medium left top right bottom filled)))

(defmethod medium-draw-ellipse* ((medium w32-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (multiple-value-bind (r g b)
      (medium-rgb medium (medium-ink medium))
    (w32api::with-drawing-object ((w32-medium-dc medium) (if filled
							     (w32api::create-brush :color (list (round-coordinate (* 255 r))
												(round-coordinate (* 255 g))
												(round-coordinate (* 255 b))))
							     (w32api::get-stock-object :NULL_BRUSH)) :delete-p t)
      (if (= (mod (- end-angle start-angle) (* 2 pi)) 0.0d0)
	  (w32api.gdi32::Ellipse (w32-medium-dc medium)
				 (round-coordinate (- center-x radius-1-dx))
				 (round-coordinate (- center-y radius-2-dy))
				 (round-coordinate (+ center-x radius-1-dx))
				 (round-coordinate (+ center-y radius-2-dy)))
	  (w32api.gdi32::ArcTo (w32-medium-dc medium)
			       (round-coordinate (- center-x radius-1-dx))
			       (round-coordinate (- center-y radius-2-dy))
			       (round-coordinate (+ center-x radius-1-dx))
			       (round-coordinate (+ center-y radius-2-dy))
			       (round-coordinate (* (+ center-x radius-1-dx) (cos start-angle)))
			       (round-coordinate (* (+ center-y radius-2-dy) (sin start-angle)))
			       (round-coordinate (* (+ center-x radius-1-dx) (cos end-angle)))
			       (round-coordinate (* (+ center-y radius-2-dy) (sin end-angle))))))))

(defmethod medium-draw-circle* ((medium w32-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (medium-draw-ellipse* medium center-x center-y radius 0 0 radius start-angle end-angle filled))

(defmethod text-style-ascent (text-style (medium w32-medium))
  (w32api::with-drawing-object ((w32-medium-dc medium) (when text-style (text-style-mapping (port (medium-sheet medium)) text-style)))
    (w32api::get-text-ascent (w32-medium-dc medium))))

(defmethod text-style-descent (text-style (medium w32-medium))
  (w32api::with-drawing-object ((w32-medium-dc medium) (when text-style (text-style-mapping (port (medium-sheet medium)) text-style)))
    (w32api::get-text-descent (w32-medium-dc medium))))

(defmethod text-style-height (text-style (medium w32-medium))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-character-width (text-style (medium w32-medium) char)
  (declare (ignore char))
  (w32api::with-drawing-object ((w32-medium-dc medium) (when text-style (text-style-mapping (port (medium-sheet medium)) text-style)))
    (w32api::get-text-char-width (w32-medium-dc medium))))

;;; FIXME: this one is nominally backend-independent
(defmethod text-style-width (text-style (medium w32-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size ((medium w32-medium) string &key text-style (start 0) end)
  (let* ((string (if (stringp string) string
		     (string string)))
	 (end (or end (length string)))
	 (text-style (or text-style (medium-text-style medium))))
    (values-list
     (w32api::with-drawing-object ((w32-medium-dc medium) (text-style-mapping (port (medium-sheet medium)) text-style))
       (cond ((= start end) (values 0 0 0 0 0))
	     (t
	      (let ((position-newline (position #\newline string :start start :end end)))
		(cond ((not (null position-newline))
		       (multiple-value-bind (cx cy)
			   (w32api::get-text-extent (w32-medium-dc medium) (subseq string start position-newline))
			 (multiple-value-bind (w h x y baseline)
			     (text-size medium string :text-style text-style :start (1+ position-newline) :end end)
			   (list (max w cx) (+ cy h) x (+ cy y) (+ cy baseline)))))
		      (t
		       (multiple-value-bind (cx cy)
			   (w32api::get-text-extent (w32-medium-dc medium) string)
			 (list cx cy cx 0 (text-style-ascent text-style medium))))))))))))

(defmethod climi::text-bounding-rectangle*
    ((medium w32-medium) string &key text-style (start 0) end)
  (text-size medium string :text-style text-style :start start :end end))

(defmethod medium-draw-text* ((medium w32-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore align-x align-y
		   toward-x toward-y transform-glyphs))
  (multiple-value-bind (r g b)
      (medium-rgb medium (medium-ink medium))
    (w32api::with-background-mode ((w32-medium-dc medium) :OPAQUE)
      (let ((string (subseq string start end))
	    (old-color (w32api.gdi32::SetTextColor (w32-medium-dc medium) (w32api::make-rgb-color (round-coordinate (* 255 r))
												  (round-coordinate (* 255 g))
												  (round-coordinate (* 255 b))))))
	(w32api.gdi32::TextOutW (w32-medium-dc medium) (round-coordinate x) (round-coordinate y) string (length string))
	(w32api.gdi32::SetTextColor (w32-medium-dc medium) old-color)))
    t))

#+nil
(defmethod medium-buffering-output-p ((medium w32-medium))
  t)

#+nil
(defmethod (setf medium-buffering-output-p) (buffer-p (medium w32-medium))
  buffer-p)

(defmethod medium-draw-glyph ((medium w32-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  (declare (ignore element x y
		   align-x align-y toward-x toward-y
		   transform-glyphs))
  nil)

(defmethod medium-finish-output ((medium w32-medium))
  (w32api:update-window (sheet-mirror (medium-sheet medium))))

(defmethod medium-force-output ((medium w32-medium))
  (w32api:update-window (sheet-mirror (medium-sheet medium))))

(defmethod medium-clear-area ((medium w32-medium) left top right bottom)
  (w32api:invalidate-rect (sheet-mirror (medium-sheet medium))
			  (round-coordinate left)
			  (round-coordinate top)
			  (round-coordinate right)
			  (round-coordinate bottom)))

(defmethod medium-beep ((medium w32-medium))
  (declare (ignore medium))
  nil)

(defmethod invoke-with-special-choices (continuation (medium w32-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

(defmethod medium-miter-limit ((medium w32-medium))
  0)

;;; FIXME: need these to stop the default method attempting to do
;;; pixmaps, which it appears the null backend doesn't support yet.
(defmethod climi::medium-draw-bezier-design* 
    ((medium w32-medium) (design climi::bezier-area))
  nil)
(defmethod climi::medium-draw-bezier-design* 
    ((medium w32-medium) (design climi::bezier-union))
  nil)
(defmethod climi::medium-draw-bezier-design* 
    ((medium w32-medium) (design climi::bezier-difference))
  nil)
