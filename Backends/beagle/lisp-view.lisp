;;; -*- Mode: Lisp; Package: beagle; -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2003, 2004 by
;;;           Duncan Rose (duncan@robotcat.demon.co.uk)

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

(in-package :beagle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create a subclass of NSView that will behave as we want for CLIM.
;;; Note that all the OpenMCL objective c stuff is currently in package
;;; ccl, so we need to make this stuff look a little ugly for it to
;;; work.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define an obj-c class for the view we use as a mirror for clim
;(def-objc-class lisp-view ns-view
;  )

;;; Note that whilst the "tracking rect" is apparently an NSTrackingRectTag
;;; according to the docs, this is actually just a typedef to an int
;;; (in system/Library/Frameworks/AppKit.framework/Headers/NSView.h)
(defclass lisp-view (ns:ns-view)
  ((bgcolour :foreign-type :id  :accessor view-background-colour)
   (trckrect :foreign-type :int :initform -1 :accessor view-tracking-rectangle)
   (eventmask :foreign-type :int :accessor view-event-mask))
  (:metaclass ns:+ns-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Settings that act as hints or instructions to Cocoa.
;;;

;;; This is only a hint to the window manager... so leave it for now.
;;;(define-objc-method ((:<BOOL> is-opaque) lisp-view)
;;;  (if (eql (view-background-colour self) (%null-ptr))
;;;      #$NO
;;;  #$YES))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cocoa has the origin in the *bottom left* of the window. CLIM appears
;; to assume (or at least, the McCLIM implementation of it) that the
;; origin is in the *top left*. Indicate that the coordinate system for
;; drawing in a view is flipped. ::FIXME:: should do this with a better
;; transform...
(define-objc-method ((:<BOOL> is-flipped) lisp-view)
  #$YES)

;;;(define-objc-method ((:void :draw-rect (:<NSR>ect rect)) 
;;;                                         lisp-view)
;;;;;;  (send-super :draw-rect rect)  ; <- is this right?
;;;  (slet ((bounds (send self 'bounds)))
;;;    (if (eql (%null-ptr) (view-background-colour self))
;;;        (send (the ns-color (send (@class ns-color) 'white-color)) 'set)
;;;      (send (the ns-color (view-background-colour self)) 'set))
;;;    (#_NSRectFill bounds)))

(define-objc-method ((:void :draw-string string
				 :at-point (:<NSP>oint point)
				 :with-attributes attr
				 :in-colour colour
				 :with-width (:float width)
				 :with-cap-style (:int cap)
				 :with-join-style (:int join)) lisp-view)
  (when (send self 'lock-focus-if-can-draw)
    (send (the ns-color colour) 'set)
    (send (@class ns-bezier-path) :set-default-line-width width)
    (send (@class ns-bezier-path) :set-default-line-cap-style cap)
    (send (@class ns-bezier-path) :set-default-line-join-style join)
    (send string :draw-at-point point :with-attributes attr)
    (send (send self 'window) 'flush-window)
    (send self 'unlock-focus)))

(define-objc-method ((:void :stroke-path path :in-colour colour) lisp-view)
  (when (send self 'lock-focus-if-can-draw)
    (send (the ns-color colour) 'set)      ; colour for current graphics context
    (send path 'stroke)
    (send (send self 'window) 'flush-window)
    (send self 'unlock-focus)))

(define-objc-method ((:void :fill-path path :in-colour colour) lisp-view)
  (when (send self 'lock-focus-if-can-draw)
    (send (the ns-color colour) 'set)      ; colour for current graphics context
    (send path 'fill)
    (send (send self 'window) 'flush-window)
    (send self 'unlock-focus)))

(define-objc-method ((:id :copy-bitmap-from-region (:<NSR>ect rect)) lisp-view)
  (debug-log 1 "lisp-view -> copy-bitmap-from-region (~A ~A ~A ~A)~%"
	     (pref rect :<NSR>ect.origin.x)
	     (pref rect :<NSR>ect.origin.y)
	     (pref rect :<NSR>ect.size.width)
	     (pref rect :<NSR>ect.size.height))
  (if (send self 'lock-focus-if-can-draw)
      (progn
	(debug-log 1 "focus is locked~%")
	(let ((bitmap (send (send (@class ns-bitmap-image-rep) 'alloc) :init-with-focused-view-rect rect)))
	  (debug-log 1 "Got bitmap ~S = ~S~%" bitmap (ccl::description bitmap))
	  (send bitmap 'retain)
	  (send self 'unlock-focus)
	  bitmap))
    (progn
;;;      (format *debug-io* "(copy-bitmap...) - FAILED TO LOCK FOCUS ON VIEW ~S!!!~%" self)
      nil)))

(define-objc-method ((:void :paste-bitmap bitmap :to-point (:<NSP>oint point)) lisp-view)
  (debug-log 1 "lisp-view -> paste-bitmap entered~%")
  ;; "fraction" defines the opacity of the bitmap.
  
;  NSImage image = [[NSImage alloc] initWithData:[bitmap TIFFRepresentation]];
;  // We can also use "composite" methods in NSImage to do flipping (probably).
;  [image dissolveToPoint:point fraction:1.0];
;  [image release];

  (if (send self 'lock-focus-if-can-draw)
      (progn
	(let ((image (send (send (@class ns-image) 'alloc) :init-with-data (send bitmap "TIFFRepresentation"))))
	  (send image :dissolve-to-point point :fraction 1.0))
	(send (send self 'window) 'flush-window)
	(send self 'unlock-focus))))
;;;    (format *debug-io* "(paste-bitmap...) - FAILED TO LOCK FOCUS ON VIEW ~S!!!~%" self)))

;;; ----------------------------------------------------------------------------

;;; Tracking rectangle support. Each view establishes a tracking rectangle
;;; which is the same as the bounds of the view. This enables mouse-enter and
;;; mouse-exit events to be generated properly.

;;; Note that tracking rectangles need to be reset whenever the view's bounds or
;;; frame changes, so we override the default setBounds and setFrame methods.

;;; Also take note that cursor rectangles and tracking rectangles are not the
;;; same!

;;; ----------------------------------------------------------------------------

(define-objc-method ((:void establish-tracking-rect) lisp-view)
  ;; If the view already has a tracking rectangle (each view should have a single tracking
  ;; rectangle, or none) remove it. Since the NSTrackingRectTag is just a synonym for an
  ;; int, and counting them starts at 0, it's a little hard to check if we have one already
  ;; or not. It doesn't appear to hurt though to remove a non-existent tracking rectangle.
;;;  (nslog (format nil "Entered establish-tracking-rect for view ~S" self))
  (when (eq (view-tracking-rectangle self) -1)
    (send self :remove-tracking-rect (view-tracking-rectangle self)))
  ;; Establish a new tracking rectangle for the view's current bounds
  (let ((trrect (send self :add-tracking-rect (send self 'bounds)
					          :owner self
						  :user-data (%null-ptr)
						  :assume-inside #$NO)))
;;;    (nslog (format nil "Just about to set tracking rectangle slot to: ~S for view: ~S" trrect self))  
    (setf (view-tracking-rectangle self) trrect)))

;;; Support method; when the bounds or frame are reset, fill them with whatever
;;; background colour they have set.

(define-objc-method ((:void fill-bounds) lisp-view)
;;;  (nslog (format nil "setting view bounds, about to attempt to lock focus for filling~%"))
  (when (send self 'lock-focus-if-can-draw)
;;;    (nslog (format nil "got lock~%"))
    (if (eql (%null-ptr) (view-background-colour self))
	(send (the ns-color (send (@class ns-color) 'white-color)) 'set)
      (send (the ns-color (view-background-colour self)) 'set))
    (slet ((bounds (send self 'bounds)))
      (#_NSRectFill bounds))
    (send (send self 'window) 'flush-window)
    (send self 'unlock-focus)))

;;; Override the various :set-bounds-size etc. methods so we can reset the tracking
;;; rectangle when they change.

;;;;;; --->8--- start cut for kludgey work-around of OpenMCL bug --->8---
;;;
;;;(define-objc-method ((:void :set-bounds (:<NSR>ect bounds)) lisp-view)
;;;  (send-super :set-bounds bounds)
;;;  (send self 'fill-bounds)  
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :set-bounds-origin (:<NSP>oint point)) lisp-view)
;;;  (send-super :set-bounds-origin point)
;;;  (send self 'fill-bounds)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :set-bounds-rotation (:float angle)) lisp-view)
;;;  (send-super :set-bounds-rotation angle)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :set-bounds-size (:<NSS>ize size)) lisp-view)
;;;  (send-super :set-bounds-size size)
;;;  (send self 'fill-bounds)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :translate-origin-to-point (:<NSP>oint point)) lisp-view)
;;;  (send-super :translate-origin-to-point point)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :scale-unit-square-to-size (:<NSS>ize size)) lisp-view)
;;;  (send-super :scale-unit-square-to-size size)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :rotate-by-angle (:float angle)) lisp-view)
;;;  (send-super :rotate-by-angle angle)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :set-frame (:<NSR>ect frame)) lisp-view)
;;;  (send-super :set-frame frame)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :set-frame-origin (:<NSP>oint point)) lisp-view)
;;;  (send-super :set-frame-origin point)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :set-frame-rotation (:float angle)) lisp-view)
;;;  (send-super :set-frame-rotation angle)
;;;  (send self 'establish-tracking-rect))
;;;
;;;(define-objc-method ((:void :set-frame-size (:<NSS>ize size)) lisp-view)
;;;  (send-super :set-frame-size size)
;;;  (send self 'fill-bounds)
;;;  (send self 'establish-tracking-rect))
;;;
;;;;;; --->8--- end cut for kludgey work-around of OpenMCL bug --->8---

;;; ----------------------------------------------------------------------------

;;; Event handling methods.

;;; Add the event they're invoked with to the "event queue" we define
;;; in the events.lisp file.
;;;
;;; Cocoa docs say if you don't want to handle the event, you should
;;; pass it on to your superclass. So that's what we do.

;;; ----------------------------------------------------------------------------

(define-objc-method ((:void :mouse-moved event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSMouseMovedMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received MOUSE MOVED event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :mouse-down event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSLeftMouseDownMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received MOUSE DOWN event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :mouse-dragged event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSLeftMouseDraggedMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received MOUSE DRAGGED event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :mouse-up event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSLeftMouseUpMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received MOUSE UP event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :mouse-entered event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSMouseEnteredMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received MOUSE ENTERED event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :mouse-exited event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSMouseExitedMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received MOUSE EXITED event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :right-mouse-down event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSRightMouseDownMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received RIGHT MOUSE DOWN event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :right-mouse-dragged event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSRightMouseDraggedMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received RIGHT MOUSE DRAGGED event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :right-mouse-up event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSRightMouseUpMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received RIGHT MOUSE UP event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :other-mouse-down event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSOtherMouseDownMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received OTHER MOUSE DOWN event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :other-mouse-dragged event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSOtherMouseDraggedMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received OTHER MOUSE DRAGGED event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :other-mouse-up event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSOtherMouseUpMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received OTHER MOUSE UP event: ~S" (description event)))
    (add-event-to-queue self event)))

(define-objc-method ((:void :scroll-wheel event) lisp-view)
  (when (> (logand (view-event-mask self) #$NSScrollWheelMask) 0)
;;;    (nslog (format nil "LISP-VIEW: Received SCROLL WHEEL event: ~S" (description event)))
    (add-event-to-queue self event)))

;;; ----------------------------------------------------------------------------

;;; Events after this point are not handled.

(define-objc-method ((:void :flags-changed event) lisp-view)
;;;  (when NSFlagsChangedMask
;;;    (nslog (format nil "LISP-VIEW: Received FLAGS CHANGED event: ~S" (description event)))
    (send-super :flags-changed event))

(define-objc-method ((:void :help-requested event) lisp-view)
;;;  (nslog (format nil "LISP-VIEW: Received HELP REQUESTED event: ~S" (description event)))
  (send-super :help-requested event))

;;; key up / key down are handled by the NSWindow class, *not* the NSView classes.