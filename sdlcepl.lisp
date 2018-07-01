(defpackage :sdlcepl (:use
		      :common-lisp
		      :cl-user
		      :cepl
		      :cepl.sdl2
		      :vari
		      :nineveh
		      :rtg-math))

(in-package :sdlcepl)

(defparameter *buf-stream* nil)
(defparameter *tex* nil)
(defparameter *sampler* nil)

(defparameter *running* t)

(defun-g s-vert ((pos :vec3)
		 &uniform (mat :mat4)
		 (offset :vec3)
		 (perspective :mat4))
  (let* ((xid (mod gl-instance-id 3))
	 (yid (/ gl-instance-id 3))
	 (vert (* perspective (+ (* mat (v! pos 1))
				 (v! offset 0)
				 (v! (* (1- xid) 2) (* (1- yid) 2) 0 0))))
	 (col (+ pos (v! 0.5 0.5 0.5))))
    (values vert (:smooth col) (v! (1- (x pos)) (1- (y pos))))))

(defun-g s-frag ((col :vec3)
		 (uv :vec2))
  (v! 1.0 1.0 1.0))

(defpipeline-g pipe-test ()
  (s-vert :vec3) (s-frag :vec3 :vec2))

(defparameter *matrix* (rtg-math.matrix4:rotation-z 0.25))
(defparameter *offset* (v! 0 0 -5.5f0))

(defparameter *persp* (rtg-math.projection:perspective 10f0 10f0 0.1 30f0 60f0))

(defun draw-with-matrix ()
  (step-host)
  (clear)
  (with-instances 6
    (map-g #'pipe-test *buf-stream* :mat *matrix*
				    :offset *offset*
				    :perspective *persp*))
  (swap))

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with arguments ARGS"
  (apply #'format t msg args)
  ;; Flush to standard out
  (finish-output))

(defparameter *running* t)

(defun process-key (keysym)
  (let ((scancode (sdl2:scancode-value keysym))
	(sym (sdl2:sym-value keysym))
	(mod-value (sdl2:mod-value keysym)))
    (cond
      ;; Key strokes for navigation
      ((sdl2:scancode= scancode 41) (setf *running* nil)))
    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
              sym
              scancode
              mod-value)))

(defun get-cepl-context ()
  (slot-value (slot-value cepl.context::*cepl-context* 'gl-context) 'cepl.context::handle))

(defun main-loop ()
  "Run the game loop that handles input, rendering through the
    render function RENDER-FN, amongst others."
  (setf *running* t)
  (sdl2:with-init (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-gl-context (c (get-cepl-context))
      (sdl2:with-event-loop (:method :poll)
	(:keydown (:keysym keysym)
		  (process-key keysym))
	(:idle ()
	       (draw-with-matrix)
	       (sdl2:gl-swap-window c)
	       (if (not *running*) (sdl2:push-event :quit)))
	(:quit () t)))))
