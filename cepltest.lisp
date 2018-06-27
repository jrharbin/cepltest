(defpackage :ptest (:use
		    :common-lisp
		    :cl-user
		    :cepl
		    :nineveh
		    :rtg-math))

(in-package :ptest)

(cepl:repl)

(defparameter *buf-stream* nil)

(defun init (shape)
  (setf *buf-stream*
	(let ((bi (ecase shape
		    (cone (nineveh.mesh.data.primitives:cone-gpu-arrays))
		    (lattice (nineveh.mesh.data.primitives:lattice-gpu-arrays))
		    (cube (nineveh.mesh.data.primitives:cube-gpu-arrays))
		    (cylinder (nineveh.mesh.data.primitives:cylinder-gpu-arrays)))))
	  (make-buffer-stream (car bi) :index-array (cadr bi)))))

(defun-g s-vert ((pos :vec3)
		 &uniform (mat :mat4)
		 (offset :vec3)
		 (perspective :mat4))
  (let* ((vert (* perspective (+ (* mat (v! pos 1)) (v! offset 1))))
	 (col (+ pos (v! 0.5 0.5 0.5))))
    (values vert (:smooth col))))

(defun-g s-frag ((col :vec3))
  ;;(v! 0 0 1)
  col)

(defpipeline-g pipe-test ()
  (s-vert :vec3) (s-frag :vec3))

(defun set-res ()
  (setf (resolution (current-viewport))
	(surface-resolution (current-surface))))

(defparameter *persp*
  (rtg-math.projection:perspective 10f0 10f0 0.1 30f0 60f0)) 

(defun draw-with-matrix (&key mat offset)
  (set-res)
  (step-host)
  (clear)
  (map-g #'pipe-test *buf-stream* :mat mat :offset offset :perspective *persp*)
  (swap))

(defun now () (/ (get-internal-real-time) 5000.0))

(defparameter *shape* 'cube)

(nineveh:def-simple-main-loop testloop (:on-start (lambda () (init *shape*)))
  (let ((offsetx (sin (/ (get-internal-real-time) 1500.0)))
	(offsety (cos (/ (get-internal-real-time) 200.0))))
    (draw-with-matrix
     :mat
     (rtg-math.matrix4:* (rtg-math.matrix4:rotation-z (now))
			 (rtg-math.matrix4:rotation-x (/ (now) 0.5)))
     :offset (v! offsetx offsety -2.5f0))))

(testloop :start)
