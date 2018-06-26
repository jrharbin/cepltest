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
	(let ((bi
		(ecase shape
		  (cone (nineveh.mesh.data.primitives:cone-gpu-arrays))
		  (lattice (nineveh.mesh.data.primitives:lattice-gpu-arrays))
		  (cube (nineveh.mesh.data.primitives:cube-gpu-arrays))
		  (cylinder (nineveh.mesh.data.primitives:cylinder-gpu-arrays)))))
	  (make-buffer-stream (car bi)
			      :index-array (cadr bi)))))

(defun-g s-vert ((pos :vec3) &uniform (m :mat4))
  (let* ((vert (* m (v! pos 1)))
	 (col (+ pos (v! 1.0 0.5 0.5))))
    (values vert col)))

(defun-g s-frag ((col :vec3))
  col)

(defpipeline-g pipe-test ()
  (s-vert :vec3) (s-frag :vec3))

(defun draw-with-matrix (m)
  (clear)
  (map-g #'pipe-test *buf-stream* :m m)
  (swap))

(draw-with-matrix (rtg-math.matrix4:rotation-z 1.0))

(defun now () (/ (get-internal-real-time) 5000.0))

(defmacro make-shape-loop (name)
  `(nineveh:def-simple-main-loop ,name (:on-start (lambda () (init ',name)))
    (draw-with-matrix
     (rtg-math.matrix4:* (rtg-math.matrix4:rotation-z (now))
			 (rtg-math.matrix4:rotation-x (/ (now) 0.1))))))

(make-shape-loop cube)
(make-shape-loop cylinder)
(make-shape-loop lattice)
(make-shape-loop cone)

(cube :start)
