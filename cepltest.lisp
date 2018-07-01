(defpackage :ptest (:use
		    :common-lisp
		    :cl-user
		    :skitter
		    :skitter.sdl2.keys
		    :cepl
		    :vari
		    :nineveh
		    :rtg-math))

(in-package :ptest)

(defparameter *buf-stream* nil)
(defparameter *tex* nil)
(defparameter *sampler* nil)

(defun setup-tex ()
  (setf *tex* (dirt:load-image-to-texture "/home/jharbin/addie.png")))

(defun init (shape)
  (unless *tex* (setup-tex))
  (unless (current-surface) (cepl:repl))
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
  (let* ((xid (mod gl-instance-id 3))
	 (yid (/ gl-instance-id 3))
	 (vert (* perspective (+ (* mat (v! pos 1))
				 (v! offset 0)
				 (v! (* (1- xid) 2) (* (1- yid) 2) 0 0))))
	 (col (+ pos (v! 0.5 0.5 0.5))))
    (values vert (:smooth col) (v! (1- (x pos)) (1- (y pos))))))

(defun-g s-frag ((col :vec3)
		 (uv :vec2)
		 &uniform (sam :sampler-2d))
  (texture sam (/ uv 2))
  ;;col
  ;;(v! 1.0 1.0 1.0)
  )

(defpipeline-g pipe-test ()
  (s-vert :vec3) (s-frag :vec3 :vec2))

(defun set-res ()
  (setf (resolution (current-viewport))
	(surface-resolution (current-surface))))

(defparameter *persp*
  (rtg-math.projection:perspective 10f0 10f0 0.1 30f0 60f0)) 

(defun draw-with-matrix (&key mat offset)
  (set-res)
;;  (format t "~A~%" (mouse-pos (mouse)))
  (step-host)
  (clear)
  (with-instances 6
    (map-g #'pipe-test *buf-stream* :mat mat
				    :offset offset
				    :perspective *persp*
				    :sam (sample *tex*)))
  (swap))

(defun now () (/ (get-internal-real-time) 5000.0))

(defparameter *offsetz* 0.0)

(defun register-keys ()
  (when (keyboard-button (keyboard 0) key.escape)
    (format t "ESC~%"))
  (when (keyboard-button (keyboard 0) key.left)
    (decf *offsetz* 0.1))
  (when (keyboard-button (keyboard 0) key.right)
    (incf *offsetz* 0.1)))

(defparameter *shape* 'cube)

(nineveh:def-simple-main-loop testloop (:on-start (lambda () (init *shape*)))
  (let ((offsetx (sin (/ (get-internal-real-time) 2554.0)))
	(offsety (cos (/ (get-internal-real-time) 2500.0))))
    (register-keys)
    (draw-with-matrix
     :mat
     (rtg-math.matrix4:* (rtg-math.matrix4:rotation-z (now))
			 (rtg-math.matrix4:rotation-y *offsetz*)
			 (rtg-math.matrix4:rotation-x (/ (now) 0.5)))
     :offset (v! offsetx offsety -5.5f0))))

(testloop :start)

(defun exit ()
  (cl-user::exit))
