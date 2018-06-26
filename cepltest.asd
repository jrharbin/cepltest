(asdf:defsystem cepltest
  :name "cepltest"
  :version "0.1"
  :maintainer "JRH"
  :author "JRH"
  :license "MIT"
  :description "Test of CEPL"
  :serial t
  :depends-on (:cepl
	       :cepl.sdl2
	       :nineveh
	       :rtg-math)
  :components ((:file "cepltest")))
