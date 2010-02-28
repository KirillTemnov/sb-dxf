;;; -*- Mode: Lisp -*-

(defpackage :sb-dxf-system  
  (:use :common-lisp :asdf))

(in-package :sb-dxf-system)

(defsystem sb-dxf
  :version "0.1.2"
  :licence "BSD"
  :description "sb-dxf"
  :long-description "Lisp implementation of dxf writer"
  :author "Kirill Temnov allselead@gmail.com"
  :components ((:file "sb-dxf")
	       (:doc-file "README.txt")
	       (:doc-file "licence.txt")))

