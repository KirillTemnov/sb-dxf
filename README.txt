sb-dxf copyright 2010 Kirill Temnov

sb-dxf is a standalone library for generating dxf documents.

Installstion:
1) asdl installation
   $ tar -pczf sb-dxf.tar.gz ./sb-dxf/
   $ sbcl
(require 'asdf)
(require 'asdf-install)
(asdf-install:install "sb-dxf.tar.gz")
(asdf:oos 'asdf:compile-op 'sb-dxf)

   You may be need to create symlink before calling (asdf:oos 'asdf:compile-op 'sb-dxf) 
   like this:
   $ cd ~/.sbcl/system
   $ ln -s  ~/.sbcl/site/sb-test/sb-test.asd .
   
   If you know how to avoid of creating symlink manualy, please write me

2) manual
   # create subdirectory in packages lisp directory
   # for sbcl this will be
   $ mkdir ~/.sbcl/site/sb-dxf/ 

   # copy files to that directory
   $ cp ./sb-dxf/* ~/.sbcl/site/sb-dxf/ 
   
   # create symlink to sb-dxf.asd file 
   # for sbcl:
   $ cd ~/.sbcl/system
   $ ln -s  ~/.sbcl/site/sb-test/sb-test.asd .

Using sample:
	
(require 'asdf)
(asdf:oos 'asdf:load-op :sb-dxf)    

(defvar mgr (dxf:create-manager :filename "manager.dxf"))
(dxf:add-object mgr (make-instance 'dxf:dxf-line :start-point '(10 10 0) :end-point '(40 40 0)))
(dxf:add-object mgr (make-instance 'dxf:dxf-line :start-point '(40 10 0) :end-point '(10 40 0)))
(dxf:add-object mgr (make-instance 'dxf:dxf-circle :center-point '(25 25 0) :radius 7.5 ))
(dxf:add-object mgr (make-instance 'dxf:dxf-arc :center-point '(25 25 0) :radius 10 :start-angle 10 :end-angle 80))
(dxf:add-object mgr (make-instance 'dxf:dxf-arc :center-point '(25 25 0) :radius 10 :start-angle 100 :end-angle 170))
(dxf:add-object mgr (make-instance 'dxf:dxf-arc :center-point '(25 25 0) :radius 10 :start-angle 190 :end-angle 260))
(dxf:add-object mgr (make-instance 'dxf:dxf-arc :center-point '(25 25 0) :radius 10 :start-angle 280 :end-angle 350))
(dxf:flush-manager mgr)

    After performing this command in current directory appears file "manager.dxf". 	

Documentation in source file.

This implementation developed under sbcl, so, in other lisps, library may not will work correctly.
