
(defpackage :sb-dxf
  (:use :common-lisp)
  (:nicknames :dxf)
  (:export dxf-render-error
	   dxf-object
	   dxf-line
	   dxf-point
	   dxf-circle
	   dxf-arc
	   to-dxf
	   dxf-objects-manager
	   create-manager
	   add-object
	   reset-manager
	   flush-manager))

(in-package sb-dxf)
;;--------------------------------------------------------------------------------
;; constants, description from http://www.martinreddy.net/gfx/3d/DXF12.spec
;;--------------------------------------------------------------------------------
(defconstant +group-codes+
  '((begin-mark 0)
    (text-value 1)
    (name 2)
    (text-value-other-1 3)
    (text-value-other-2 4)
    (hex-entity-handle 5)
    (line-type-handle 6)
    (text-style-name 7)
    (layer-name 8)
    (variable-name-id 9)
    (primary-x-coord 10)
    (other-x-coord-1 11)
    (other-x-coord-2 12)
    (other-x-coord-3 13)
    (other-x-coord-4 14)
    (other-x-coord-5 15)
    (other-x-coord-6 16)
    (other-x-coord-7 17)
    (other-x-coord-8 18)
    (primary-y-coord 20)
    (other-y-coord-1 21) 
    (other-y-coord-2 22) 
    (other-y-coord-3 23) 
    (other-y-coord-4 24) 
    (other-y-coord-5 25) 
    (other-y-coord-6 26) 
    (other-y-coord-7 27) 
    (other-y-coord-8 28) 
    (primary-z-coord 30)
    (other-z-coord-1 31)
    (other-z-coord-2 32)
    (other-z-coord-3 33)
    (other-z-coord-4 34)
    (other-z-coord-5 35)
    (other-z-coord-6 36)
    (other-z-coord-7 37)
    (elevation-value--obsolete 38)
    (thickness-value 39)
    (floating-point-value-1 40) 
    (floating-point-value-2 41) 
    (floating-point-value-3 42) 
    (floating-point-value-4 43) 
    (floating-point-value-5 44) 
    (floating-point-value-6 45) 
    (floating-point-value-7 46) 
    (floating-point-value-8 47) 
    (floating-point-value-9 48) 
    (repeated-value 49)
    (angle-1 50) 
    (angle-2 51) 
    (angle-3 52) 
    (angle-4 53) 
    (angle-5 54) 
    (angle-6 55) 
    (angle-7 56) 
    (angle-8 57) 
    (angle-9 58) 
    (color-number 62)
    (entities-follow-flag 66)
    (model-space-or-paper-space 67)
    (viewport-status 68)
    (viewport-id 69)
    (int-value-1 70) 
    (int-value-2 71) 
    (int-value-3 72) 
    (int-value-4 73) 
    (int-value-5 74) 
    (int-value-6 75) 
    (int-value-7 76) 
    (int-value-8 77) 
    (int-value-9 78) 
    (extrusion-direction-x 210)
    (extrusion-direction-y 220)
    (extrusion-direction-z 230)
    (comment 999)
    (ascii-string 1000)
    (registered-application-name 1001)
    (extended-entity-data-control-string 1002)
    (extended-entity-data-layer-name 1003)
    (extended-entity-data-bytes 1004)
    (extended-entity-data-database-handle 1005)
    (extended-entity-data-x-coord 1010)
    (extended-entity-data-y-coord 1020)
    (extended-entity-data-z-coord 1030)
    (extended-entity-data-x-coord-3d-position 1011)
    (extended-entity-data-y-coord-3d-position 1021)
    (extended-entity-data-z-coord-3d-position 1031)
    (extended-entity-data-x-coord-3d-displacement 1012)
    (extended-entity-data-y-coord-3d-displacement 1022)
    (extended-entity-data-z-coord-3d-displacement 1032)
    (extended-entity-data-x-coord-3d-direction 1013)
    (extended-entity-data-y-coord-3d-direction 1023)
    (extended-entity-data-z-coord-3d-direction 1033)
    (extended-entity-data-floating-point-value 1040)
    (extended-entity-data-distance-value 1041)
    (extended-entity-data-scale-factor 1042)
    (extended-entity-data-16bit-signed-integer 1070)
    (extended-entity-data-16bit-signed-long 1071)))
;;--------------------------------------------------------------------------------
;; conditions
;;--------------------------------------------------------------------------------
(define-condition dxf-render-error (error) 
  ((error-element :initarg :error-element :reader error-element :initform nil))
  (:documentation "Error condition in process of rendering dxf file content.")
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Error rendering element : ~s ~%~%" (error-element condition)))))

;;--------------------------------------------------------------------------------
;; helper functions
;;--------------------------------------------------------------------------------
(defun mapconcat (func elems)
  "Execute function on each of elems and concatenate all results in a string."
  (cond 
    ((eq nil elems) "")
    (t
     (concatenate 'string
		  (funcall func (first elems))
		  (mapconcat func (cdr elems))))))

(defun write-to-file (content filename)
  "Write content to file (filename). Overwrite existing file"
  (with-open-file (stream  filename 
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
    (format stream content))
  filename) 

(defun dxf-get-code-by-number (number)
  "Return list of group code and it's numder by specified number."
  (find-if #'(lambda (code-descr) 
	       (eq number (second code-descr)))
	   +group-codes+))

(defun dxf-get-number-by-code (code)
  "Return dxf value of code."
  (let ((code-descr (find-if #'(lambda (code-descr)
				 (eq code (first code-descr)))
			     +group-codes+)))
    (cond 
      ((listp code-descr) (second code-descr))
      (t nil))))
      
(defun format-dxf (code value)
  "Format code and value in DXF format specification"
  (format nil "~s~%~s~%" (dxf-get-number-by-code code) value))

(defun dxf-render-element (elements)
  "render element(s) to dxf format.
:TODO: add code in this function.
"
  (cond 
    ((eq elements nil) "")
    ((stringp elements) elements)
    ((listp elements) (concatenate 'string 
			 (dxf-render-element (first elements)) 
			 (dxf-render-element (cdr elements))))
    (t (error 'dxf-render-error :error-element elements))))
;;--------------------------------------------------------------------------------
;; header section
;;--------------------------------------------------------------------------------
(defun dxf-render-header-section (&optional (header-vars ""))
  "returns header section.
header-vars must be a string"
  (concatenate 'string
	       (format-dxf 'begin-mark 'SECTION)
	       (format-dxf 'name 'HEADER)	      
	       (format-dxf 'variable-name-id '$ACADVER)
	       (format-dxf 'text-value 'AC1006)
	       (dxf-render-element header-vars)
	       (format-dxf 'begin-mark 'ENDSEC)))
;; sample content of header section
;; 9
;; $EXTMIN
;; 10
;; 0.0
;; 20
;; 100.0
;; 9
;; $EXTMAX
;; 10
;; 814.0
;; 20
;; 798.0
;; 9
;; $LIMMIN
;; 10
;; 0.0
;; 20
;; 798.0
;; 9
;; $LIMMAX
;; 10
;; 814.0
;; 20
;; 798.0

(defun dxf-eof nil
  "returns end of dxf file."
  (format-dxf 'begin-mark 'EOF))
;;--------------------------------------------------------------------------------
;; tables section
;;--------------------------------------------------------------------------------
(defun dxf-render-tables-section (&optional (tables-vars ""))
  "Generates default tables section. Not using tables-vars for now."
  (declare (ignore tables-vars))
  (concatenate 'string 
	       (format-dxf 'begin-mark 'SECTION)
	       (format-dxf 'name 'TABLES)
	       (format-dxf 'begin-mark 'TABLE)
	       (format-dxf 'name 'LAYER)
	       (format-dxf 'begin-mark 0)
	       ;; set layer flags
	       ;; 0 - default
	       ;; 1 = Layer is frozen; otherwise layer is thawed. 
	       ;; 2 = Layer is frozen by default in new viewports.
	       ;; 4 = Layer is locked.
	       ;; 16 = If set, table entry is externally dependent on an xref.
	       ;; 32 = If this bit and bit 16 are both set, the externally dependent 
	       ;;      xref has been successfully resolved.
	       ;; 64 = If set, the table entry was referenced by at least one entity
	       ;;       in the drawing the last time the drawing was edited.
	       (format-dxf 'int-value-1 0) 
	       (format-dxf 'begin-mark 'ENDTAB)
	       (format-dxf 'begin-mark 'ENDSEC)))
;;--------------------------------------------------------------------------------
;; block section
;;--------------------------------------------------------------------------------
(defun dxf-render-blocks-section (&optional (blocks ""))
  "Generates default block section. Not using block for now."
  (declare (ignore blocks))
  (concatenate 'string
	       (format-dxf 'begin-mark 'SECTION)
	       (format-dxf 'name 'BLOCKS)
	       (format-dxf 'begin-mark 'ENDSEC)))
;;--------------------------------------------------------------------------------
;; entities section
;;--------------------------------------------------------------------------------
(defun dxf-render-entities-section (entities)
  (concatenate 'string
	       (format-dxf 'begin-mark 'SECTION)
	       (format-dxf 'name 'ENTITIES)
	       (mapconcat #'(lambda (dxf-elem)
			      (to-dxf dxf-elem))
			  entities)
	       (format-dxf 'begin-mark 'ENDSEC)))
;;--------------------------------------------------------------------------------
;; render a file
;;--------------------------------------------------------------------------------
(defun dxf-render-file (entities &key (header "") (tables "")  (blocks ""))
  "Render dxf file as a string. Entities is a list of instances of dxf-object
or derived classes. Every entity instances must support method to-dxf.
Parameters header, tables and blocks not using for now."
  (concatenate 'string
	       (dxf-render-header-section header)
	       (dxf-render-tables-section tables)
	       (dxf-render-blocks-section blocks)
	       (dxf-render-entities-section entities)
	       (dxf-eof)))
;;--------------------------------------------------------------------------------
;; Drawing objects
;;--------------------------------------------------------------------------------
(defclass dxf-object nil
  ((layer-name
    :initarg :layer-name :initform "0")
   (elevation
    :initarg :elevation :initform nil)
   (thickness
    :initarg :thickness :initform nil)
   (linetype
    :initarg :linetype :initform nil)
   (color
    :initarg :color :initform nil)
   (object-type
    :initarg :object-type :initform nil))
  (:documentation "A common class, containing basic properties for drawing element (entities)."))

(defclass dxf-line (dxf-object)
  ((start-point
    :initarg :start-point)
   (end-point
    :initarg :end-point)
   (object-type
    :initarg :object-type :initform 'LINE))
  (:documentation "Class, representing a line. Sample of creating instance:
 (make-instance 'dxf-line :start-point '(0 0 0) :end-point '(100 100 0))"))

;; don't know if it's works. not exporting this class
(defclass dxf-ray (dxf-object)
  ((start-point
    :initarg :start-point)
   (direction-point
    :initarg :direction-point)
   (object-type
    :initarg :object-type :initform 'RAY))
  (:documentation "Class, representing a ray. Sample of creating instance:
  (make-instance 'dxf-ray :start-point '(50 30 0) :direction-point '(0 0 0))"))

(defclass dxf-point (dxf-object)
  ((the-point
    :initarg :the-point)
   (object-type
    :initarg :object-type :initform 'POINT))
  (:documentation "Class, representing a point. Sample of creating instance:
  (make-instance 'dxf-point :the-point '(5 5 0))"))

(defclass dxf-circle (dxf-object)
  ((center-point
    :initarg :center-point)
   (radius
    :initarg :radius)
   (object-type
    :initarg :object-type :initform 'CIRCLE))
  (:documentation "Class, representing a circle. Sample of creating instance:
  (make-instance 'dxf-circle :center-point '(75 75 0) :radius 25 )"))

(defclass dxf-arc (dxf-circle)
  ((start-angle
    :initarg :start-angle)
   (end-angle
    :initarg :end-angle)
   (object-type
    :initarg :object-type :initform 'ARC))
  (:documentation "Class representing an arc. Angles set in degrees. Sample of creating instance:
  (make-instance 'dxf-arc :center-point '(40 50 0) :radius 20 :start-angle 0 :end-angle 90)"))

;; this class is buggy, not works for now. not exporting this class
(defclass dxf-aligned-dimension (dxf-object)
  ((from-point-left
    :initarg :from-point-left)
   (from-point-right
    :initarg :from-point-right)
   (center-text-point
    :initarg :center-text-point)
   (base-point 
    :initarg :base-point)
   (dimension-value
    :initarg :dimension-value :initform "<>")
   (object-type
    :initarg :object-type :initform 'DIMENSION))
  (:documentation 
"
Point positions:
		     |  (center-text-point)  |
		     |<----------.---------->. (base-point)
		     |                       | 
		     |                       |
 (from-point-left)   .                       . (from-point-right)
Sample of creating instance:
 (make-instance 'dxf-aligned-dimension :base-point '(100 20 0) :center-text-point '(60 20 0) :from-point-left '(30 70 0) :from-point-right '(100 70 0))
"))


;;--------------------------------------------------------------------------------
(defgeneric to-dxf (object)
  (:documentation "Transform object to dxf-format representation"))

(defmethod to-dxf ((object dxf-aligned-dimension))
  (let ((base-point         (slot-value object 'base-point))
	(center-text-point  (slot-value object 'center-text-point))
	(from-point-left    (slot-value object 'from-point-left))
	(from-point-right   (slot-value object 'from-point-right)))
    (concatenate 'string
		 ;; call parent class method before writing own properties
		 (call-next-method)
		 (format-dxf 'primary-x-coord (first   base-point))
		 (format-dxf 'primary-y-coord (second  base-point))
		 (format-dxf 'primary-z-coord (third   base-point))
		 (format-dxf 'other-x-coord-3 (first   center-text-point))
		 (format-dxf 'other-y-coord-3 (second  center-text-point))
		 (format-dxf 'other-z-coord-3 (third   center-text-point))
		 (format-dxf 'other-x-coord-4 (first   from-point-left))
		 (format-dxf 'other-y-coord-4 (second  from-point-left))
		 (format-dxf 'other-z-coord-4 (third   from-point-left))
		 (format-dxf 'other-x-coord-5 (first   from-point-right))
		 (format-dxf 'other-y-coord-5 (second  from-point-right))
		 (format-dxf 'other-z-coord-5 (third   from-point-right))
		 (format-dxf 'text-value (slot-value object 'dimension-value))
		 (format-dxf 'int-value-2 5)
		 (format-dxf 'int-value-1 160)
		 )))

(defmethod to-dxf ((object dxf-object))
  (let ((result      (format-dxf 'layer-name (slot-value object 'layer-name)))
	(elevation   (slot-value object 'elevation))
	(thickness   (slot-value object 'thickness))
	(linetype    (slot-value object 'linetype))
	(color       (slot-value object 'color))
	(object-type (slot-value object 'object-type)))
    (when (not (eq nil elevation))
      (setf result (concatenate 'string 
				result 
				(format-dxf 'elevation-value--obsolete elevation))))
    (when (not (eq nil thickness))
      (setf result (concatenate 'string 
				result
				(format-dxf 'thickness-value thickness))))
    (when (not (eq nil linetype))
      (setf result (concatenate 'string
				result
				(format-dxf 'line-type-handle linetype))))
    (when (not (eq nil color))
      (setf result (concatenate 'string
				result
				(format-dxf 'color-number color))))
    ;; object-type must be first record
    (when (not (eq nil object-type))
      (setf result (concatenate 'string
				(format-dxf 'begin-mark object-type)
				result)))
    result))
  
(defmethod to-dxf ((object dxf-line))
  (let ((p0 (slot-value object 'start-point))
	 (p1 (slot-value object 'end-point)))
    (concatenate 'string 
		 ;; call parent class method before writing own properties
		 (call-next-method)
		 (format-dxf 'primary-x-coord (first p0))
		 (format-dxf 'primary-y-coord (second p0))
		 (format-dxf 'primary-z-coord (third p0))
		 (format-dxf 'other-x-coord-1 (first p1))
		 (format-dxf 'other-y-coord-1 (second p1))
		 (format-dxf 'other-z-coord-1 (third p1)))))

(defmethod to-dxf ((object dxf-ray))
  (let ((p0 (slot-value object 'start-point))
	 (p1 (slot-value object 'direction-point)))
    (concatenate 'string 
		 ;; call parent class method before writing own properties
		 (call-next-method)
		 (format-dxf 'primary-x-coord (first p0))
		 (format-dxf 'primary-y-coord (second p0))
		 (format-dxf 'primary-z-coord (third p0))
		 (format-dxf 'other-x-coord-1 (first p1))
		 (format-dxf 'other-y-coord-1 (second p1))
		 (format-dxf 'other-z-coord-1 (third p1)))))

(defmethod to-dxf ((object dxf-point))
  (let ((p (slot-value object 'the-point)))
    (concatenate 'string
		 ;; call parent class method before writing own properties
		 (call-next-method)
		 (format-dxf 'primary-x-coord (first p))
		 (format-dxf 'primary-y-coord (second p))
		 (format-dxf 'primary-y-coord (third p)))))

(defmethod to-dxf ((object dxf-circle))
  (let ((center (slot-value object 'center-point)))
    (concatenate 'string
		 ;; call parent class method before writing own properties
		 (call-next-method)
		 (format-dxf 'primary-x-coord (first center))
		 (format-dxf 'primary-y-coord (second center))
		 (format-dxf 'primary-z-coord (third center))
		 (format-dxf 'floating-point-value-1 (slot-value object 'radius)))))

(defmethod to-dxf ((object dxf-arc))
  (concatenate 'string
	       ;; call parent class method before writing own properties
	       (call-next-method)
	       (format-dxf 'angle-1 (slot-value object 'start-angle))
	       (format-dxf 'angle-2 (slot-value object 'end-angle))))

;;--------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------
(defclass dxf-objects-manager nil
  ((entities-list
    :initarg :entities-list :initform nil)
   (file-to-save
    :initarg :file-to-save :initform nil))
   (:documentation "Manage drawing entities."))
;;--------------------------------------------------------------------------------
(defgeneric create-manager (&key filename)
  (:documentation "Wrapper for make-instance function. You can call make-instance 
directly and get more flexibility."))

(defgeneric add-object (manager entity)
  (:documentation "Add new entity to manager."))

(defgeneric reset-manager (manager)
  (:documentation "Reset manager state (and delete all objects) to default. Slot 
file-to-save is not resets!"))

(defgeneric flush (manager)
  (:documentation "Save all objects to file."))
;;--------------------------------------------------------------------------------
(defmethod create-manager (&key (filename "manager.dxf"))
  (make-instance 'dxf-objects-manager :file-to-save filename))


(defmethod add-object ((manager dxf-objects-manager) (entity dxf-object))
  (cond
    ((eq nil (slot-value manager 'entities-list))
     (setf (slot-value manager 'entities-list) (list entity)))
    (t
     (push entity (slot-value manager 'entities-list)))))

(defmethod reset-manager ((manager dxf-objects-manager))
  (setf (slot-value manager 'entities-list) nil))

(defmethod flush-manager ((manager dxf-objects-manager))
  (when (not (eq nil (slot-value manager 'entities-list)))
    (write-to-file (dxf-render-file (slot-value manager 'entities-list)) 
		   (slot-value manager 'file-to-save))))


