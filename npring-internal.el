;; -*- lexical-binding: t -*-
(require 'eieio)
(defclass npring--internal-orange nil
  ((segments       :protection :protected :initarg :seg :type (or nil vector)
		   :documentation "segs hold objects")
   (max-length     :protection :protected :initarg :mlen :type integer
		   :documentation "number of segs")
   (current-length :protection :protected :type integer :initform 0
		   :documentation "number of segs which holds an object")
   (mutation-idx   :protection :private :type integer :initform 0
		   :documentation "idx finger-pointing where the next mutation occurs")
   (oldest-idx     :protection :private :type (or null integer) :initform nil
	           :documentation "idx finger-pointing where the most oldest object is in")

   (acceptor       :protection :protected :initarg :af :type (or null function)
		   :documentation "predicate function determines whether enqueued one is acceptable a(enqueued) => boolean")
   (acceptor-str   :protection :protected :initarg :af-str :type (or null string)
		   :documentation "string appended to err-message acceptor throws")
   (constructor    :protection :protected :initarg :cf :type (or null function)
		   :documentation "creates an object based on a enqueued object c(enqueued) => obj")
   (mutator        :protection :protected :initarg :mf :type (or null function)
		   :documentation "mutates an object based on a enqueued object m(obj enqueued) => nil")
   (destructor     :protection :protected :initarg :df :type (or null function)
		   :documentation "kills an object that was in segments d(obj) => nil")))

(defun npring--internal-make-npring (number-of-segs &optional acceptor acceptor-str
					            constructor mutator destructor)
  (npring--internal-orange :seg (make-vector number-of-segs nil)
			   :mlen number-of-segs
			   :af acceptor :af-str acceptor-str
			   :cf constructor :mf mutator :df destructor))
(cl-defmethod npring--internal-enqueued-acceptp ((nio npring--internal-orange) enqueued)
  (let ((af (oref nio acceptor))
	(afs  (oref nio acceptor-str)))
    (unless (or (null af) (funcall af enqueued))
      (let* ((es (format "Enqueued object %S is not acceptable" enqueued))
	     (estr (concat es (if afs (concat ", " afs) ""))))
	(error estr)))))
(cl-defmethod npring--internal-construct-object ((nio npring--internal-orange) enqueued)
  (let ((cf (oref nio constructor)))
    (if cf (funcall cf enqueued) enqueued)))
(cl-defmethod npring--internal-destruct-object ((nio npring--internal-orange))
  (let ((df (oref nio destructor)) (oi (oref nio oldest-idx)) (seg (oref nio segments)))
    (when df (funcall df (aref seg oi)))))
(cl-defmethod npring--internal-mutate-object  ((nio npring--internal-orange) enqueued)
  (let ((mf (oref nio mutator)) (oi (oref nio oldest-idx)) (seg (oref nio segments)))
    (when mf (funcall mf (aref seg oi) enqueued))))
(cl-defmethod npring--internal-1+idx ((nio npring--internal-orange) idx)
  (let ((ml (oref nio max-length))) (mod (1+ idx) ml)))
(cl-defmethod npring--internal-enqueue ((nio npring--internal-orange) enqueued
					&optional force-kill)
  (npring--internal-enqueued-acceptp nio enqueued)
  (let ((os (oref nio segments)) (ml (oref nio max-length)) (cl (oref nio current-length))
        (mi (oref nio mutation-idx)) (oi (oref nio oldest-idx))
	(mf (oref nio mutator)) (df (oref nio destructor)))
    (when (and force-kill (null df))
      (error "no destructor assigned"))
    (cond ((< cl ml) ;pattern1 there exists a room remained
	   (aset os mi (npring--internal-construct-object nio enqueued))
	   (cond ((eq cl 0);pattern1a first enqueuing
		  (setf (oref nio oldest-idx) mi ;init oldest
			(oref nio mutation-idx) (npring--internal-1+idx nio mi);mutate++
		        (oref nio current-length) 1))
		 (t;pattern1b  current-length ∈ (1, max-length)
		  (setf  (oref nio mutation-idx) (npring--internal-1+idx nio mi)
			 (oref nio current-length) (1+ cl)))))
	                         ;;pattern1 ends here(´・ω・｀)
	  (t  ;pattern2 all segments are filled (cl==ml)
	   (cond (force-kill;pattern2a forced into killing
		  (npring--internal-destruct-object nio);kills oldest
		  (aset  os oi (npring--internal-construct-object nio enqueued)))
		 (mf;pattern2b mutator exists
		  (npring--internal-mutate-object nio enqueued))
		 (t;pattern2c not having mutator nor forced to kill
		  (npring--internal-destruct-object nio);but kill?(´・ω・｀)
		  (aset  os oi (npring--internal-construct-object nio enqueued))))
	   ;;in any case, pattern2 ends with 1-increments of both indices
	   (setf (oref nio oldest-idx) (npring--internal-1+idx nio oi)
		 (oref nio mutation-idx) (npring--internal-1+idx nio mi))))))
;; (setf some (npring--internal-make-npring 4))
;; (npring--internal-enqueue some 37)
;; (npring--internal-enqueue some 38)
;; (npring--internal-enqueue some 39)
(cl-defmethod npring--internal-dequeue ((nio npring--internal-orange))
  (let ((ml (oref nio max-length)) (cl (oref nio current-length))
	(oi (oref nio oldest-idx)) (os (oref nio segments)))
    (when (< 0 cl)
      (npring--internal-destruct-object nio)
      (setf (oref nio current-length) (1- cl)
	    (oref nio oldest-idx) (if (eq cl 1) nil
				    (npring--internal-1+idx nio oi)))
      (aset os oi nil))))
;; (npring--internal-dequeue some)
;; (npring--internal-enqueue some 40)
;; (npring--internal-enqueue some 41)
;; (npring--internal-enqueue some 42)



(cl-defmethod npring--internal-length ((nio npring--internal-orange))
  (oref nio current-length))
(cl-defmethod npring--internal-elems ((nio npring--internal-orange))
  (let ((cl (npring--internal-length nio)))
    (when (< 0 cl)
      (let* ((seg (oref nio segments)) (oi (oref nio oldest-idx))
	     (rightwing (seq-take (seq-drop seg oi) cl))
	     (leftwing  (seq-take seg (- cl (length rightwing)))))
	(seq-concatenate 'list rightwing leftwing)))))
;; (npring--internal-elems some)

(provide 'npring-internal)
