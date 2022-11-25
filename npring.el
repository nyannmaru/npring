(require 'npring-internal)
(defun make-npring (size &optional acceptor acceptor-str constructor mutator destructor)
  ""
  (declare (indent 1))
  (cond ((or (not (integerp size)) (<= size 0))  (error "size need to be a integer larger than 0"))
	((and acceptor-str (not (stringp acceptor-str))) (error "acceptor-str must be a string"))
	
	((and acceptor (not (functionp acceptor)))       (error "given acceptor is not a function"))
	((and constructor (not (functionp constructor))) (error "given constructor is not a function"))
	((and mutator (not (functionp mutator)))         (error "given mutator is not a function"))
	((and destructor (not (functionp destructor)))   (error "given destructor is not a function")))
  (npring--internal-make-npring size acceptor acceptor-str constructor mutator destructor))
;; (setf some (make-npring 5 (lambda (x) (< 5 x)) "need to be larger than 5"))
;; (npring--internal-enqueue some 2)
(defun npring-p (obj)
  "Returns nil if obj is not a npring object othrewise retruns t."
  (npring--internal-orange-p obj))
(defun npring--interface-check (npring)
  (unless (npring-p npring) (error "object feeded to npring is not a npring-objcect")))

(defun npring-empty-p (npring)
  "Returns t if a given npring-object is empty otherwise nil."
  (npring--interface-check npring)
  (eq (npring--internal-length npring) 0))
(defun npring-size (npring)
  "Returns size of npring."
  (npring--interface-check npring)
  (npring--internal-length npring))
(defun npring-enqueue (npring elem &optional use-destructor)
  "Enqueues elem as a newcomer into the given npring-object and returns the size of npring after enqueuing.
If use-destructor is non-nil this function would use the destructor instead of the mutator over the oldest object inside npring when it is inevitable to kill the object in order to hold the newcomer(Note! if destructor is not assigned throws error)."
  (npring--interface-check npring)
  (npring--internal-enqueue npring elem use-destructor)
  (npring-size npring))

(defun npring-dequeue (npring)
  "Dequeues the oldest object inside npring and returns the size of npring after dequeuing."
  (npring--interface-check npring)
  (npring--internal-dequeue npring)
  (npring-size npring))

(defun npring-elems (npring)
  "Returns the list whose elements are all objects inside npring in oldest-to-newest order."
  (npring--interface-check npring)
  (npring--internal-elems npring))
(defun npring-oldest (npring)
  (let ((listed (npring-elems npring)))
    (car listed)))
(defun npring-newest (npring)
  (let ((listed (npring-elems npring)))
    (car (seq-reverse listed))))




(provide 'npring)
