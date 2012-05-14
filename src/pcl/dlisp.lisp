;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")


;;;; some support stuff for getting a hold of symbols that we need when
;;;; building the discriminator codes. It's OK for these to be interned
;;;; symbols because we don't capture any user code in the scope in which
;;;; these symbols are bound.

(declaim (list *dfun-arg-symbols*))
(defvar *dfun-arg-symbols* '(.ARG0. .ARG1. .ARG2. .ARG3.))

(defun dfun-arg-symbol (arg-number)
  (or (nth arg-number *dfun-arg-symbols*)
      (format-symbol *pcl-package* ".ARG~A." arg-number)))

(declaim (list *slot-vector-symbols*))
(defvar *slot-vector-symbols* '(.SLOTS0. .SLOTS1. .SLOTS2. .SLOTS3.))

(defun slot-vector-symbol (arg-number)
  (or (nth arg-number *slot-vector-symbols*)
      (format-symbol *pcl-package* ".SLOTS~A." arg-number)))

(declaim (inline make-dfun-required-args))
(defun make-dfun-required-args (count)
  (declare (type index count))
  (let (result)
    (dotimes (i count (nreverse result))
      (push (dfun-arg-symbol i) result))))

(defun make-dfun-lambda-list (nargs applyp)
  (let ((required (make-dfun-required-args nargs)))
    (if applyp
        (nconc required
               ;; Use &MORE arguments to avoid consing up an &REST list
               ;; that we might not need at all. See MAKE-EMF-CALL and
               ;; INVOKE-EFFECTIVE-METHOD-FUNCTION for the other
               ;; pieces.
               '(&more .dfun-more-context. .dfun-more-count.))
        required)))

(defun make-dlap-lambda-list (nargs applyp)
  (let* ((required (make-dfun-required-args nargs))
         (lambda-list (if applyp
                          (append required '(&more .more-context. .more-count.))
                          required)))
    ;; Return the full lambda list, the required arguments, a form
    ;; that will generate a rest-list, and a list of the &MORE
    ;; parameters used.
    (values lambda-list
            required
            (when applyp
              '((sb-c::%listify-rest-args
                 .more-context.
                 (the (and unsigned-byte fixnum)
                   .more-count.))))
            (when applyp
              '(.more-context. .more-count.)))))

(defun make-emf-call (nargs applyp fn-variable &optional emf-type)
  (let ((required (make-dfun-required-args nargs)))
    `(,(if (eq emf-type 'fast-method-call)
           'invoke-effective-method-function-fast
           'invoke-effective-method-function)
       ,fn-variable
       ,applyp
       :required-args ,required
       ;; INVOKE-EFFECTIVE-METHOD-FUNCTION will decide whether to use
       ;; the :REST-ARG version or the :MORE-ARG version depending on
       ;; the type of the EMF.
       :rest-arg ,(if applyp
                      ;; Creates a list from the &MORE arguments.
                      '((sb-c::%listify-rest-args
                         .dfun-more-context.
                         (the (and unsigned-byte fixnum)
                           .dfun-more-count.)))
                      nil)
       :more-arg ,(when applyp
                    '(.dfun-more-context. .dfun-more-count.)))))

(defun make-fast-method-call-lambda-list (nargs applyp)
  (list* '.pv. '.next-method-call. (make-dfun-lambda-list nargs applyp)))

;;; Emitting various accessors.

(defun emit-one-class-reader (class-slot-p)
  (emit-reader/writer :reader 1 class-slot-p))

(defun emit-one-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 1 class-slot-p))

(defun emit-one-class-writer (class-slot-p)
  (emit-reader/writer :writer 1 class-slot-p))

(defun emit-two-class-reader (class-slot-p)
  (emit-reader/writer :reader 2 class-slot-p))

(defun emit-two-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 2 class-slot-p))

(defun emit-two-class-writer (class-slot-p)
  (emit-reader/writer :writer 2 class-slot-p))

;;; --------------------------------

(defun emit-one-index-readers (class-slot-p)
  (emit-one-or-n-index-reader/writer :reader nil class-slot-p))

(defun emit-one-index-boundps (class-slot-p)
  (emit-one-or-n-index-reader/writer :boundp nil class-slot-p))

(defun emit-one-index-writers (class-slot-p)
  (emit-one-or-n-index-reader/writer :writer nil class-slot-p))

(defun emit-n-n-readers ()
  (emit-one-or-n-index-reader/writer :reader t nil))

(defun emit-n-n-boundps ()
  (emit-one-or-n-index-reader/writer :boundp t nil))

(defun emit-n-n-writers ()
  (emit-one-or-n-index-reader/writer :writer t nil))

;;; --------------------------------

(defun emit-checking (metatypes applyp)
  (emit-checking-or-caching nil nil metatypes applyp))

(defun emit-caching (metatypes applyp)
  (emit-checking-or-caching t nil metatypes applyp))

(defun emit-in-checking-cache-p (metatypes)
  (emit-checking-or-caching nil t metatypes nil))

(defun emit-constant-value (metatypes)
  (emit-checking-or-caching t t metatypes nil))

;;; --------------------------------

(defvar *precompiling-lap* nil
  "When T GENERATING-LISP returns a LAMBDA form. When NIL, it compiles that same form.")

(defun generate-lisp-p ()
  ;; FIXME:
  ;;
  ;; 1. Some of the "non-generated" versions look like they're
  ;;    going to pretty much always perform at least as well as
  ;;    the generated ones -- as there is little point in generating
  ;;    those. Benchmark and make sure.
  ;;
  ;; 2. While this is enough to get PCL to build, it would be good
  ;;    to figure out which ones are actually needed for bootstrap.
  ;;
  ;; 3. For some generated functions, share similar ones instead of re-generating
  ;;    them all the time, which is good. See *ENABLE-DFUN-CONSTRUCTOR-CACHING*.
  ;;
  ;; 4. Once we know which is fast, we can always take that path, plus add "if
  ;;    the world-lock-is free" as extra precondition for compilng.
  (or *precompiling-lap* (eq 'early **boot-state**)))

(defun emit-default-only (metatypes applyp)
  (if (generate-lisp-p)
      (multiple-value-bind (lambda-list args rest-arg more-arg)
          (make-dlap-lambda-list (length metatypes) applyp)
        (generating-lisp '(emf)
                         lambda-list
                         `(invoke-effective-method-function emf
                                                            ,applyp
                                                            :required-args ,args
                                                            :more-arg ,more-arg
                                                            :rest-arg ,rest-arg)))
      (lambda (emf)
        (declare #.*optimize-speed*)
        (lambda (&rest args)
          (declare (dynamic-extent args))
          (invoke-emf emf args)))))

;;;; Used by slot accessors generators below

(defmacro with-wrapper-and-slots-or-miss
    ((check wrapper slots miss instance &optional value) &body body)
  `(block nil
     (flet ((miss ()
              ,(if value
                   `(return (funcall ,miss ,value ,instance))
                   `(return (funcall ,miss ,instance)))))
       (multiple-value-bind (,wrapper ,@(when slots (list slots)))
           ,(if slots
                `(cond ((std-instance-p ,instance)
                        (let ((w (std-instance-wrapper ,instance)))
                          (if (layout-for-std-class-p w)
                              (values w (std-instance-slots ,instance))
                              (miss))))
                       ((fsc-instance-p ,instance)
                        (let ((w (fsc-instance-wrapper ,instance)))
                          (if (layout-for-std-class-p w)
                              (values w (fsc-instance-slots ,instance))
                          (miss))))
                       (t
                        (miss)))
                `(cond ((std-instance-p ,instance)
                        (let ((w (std-instance-wrapper ,instance)))
                          (if (layout-for-std-class-p w)
                              w
                              (miss))))
                       ((fsc-instance-p ,instance)
                        (let ((w (fsc-instance-wrapper ,instance)))
                          (if (layout-for-std-class-p w)
                              w
                          (miss))))
                       (t
                        (miss))))
         ,@(ecase check
             (:check `((when (invalid-wrapper-p ,wrapper)
                         (miss))))
             ;; We elide the check when we go to probe-cache next,
             ;; since it will miss on invalid wrapper.
             (:probe nil))
         (return (locally ,@body))))))

;;;; Slot accessors using cached indexes.

(defun make-cached-index-slot-boundp (cache miss-fn)
  (declare (cache cache)
           (function miss-fn))
  (named-lambda cached-index-slot-boundp (instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper slots miss-fn instance)
      (let ((layouts (list wrapper)))
        (declare (dynamic-extent layouts))
        (let ((index (nth-value 1 (probe-cache cache layouts))))
          (if index
              (neq +slot-unbound+ (clos-slots-ref slots index))
              (miss)))))))

(defun make-cached-index-slot-value (cache miss-fn)
  (declare (cache cache)
           (function miss-fn))
  (named-lambda cached-index-slot-value (instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper slots miss-fn instance)
      (let ((layouts (list wrapper)))
        (declare (dynamic-extent layouts))
        (let ((index (nth-value 1 (probe-cache cache layouts))))
          (if index
              (let ((value (clos-slots-ref slots index)))
                (if (eq +slot-unbound+ value)
                    (miss)
                    value))
              (miss)))))))

(defun make-cached-index-setf-slot-value (cache miss-fn)
  (declare (cache cache)
           (function miss-fn))
  (named-lambda cached-index-setf-slot-value (value instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper slots miss-fn instance value)
       (let ((layouts (list wrapper)))
         (declare (dynamic-extent layouts))
         (let ((index (nth-value 1 (probe-cache cache layouts))))
           (if index
               (setf (clos-slots-ref slots index) value)
               (miss)))))))

;;;; Slot accessors with caches, for class slots

(defun make-cached-class-slot-boundp (cache cell miss-fn)
  (declare (cache cache)
           (cons cell)
           (function miss-fn))
  (named-lambda cached-class-slot-boundp (instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper nil miss-fn instance)
      (let ((layouts (list wrapper)))
        (declare (dynamic-extent layouts))
        (if (probe-cache cache layouts)
            (neq +slot-unbound+ (cdr cell))
            (miss))))))

(defun make-cached-class-slot-value (cache cell miss-fn)
  (declare (cache cache)
           (cons cell)
           (function miss-fn))
  (named-lambda cached-class-slot-value (instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper nil miss-fn instance)
      (let ((layouts (list wrapper)))
        (declare (dynamic-extent layouts))
        (if (probe-cache cache layouts)
            (let ((value (cdr cell)))
              (if (eq +slot-unbound+ value)
                  (miss)
                  value))
            (miss))))))

(defun make-cached-class-setf-slot-value (cache cell miss-fn)
  (declare (cache cache)
           (cons cell)
           (function miss-fn))
  (named-lambda cached-class-setf-slot-value (value instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper nil miss-fn instance value)
       (let ((layouts (list wrapper)))
         (declare (dynamic-extent layouts))
         (if (probe-cache cache layouts)
             (setf (cdr cell) value)
             (miss))))))

;;;; Slot accessors with fixed indexes

(defun make-cached-1-index-slot-boundp (cache index miss-fn)
  (declare (cache cache)
           (index index)
           (function miss-fn))
  (named-lambda cached-1-index-slot-boundp (instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper slots miss-fn instance)
      (let ((layouts (list wrapper)))
        (declare (dynamic-extent layouts))
        (if (probe-cache cache layouts)
            (neq +slot-unbound+ (clos-slots-ref slots index))
            (miss))))))

(defun make-cached-1-index-slot-value (cache index miss-fn)
  (declare (cache cache)
           (index index)
           (function miss-fn))
  (named-lambda cached-1-index-slot-value (instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper slots miss-fn instance)
      (let ((layouts (list wrapper)))
        (declare (dynamic-extent layouts))
        (if (probe-cache cache layouts)
            (let ((value (clos-slots-ref slots index)))
              (if (eq +slot-unbound+ value)
                  (miss)
                  value))
            (miss))))))

(defun make-cached-1-index-setf-slot-value (cache index miss-fn)
  (declare (cache cache)
           (index index)
           (function miss-fn))
  (named-lambda cached-1-index-setf-slot-value (value instance)
    (with-wrapper-and-slots-or-miss (:probe wrapper slots miss-fn instance value)
       (let ((layouts (list wrapper)))
         (declare (dynamic-extent layouts))
         (if (probe-cache cache layouts)
             (setf (clos-slots-ref slots index) value)
             (miss))))))

;;;; 1-class class slot accessors

(defun fun-layout-p (layout)
  (find (load-time-value (find-layout 'function)) (layout-inherits layout)))

(defun make-1-class-class-slot-value (wrapper-0 cell miss-fn)
  (declare (function miss-fn)
           (cons cell))
  (cond ((not (layout-for-std-class-p wrapper-0))
         miss-fn)
        ((fun-layout-p wrapper-0)
         (named-lambda 1-class-fsc-class-slot-value (instance)
           (block nil
             (when (and (fsc-instance-p instance)
                        (eq wrapper-0 (fsc-instance-wrapper instance))
                        (not (invalid-wrapper-p wrapper-0)))
                  (let ((value (cdr cell)))
                    (unless (eq +slot-unbound+ value)
                      (return value))))
             (return (funcall miss-fn instance)))))
        (t
         (named-lambda 1-class-std-class-slot-value (instance)
           (block nil
             (when (and (std-instance-p instance)
                        (eq wrapper-0 (std-instance-wrapper instance))
                        (not (invalid-wrapper-p wrapper-0)))
               (let ((value (cdr cell)))
                 (unless (eq +slot-unbound+ value)
                   (return value))))
             (return (funcall miss-fn instance)))))))

(defun make-1-class-setf-class-slot-value (wrapper-0 cell miss-fn)
  (declare (function miss-fn)
           (cons cell))
  (cond ((not (layout-for-std-class-p wrapper-0))
         miss-fn)
        ((fun-layout-p wrapper-0)
         (named-lambda 1-class-fsc-setf-class-slot-value (value instance)
           (if (and (fsc-instance-p instance)
                    (eq wrapper-0 (fsc-instance-wrapper instance))
                    (not (invalid-wrapper-p wrapper-0)))
               (setf (cdr cell) value)
               (funcall miss-fn value instance))))
        (t
         (named-lambda 1-class-std-setf-class-slot-value (value instance)
           (if (and (stf-instance-p instance)
                    (eq wrapper-0 (std-instance-wrapper instance)))
               (setf (cdr cell) value)
               (funcall miss-fn value instance))))))

(defun make-1-class-class-slot-boundp (wrapper-0 cell miss-fn)
  (declare (function miss-fn)
           (cons cell))
  (cond ((not (layout-for-std-class-p wrapper-0))
         miss-fn)
        ((fun-layout-p wrapper-0)
         (named-lambda 1-class-fsc-class-slot-boundp (instance)
           (if (and (fsc-instance-p instance)
                    (eq wrapper-0 (fsc-instance-wrapper instance))
                    (not (invalid-wrapper-p wrapper-0)))
               (neq +slot-unbound+ (cdr cell))
               (funcall miss-fn instance))))
        (t
         (named-lambda 1-class-fsc-class-slot-boundp (instance)
           (if (and (fsc-instance-p instance)
                    (eq wrapper-0 (fsc-instance-wrapper instance))
                    (not (invalid-wrapper-p wrapper-0)))
               (neq +slot-unbound+ (cdr cell))
               (funcall miss-fn instance))))))

;;;; 1-class instance slot accessors

(defun make-1-class-slot-value (wrapper-0 index miss-fn)
  (declare (function miss-fn)
           (index index))
  (cond ((not (layout-for-std-class-p wrapper-0))
         miss-fn)
        ((fun-layout-p wrapper-0)
         (named-lambda 1-class-fsc-slot-value (instance)
           (block nil
             (when (and (fsc-instance-p instance)
                        (eq wrapper-0 (fsc-instance-wrapper instance))
                        (not (invalid-wrapper-p wrapper-0)))
               (let ((value (clos-slots-ref
                             (fsc-instance-slots instance) index)))
                 (unless (eq +slot-unbound+ value)
                   (return value))))
             (return (funcall miss-fn instance)))))
        (t
         (named-lambda 1-class-std-slot-value (instance)
           (block nil
             (when (and (std-instance-p instance)
                        (eq wrapper-0 (std-instance-wrapper instance))
                        (not (invalid-wrapper-p wrapper-0)))
               (let ((value (clos-slots-ref
                             (std-instance-slots instance) index)))
                 (unless (eq +slot-unbound+ value)
                   (return value))))
             (return (funcall miss-fn instance)))))))

(defun make-1-class-setf-slot-value (wrapper-0 index miss-fn)
  (declare (function miss-fn)
           (index index))
  (cond ((not (layout-for-std-class-p wrapper-0))
         miss-fn)
        ((fun-layout-p wrapper-0)
         (named-lambda 1-class-fsc-setf-slot-value (value instance)
           (if (and (fsc-instance-p instance)
                      (eq wrapper-0 (fsc-instance-wrapper instance))
                      (not (invalid-wrapper-p wrapper-0)))
               (setf (clos-slots-ref (fsc-instance-slots instance) index) value)
               (funcall miss-fn value instance))))
        (t
         (named-lambda 1-class-std-setf-slot-value (value instance)
           (if (and (std-instance-p instance)
                    (eq wrapper-0 (std-instance-wrapper instance))
                    (not (invalid-wrapper-p wrapper-0)))
               (setf (clos-slots-ref (std-instance-slots instance) index) value)
               (funcall miss-fn value instance))))))

(defun make-1-class-slot-boundp (wrapper-0 index miss-fn)
  (declare (function miss-fn)
           (index index))
  (cond ((not (layout-for-std-class-p wrapper-0))
         miss-fn)
        ((fun-layout-p wrapper-0)
         (named-lambda 1-class-fsc-slot-boundp (instance)
           (if (and (fsc-instance-p instance)
                    (eq wrapper-0 (fsc-instance-wrapper instance))
                    (not (invalid-wrapper-p wrapper-0)))
               (neq +slot-unbound+
                    (clos-slots-ref (fsc-instance-slots instance) index))
               (funcall miss-fn instance))))
        (t
         (named-lambda 1-class-std-slot-boundp (instance)
           (if (and (std-instance-p instance)
                    (eq wrapper-0 (std-instance-wrapper instance))
                    (not (invalid-wrapper-p wrapper-0)))
               (neq +slot-unbound+
                    (clos-slots-ref (std-instance-slots instance) index))
               (funcall miss-fn instance))))))

;;;; 2-class class slot accessors

(defun make-2-class-class-slot-value (wrapper-0 wrapper-1 cell miss-fn)
  (declare (function miss-fn)
           (cons cell))
  (named-lambda 2-class-class-slot-value (instance)
    (with-wrapper-and-slots-or-miss (:check wrapper slots miss-fn instance)
      (if (or (eq wrapper wrapper-0) (eq wrapper wrapper-1))
          (let ((value (cdr cell)))
            (if (eq +slot-unbound+ value)
                (miss)
                (return value)))
          (miss)))))

(defun make-2-class-class-slot-boundp (wrapper-0 wrapper-1 cell miss-fn)
  (declare (function miss-fn)
           (cons cell))
  (named-lambda 2-class-slot-boundp (instance)
    (with-wrapper-and-slots-or-miss (:check wrapper slots miss-fn instance)
      (if (or (eq wrapper wrapper-0) (eq wrapper wrapper-1))
          (neq +slot-unbound+ (cdr cell))
          (miss)))))

(defun make-2-class-setf-class-slot-value (wrapper-0 wrapper-1 cell miss-fn)
  (declare (function miss-fn)
           (cons cell))
  (named-lambda 2-class-setf-slot-value (value instance)
    (with-wrapper-and-slots-or-miss (:check wrapper slots miss-fn instance value)
      (if (or (eq wrapper wrapper-0) (eq wrapper wrapper-1))
          (setf (cdr cell) value)
          (miss)))))

;;;; 2-class instance slot accessors

(defun make-2-class-slot-value (wrapper-0 wrapper-1 index miss-fn)
  (declare (function miss-fn)
           (index index))
  (named-lambda 2-class-slot-value (instance)
    (with-wrapper-and-slots-or-miss (:check wrapper slots miss-fn instance)
      (if (or (eq wrapper wrapper-0) (eq wrapper wrapper-1))
          (let ((value (clos-slots-ref slots index)))
            (if (eq +slot-unbound+ value)
                (miss)
                value))
          (miss)))))

(defun make-2-class-slot-boundp (wrapper-0 wrapper-1 index miss-fn)
  (declare (function miss-fn)
           (index index))
  (named-lambda 2-class-slot-value (instance)
    (with-wrapper-and-slots-or-miss (:check wrapper slots miss-fn instance)
      (if (or (eq wrapper wrapper-0) (eq wrapper wrapper-1))
          (neq +slot-unbound+ (clos-slots-ref slots index))
          (miss)))))

(defun make-2-class-setf-slot-value (wrapper-0 wrapper-1 index miss-fn)
  (declare (function miss-fn)
           (index index))
  (named-lambda 2-class-setf-slot-value (value instance)
    (with-wrapper-and-slots-or-miss (:check wrapper slots miss-fn instance value)
      (if (or (eq wrapper wrapper-0) (eq wrapper wrapper-1))
          (setf (clos-slots-ref slots index) value)
          (miss)))))

;;; --------------------------------

(defun generating-lisp (closure-variables args form)
  (let ((lambda `(lambda ,closure-variables
                   ,@(when (member 'miss-fn closure-variables)
                           `((declare (type function miss-fn))))
                   #'(lambda ,args
                       (let ()
                         (declare #.*optimize-speed*)
                         ,form)))))
    (values (if *precompiling-lap*
                `#',lambda
                (compile nil lambda))
            nil)))

;;; note on implementation for CMU 17 and later (including SBCL):
;;; Since STD-INSTANCE-P is weakened, that branch may run on non-PCL
;;; instances (structures). The result will be the non-wrapper layout
;;; for the structure, which will cause a miss. The "slots" will be
;;; whatever the first slot is, but will be ignored. Similarly,
;;; FSC-INSTANCE-P returns true on funcallable structures as well as
;;; PCL fins.
(defun emit-reader/writer (reader/writer 1-or-2-class class-slot-p)
  (cond ((generate-lisp-p)
         (let ((instance nil)
               (arglist  ())
               (closure-variables ())
               (read-form (emit-slot-read-form class-slot-p 'index 'slots)))
           (ecase reader/writer
             ((:reader :boundp)
              (setq instance (dfun-arg-symbol 0)
                    arglist  (list instance)))
             (:writer (setq instance (dfun-arg-symbol 1)
                            arglist  (list (dfun-arg-symbol 0) instance))))
           (ecase 1-or-2-class
             (1 (setq closure-variables '(wrapper-0 index miss-fn)))
             (2 (setq closure-variables '(wrapper-0 wrapper-1 index miss-fn))))
           (generating-lisp
            closure-variables
            arglist
            `(let* (,@(unless class-slot-p `((slots nil)))
                    (wrapper (cond ((std-instance-p ,instance)
                                    ,@(unless class-slot-p
                                        `((setq slots
                                                (std-instance-slots ,instance))))
                                    (std-instance-wrapper ,instance))
                                   ((fsc-instance-p ,instance)
                                    ,@(unless class-slot-p
                                        `((setq slots
                                                (fsc-instance-slots ,instance))))
                                    (fsc-instance-wrapper ,instance)))))
               (block access
                 (when (and wrapper
                            (not (zerop (layout-clos-hash wrapper)))
                            ,@(if (eql 1 1-or-2-class)
                                  `((eq wrapper wrapper-0))
                                  `((or (eq wrapper wrapper-0)
                                        (eq wrapper wrapper-1)))))
                   ,@(ecase reader/writer
                       (:reader
                        `((let ((value ,read-form))
                            (unless (eq value +slot-unbound+)
                              (return-from access value)))))
                       (:boundp
                        `((let ((value ,read-form))
                            (return-from access (not (eq value +slot-unbound+))))))
                       (:writer
                        `((return-from access (setf ,read-form ,(car arglist)))))))
                 (funcall miss-fn ,@arglist))))))
        ((eql 1 1-or-2-class)
         (if class-slot-p
             (ecase reader/writer
               (:reader #'make-1-class-class-slot-value)
               (:boundp #'make-1-class-class-slot-boundp)
               (:writer #'make-1-class-setf-class-slot-value))
             (ecase reader/writer
               (:reader #'make-1-class-slot-value)
               (:boundp #'make-1-class-slot-boundp)
               (:writer #'make-1-class-setf-slot-value))))
        ((eql 2 1-or-2-class)
         (if class-slot-p
             (ecase reader/writer
               (:reader #'make-2-class-class-slot-value)
               (:boundp #'make-2-class-class-slot-boundp)
               (:writer #'make-2-class-class-setf-slot-value))
             (ecase reader/writer
               (:reader #'make-2-class-slot-value)
               (:boundp #'make-2-class-slot-boundp)
               (:writer #'make-2-class-setf-slot-value))))))

(defun emit-slot-read-form (class-slot-p index slots)
  (if class-slot-p
      `(cdr ,index)
      `(clos-slots-ref ,slots ,index)))

(defun emit-boundp-check (value-form miss-fn arglist)
  `(let ((value ,value-form))
     (if (eq value +slot-unbound+)
         (funcall ,miss-fn ,@arglist)
         value)))

(defun emit-slot-access (reader/writer class-slot-p slots
                         index miss-fn arglist)
  (let ((read-form (emit-slot-read-form class-slot-p index slots)))
    (ecase reader/writer
      (:reader (emit-boundp-check read-form miss-fn arglist))
      (:boundp `(not (eq ,read-form +slot-unbound+)))
      (:writer `(setf ,read-form ,(car arglist))))))

(defmacro emit-reader/writer-macro (reader/writer 1-or-2-class class-slot-p)
  (let ((*precompiling-lap* t))
    (values
     (emit-reader/writer reader/writer 1-or-2-class class-slot-p))))

(defun emit-one-or-n-index-reader/writer (reader/writer
                                          cached-index-p
                                          class-slot-p)
  ;; Theoretically we could do this too, I guess, but right
  ;; now we don't.
  (aver (not (and cached-index-p class-slot-p)))
  (cond ((generate-lisp-p)
         (multiple-value-bind (arglist metatypes)
             (ecase reader/writer
               ((:reader :boundp)
                (values (list (dfun-arg-symbol 0))
                        '(standard-instance)))
               (:writer (values (list (dfun-arg-symbol 0) (dfun-arg-symbol 1))
                                '(t standard-instance))))
           (generating-lisp
            `(cache ,@(unless cached-index-p '(index)) miss-fn)
            arglist
            `(let (,@(unless class-slot-p '(slots))
                   ,@(when cached-index-p '(index)))
               ,(emit-dlap 'cache arglist metatypes
                           (emit-slot-access reader/writer class-slot-p
                                             'slots 'index 'miss-fn arglist)
                           `(funcall miss-fn ,@arglist)
                           (when cached-index-p 'index)
                           (unless class-slot-p '(slots)))))))
        ((eq :writer reader/writer)
         (cond
           (cached-index-p #'make-cached-index-setf-slot-value)
           (class-slot-p   #'make-cached-class-setf-slot-value)
           (t              #'make-cached-1-index-setf-slot-value)))
        ((eq :boundp reader/writer)
         (cond
           (cached-index-p #'make-cached-index-slot-boundp)
           (class-slot-p   #'make-cached-class-slot-boundp)
           (t              #'make-cached-1-index-slot-boundp)))
        ((eq :reader reader/writer)
         (cond
           (cached-index-p #'make-cached-index-slot-value)
           (class-slot-p   #'make-cached-class-slot-value)
           (t              #'make-cached-1-index-slot-value)))
        (t
         (bug "Bogus reader/writer: ~S" reader/writer))))

(defmacro emit-one-or-n-index-reader/writer-macro
    (reader/writer cached-index-p class-slot-p)
  (let ((*precompiling-lap* t))
    (values
     (emit-one-or-n-index-reader/writer reader/writer
                                        cached-index-p
                                        class-slot-p))))

(defun emit-miss (miss-fn args applyp)
  (if applyp
      `(multiple-value-call ,miss-fn ,@args
                            (sb-c::%more-arg-values .more-context.
                                                    0
                                                    .more-count.))
      `(funcall ,miss-fn ,@args)))

(defun emit-checking-or-caching (cached-emf-p return-value-p metatypes applyp)
  (cond ((generate-lisp-p)
         (multiple-value-bind (lambda-list args rest-arg more-arg)
             (make-dlap-lambda-list (length metatypes) applyp)
           (generating-lisp
            `(cache ,@(unless cached-emf-p '(emf)) miss-fn)
            lambda-list
            `(let (,@(when cached-emf-p '(emf)))
               ,(emit-dlap 'cache args metatypes
                           (if return-value-p
                               (if cached-emf-p 'emf t)
                               `(invoke-effective-method-function
                                 emf ,applyp
                                 :required-args ,args
                                 :more-arg ,more-arg
                                 :rest-arg ,rest-arg))
                           (emit-miss 'miss-fn args applyp)
                           (when cached-emf-p 'emf))))))
        (cached-emf-p
         (if return-value-p
             (lambda (cache miss-fn)
               (declare (function miss-fn))
               (named-lambda cached-emf-getter (&rest args)
                 (declare #.*optimize-speed*)
                 (declare (dynamic-extent args))
                 (with-dfun-wrappers (args metatypes)
                     (dfun-wrappers invalid-wrapper-p)
                     (apply miss-fn args)
                   (if invalid-wrapper-p
                       (apply miss-fn args)
                       (multiple-value-bind (ok emf) (probe-cache cache dfun-wrappers)
                         (if ok
                             emf
                             (apply miss-fn args)))))))
             (lambda (cache miss-fn)
               (declare (type function miss-fn))
               (named-lambda cached-emf-dispatch (&rest args)
                 (declare #.*optimize-speed*)
                 (declare (dynamic-extent args))
                 (with-dfun-wrappers (args metatypes)
                     (dfun-wrappers invalid-wrapper-p)
                     (apply miss-fn args)
                   (if invalid-wrapper-p
                       (apply miss-fn args)
                       (multiple-value-bind (ok emf) (probe-cache cache dfun-wrappers)
                         (if ok
                             (invoke-emf emf args)
                             (apply miss-fn args)))))))))
        (t
         (if return-value-p
             (lambda (cache emf miss-fn)
               (declare (type function miss-fn))
               (named-lambda checking-emf-getter (&rest args)
                 (declare #.*optimize-speed*)
                 (declare (dynamic-extent args))
                 (with-dfun-wrappers (args metatypes)
                     (dfun-wrappers invalid-wrapper-p)
                     (apply miss-fn args)
                   (if invalid-wrapper-p
                       (apply miss-fn args)
                       (let ((found-p (probe-cache cache dfun-wrappers)))
                         (if found-p
                             emf
                             (apply miss-fn args)))))))
             (lambda (cache emf miss-fn)
               (declare (function miss-fn))
               (named-lambda checking-emf-dispatch (&rest args)
                 (declare #.*optimize-speed*)
                 (declare (dynamic-extent args))
                 (with-dfun-wrappers (args metatypes)
                     (dfun-wrappers invalid-wrapper-p)
                     (apply miss-fn args)
                   (if invalid-wrapper-p
                       (apply miss-fn args)
                       (let ((found-p (probe-cache cache dfun-wrappers)))
                         (if found-p
                             (invoke-emf emf args)
                             (apply miss-fn args)))))))))))

(defmacro emit-checking-or-caching-macro (cached-emf-p
                                          return-value-p
                                          metatypes
                                          applyp)
  (let ((*precompiling-lap* t))
    (values
     (emit-checking-or-caching cached-emf-p return-value-p metatypes applyp))))

(defun emit-dlap (cache-var args metatypes hit-form miss-form value-var
                  &optional slot-vars)
  (let* ((index -1)
         (miss-tag (gensym "MISSED"))
         (wrapper-bindings (mapcan (lambda (arg mt)
                                     (unless (eq mt t)
                                       (incf index)
                                       `((,(format-symbol *pcl-package*
                                                          "WRAPPER-~D"
                                                          index)
                                          ,(emit-fetch-wrapper
                                            mt arg miss-tag (pop slot-vars))))))
                                   args metatypes))
         (wrapper-vars (mapcar #'car wrapper-bindings)))
    (declare (fixnum index))
    (unless wrapper-vars
      (error "Every metatype is T."))
    `(prog ()
        (return
          (let ,wrapper-bindings
            ,(emit-cache-lookup cache-var wrapper-vars miss-tag value-var)
            ,hit-form))
      ,miss-tag
        (return ,miss-form))))

(defun emit-fetch-wrapper (metatype argument miss-tag &optional slot)
  (ecase metatype
    ((standard-instance)
     ;; This branch may run on non-pcl instances (structures). The
     ;; result will be the non-wrapper layout for the structure, which
     ;; will cause a miss. Since refencing the structure is rather iffy
     ;; if it should have no slots, or only raw slots, we use FOR-STD-CLASS-P
     ;; to ensure that we have a wrapper.
     ;;
     ;; FIXME: If we unify layouts and wrappers we can use
     ;; instance-slots-layout instead of for-std-class-p, as if there
     ;; are no layouts there are no slots to worry about.
     (with-unique-names (wrapper)
       `(cond
          ((std-instance-p ,argument)
           (let ((,wrapper (std-instance-wrapper ,argument)))
             ,@(when slot
                     `((when (layout-for-std-class-p ,wrapper)
                         (setq ,slot (std-instance-slots ,argument)))))
             ,wrapper))
          ((fsc-instance-p ,argument)
           (let ((,wrapper (fsc-instance-wrapper ,argument)))
             ,@(when slot
                     `((when (layout-for-std-class-p ,wrapper)
                         (setq ,slot (fsc-instance-slots ,argument)))))
             ,wrapper))
          (t (go ,miss-tag)))))
    ;; Sep92 PCL used to distinguish between some of these cases (and
    ;; spuriously exclude others).  Since in SBCL
    ;; WRAPPER-OF/LAYOUT-OF/BUILT-IN-OR-STRUCTURE-WRAPPER are all
    ;; equivalent and inlined to each other, we can collapse some
    ;; spurious differences.
    ((class built-in-instance structure-instance condition-instance)
     (when slot
       (bug "SLOT requested for metatype ~S, but it isnt' going to happen."
            metatype))
     `(wrapper-of ,argument))
    ;; a metatype of NIL should never be seen here, as NIL is only in
    ;; the metatypes before a generic function is fully initialized.
    ;; T should never be seen because we never need to get a wrapper
    ;; to do dispatch if all methods have T as the respective
    ;; specializer.
    ((t nil)
     (bug "~@<metatype ~S seen in ~S.~@:>" metatype 'emit-fetch-wrapper))))
