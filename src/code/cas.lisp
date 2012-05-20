(in-package "SB!IMPL")

;;;; COMPARE-AND-SWAP
;;;;
;;;; SB-EXT:COMPARE-AND-SWAP is the public API for now.
;;;;
;;;; Internally our interface has CAS, GET-CAS-EXPANSION, DEFINE-CAS-EXPANDER,
;;;; DEFCAS, and #'(CAS ...) functions -- making things mostly isomorphic with
;;;; SETF.

(defglobal **cas-expanders** (make-hash-table :test #'eq
                                              #-sb-xc-host #-sb-xc-host
                                              :synchronized t))

(define-function-name-syntax cas (list)
  (destructuring-bind (cas symbol) list
    (aver (eq 'cas cas))
    (values t symbol)))

;;; This is what it all comes down to.
(def!macro cas (place old new &environment env)
  "Synonym for COMPARE-AND-SWAP.

Addtionally DEFUN, DEFGENERIC, DEFMETHOD, FLET, and LABELS can be also used to
define CAS-functions analogously to SETF-functions:

  (defvar *foo* nil)

  (defun (cas foo) (old new)
    (cas (symbol-value '*foo*) old new))

First argument of a CAS function is the expected old value, and the second
argument of is the new value. Note that the system provides no automatic
atomicity for CAS functions, nor can it verify that they are atomic: it is up
to the implementor of a CAS function to ensure its atomicity.

EXPERIMENTAL: Interface subject to change."
  (multiple-value-bind (temps place-args old-temp new-temp cas-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list temps place-args)
            (,old-temp ,old)
            (,new-temp ,new))
       ,cas-form)))

(defun get-cas-expansion (place &optional environment)
  #!+sb-doc
  "Analogous to GET-SETF-EXPANSION. Returns the following six values:

 * list of temporary variables

 * list of value-forms whose results those variable must be bound

 * temporary variable for the old value of PLACE

 * temporary variable for the new value of PLACE

 * form using the aforementioned temporaries which performs the
   compare-and-swap operation on PLACE

 * form using the aforementioned temporaries with which to perform a volatile
   read of PLACE

Example:

  (get-cas-expansion '(car x))
  ; => (#:CONS871), (X), #:OLD872, #:NEW873,
  ;    (SB-KERNEL:%COMPARE-AND-SWAP-CAR #:CONS871 #:OLD872 :NEW873).
  ;    (CAR #:CONS871)

  (defmacro my-atomic-incf (place &optional (delta 1) &environment env)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (get-cas-expansion place env)
     (let ((delta-value (gensym \"DELTA\")))
       `(let* (,@(mapcar 'list vars vals)
               (,old ,read-form)
               (,delta-value ,delta)
               (,new (+ ,old ,delta-value)))
          (loop until (eq ,old (setf ,old ,cas-form))
                do (setf ,new (+ ,old ,delta-value)))
          ,new))))

EXPERIMENTAL: Interface subject to change."
  (flet ((invalid-place ()
           (error "Invalid place to CAS: ~S" place)))
    (let ((expanded (sb!xc:macroexpand place environment)))
      (unless (consp expanded)
        ;; FIXME: Allow (CAS *FOO* <OLD> <NEW>), maybe?
        (invalid-place))
      (let ((name (car expanded)))
        (unless (symbolp name)
          (invalid-place))
        (let ((info (gethash name **cas-expanders**)))
          (cond
            ;; CAS expander.
            (info
             (funcall info place environment))

            ;; Structure accessor
            ((setf info (info :function :structure-accessor name))
             (expand-structure-slot-cas info name expanded))

            ;; CAS function
            (t
             (with-unique-names (old new)
               (let ((vars nil)
                     (vals nil)
                     (args nil))
                 (dolist (x (reverse (cdr expanded)))
                   (cond ((constantp x environment)
                          (push x args))
                         (t
                          (let ((tmp (gensymify x)))
                            (push tmp args)
                            (push tmp vars)
                            (push x vals)))))
                 (values vars vals old new
                         `(funcall #'(cas ,name) ,old ,new ,@args)
                         `(,name ,@args)))))))))))

(defun expand-structure-slot-cas (dd name place)
  (let* ((structure (dd-name dd))
         (slotd (find name (dd-slots dd) :key #'dsd-accessor-name))
         (index (dsd-index slotd))
         (type (dsd-type slotd)))
    (unless (eq t (dsd-raw-type slotd))
      (error "Cannot use COMPARE-AND-SWAP with structure accessor ~
                for a typed slot: ~S"
             place))
    (when (dsd-read-only slotd)
      (error "Cannot use COMPARE-AND-SWAP with structure accessor ~
                for a read-only slot: ~S"
             place))
    (destructuring-bind (op arg) place
      (aver (eq op name))
      (with-unique-names (instance old new)
        (values (list instance)
                (list arg)
                old
                new
                `(truly-the (values ,type &optional)
                            (%compare-and-swap-instance-ref
                             (the ,structure ,instance)
                             ,index
                             (the ,type ,old)
                             (the ,type ,new)))
                `(,op ,instance))))))

(def!macro define-cas-expander (accessor lambda-list &body body)
  "Analogous to DEFINE-SETF-EXPANDER. Defines a CAS-expansion for ACCESSOR.
BODY must return six values as specified in GET-CAS-EXPANSION.

Note that the system provides no automatic atomicity for CAS expansion, nor
can it verify that they are atomic: it is up to the implementor of a CAS
expansion to ensure its atomicity.

EXPERIMENTAL: Interface subject to change."
  (with-unique-names (whole environment)
    (multiple-value-bind (body decls doc)
        (parse-defmacro lambda-list whole body accessor
                        'define-cas-expander
                        :environment environment
                        :wrap-block nil)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',accessor **cas-expanders**)
               (lambda (,whole ,environment)
                 ,@(when doc (list doc))
                 ,@decls
                 ,body))))))

(def!macro defcas (&whole form accessor lambda-list function
                  &optional docstring)
  "Analogous to short-form DEFSETF. Defines FUNCTION as responsible
for compare-and-swap on places accessed using ACCESSOR. LAMBDA-LIST
must correspond to the lambda-list of the accessor.

Note that the system provides no automatic atomicity for CAS expansions
resulting from DEFCAS, nor can it verify that they are atomic: it is up to the
user of DEFCAS to ensure that the function specified is atomic.

EXPERIMENTAL: Interface subject to change."
  (multiple-value-bind (reqs opts restp rest keyp keys allowp auxp)
      (parse-lambda-list lambda-list)
    (declare (ignore keys))
    (when (or keyp allowp auxp)
      (error "&KEY, &AUX, and &ALLOW-OTHER-KEYS not allowed in DEFCAS ~
              lambda-list.~%  ~S" form))
    `(define-cas-expander ,accessor ,lambda-list
       ,@(when docstring (list docstring))
       (let ((temps (mapcar #'gensymify
                            ',(append reqs opts
                                      (when restp (list (gensymify rest))))))
             (args (list ,@(append reqs
                                   opts
                                   (when restp (list rest)))))
             (old (gensym "OLD"))
             (new (gensym "NEW")))
         (values temps
                 args
                 old
                 new
                 `(,',function ,@temps ,old ,new)
                 `(,',accessor ,@temps))))))

(def!macro compare-and-swap (place old new)
  #!+sb-doc
  "Atomically stores NEW in PLACE if OLD matches the current value of PLACE.
Two values are considered to match if they are EQ. Returns the previous value
of PLACE: if the returned value is EQ to OLD, the swap was carried out.

PLACE must be an CAS-able place. Built-in CAS-able places are accessor forms
whose CAR is one of the following:

 CAR, CDR, FIRST, REST, SVREF, SYMBOL-PLIST, SYMBOL-VALUE, SVREF, SLOT-VALUE
 SB-MOP:STANDARD-INSTANCE-ACCESS, SB-MOP:FUNCALLABLE-STANDARD-INSTANCE-ACCESS,

or the name of a DEFSTRUCT created accessor for a slot whose declared type is
either FIXNUM or T. Results are unspecified if the slot has a declared type
other then FIXNUM or T.

In case of SLOT-VALUE, if the slot is unbound, SLOT-UNBOUND is called unless
OLD is EQ to SB-PCL:+SLOT-UNBOUND+ in which case SB-PCL:+SLOT-UNBOUND+ is
returned and NEW is assigned to the slot. Additionally, the results are
unspecified if there is an applicable method on either
SB-MOP:SLOT-VALUE-USING-CLASS, (SETF SB-MOP:SLOT-VALUE-USING-CLASS), or
SB-MOP:SLOT-BOUNDP-USING-CLASS.

Additionally, the PLACE can be a anything for which a CAS-expansion has been
specified using DEFCAS, DEFINE-CAS-EXPANDER, or for which a CAS-function has
been defined. (See SB-EXT:CAS for more information.)
"
  `(cas ,place ,old ,new))

;;; Out-of-line definitions for various primitive cas functions.
(macrolet ((def (name lambda-list ref &optional set)
             #!+compare-and-swap-vops
             (declare (ignore ref set))
             `(defun ,name (,@lambda-list old new)
                #!+compare-and-swap-vops
                (,name ,@lambda-list old new)
                #!-compare-and-swap-vops
                (progn
                  #!+sb-thread
                  ,(error "No COMPARE-AND-SWAP-VOPS on a threaded build?")
                  #!-sb-thread
                  (let ((current (,ref ,@lambda-list)))
                    (when (eq current old)
                      ,(if set
                           `(,set ,@lambda-list new)
                           `(setf (,ref ,@lambda-list) new)))
                    current)))))
  (def %compare-and-swap-car (cons) car)
  (def %compare-and-swap-cdr (cons) cdr)
  (def %compare-and-swap-instance-ref (instance index) %instance-ref %instance-set)
  (def %compare-and-swap-symbol-plist (symbol) symbol-plist)
  (def %compare-and-swap-symbol-value (symbol) symbol-value)
  (def %compare-and-swap-svref (vector index) svref))

