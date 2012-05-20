;;;; various extensions (including SB-INT "internal extensions")
;;;; available both in the cross-compilation host Lisp and in the
;;;; target SBCL, but which can't be defined on the target until until
;;;; some significant amount of machinery (e.g. error-handling) is
;;;; defined

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Is X a list for which LENGTH is meaningful, i.e. a list which is
;;; not improper and which is not circular?
(defun list-with-length-p (x)
  (values (ignore-errors (list-length x))))

;;; not used in 0.7.8, but possibly useful for defensive programming
;;; in e.g. (COERCE ... 'VECTOR)
;;;(defun list-length-or-die (x)
;;;  (or (list-length x)
;;;      ;; not clear how to do this best:
;;;      ;;   * Should this be a TYPE-ERROR? Colloquially that'd make
;;;      ;;     lots of sense, but since I'm not sure how to express
;;;      ;;     "noncircular list" as a Lisp type expression, coding
;;;      ;;     it seems awkward.
;;;      ;;   * Should the ERROR object include the offending value?
;;;      ;;     Ordinarily that's helpful, but if the user doesn't have
;;;      ;;     his printer set up to deal with cyclicity, we might not
;;;      ;;     be doing him a favor by printing the object here.
;;;      ;; -- WHN 2002-10-19
;;;      (error "can't calculate length of cyclic list")))

;;; This is used in constructing arg lists for debugger printing,
;;; and when needing to print unbound slots in PCL.
(defstruct (unprintable-object
            (:constructor make-unprintable-object (string))
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s)
                               (write-string (unprintable-object-string x) s))))
            (:copier nil))
  string)

;;; Used internally, but it would be nice to provide something
;;; like this for users as well.
;;;
;;; FIXME / IMPORTANT: If the slot is raw, the address is correct only for
;;; instances of the specified class, not its subclasses!
#!+sb-thread
(defmacro define-structure-slot-addressor (name &key structure slot)
  (let* ((dd (find-defstruct-description structure t))
         (slotd (when dd (find slot (dd-slots dd) :key #'dsd-name)))
         (index (when slotd (dsd-index slotd)))
         (raw-type (dsd-raw-type slotd)))
    (unless index
      (error "Slot ~S not found in ~S." slot structure))
    `(progn
       (declaim (inline ,name))
       (defun ,name (instance)
         (declare (type ,structure instance) (optimize speed))
         (sb!ext:truly-the
          sb!vm:word
          (+ (sb!kernel:get-lisp-obj-address instance)
             (- (* ,(if (eq t raw-type)
                        (+ sb!vm:instance-slots-offset index)
                        (- (1+ (sb!kernel::dd-instance-length dd)) sb!vm:instance-slots-offset index
                           (1- (sb!kernel::raw-slot-words raw-type))))
                   sb!vm:n-word-bytes)
                sb!vm:instance-pointer-lowtag)))))))

;;;; ATOMIC-INCF and ATOMIC-DECF

(defun expand-atomic-frob (name place diff)
  (flet ((invalid-place ()
           (error "Invalid first argument to ~S: ~S" name place)))
    (unless (consp place)
      (invalid-place))
    (destructuring-bind (op &rest args) place
      (case op
        (aref
         (when (cddr args)
           (invalid-place))
         #!+(or x86 x86-64 ppc)
         (with-unique-names (array)
           `(let ((,array (the (simple-array sb!ext:word (*)) ,(car args))))
              (%array-atomic-incf/word
               ,array
               (%check-bound ,array (array-dimension ,array 0) ,(cadr args))
               (logand #.(1- (ash 1 sb!vm:n-word-bits))
                       ,(ecase name
                               (atomic-incf
                                `(the sb!vm:signed-word ,diff))
                               (atomic-decf
                                `(- (the sb!vm:signed-word ,diff))))))))
         #!-(or x86 x86-64 ppc)
         (with-unique-names (array index old-value)
           (let ((incremented-value
                  (ecase name
                         (atomic-incf
                          `(+ ,old-value (the sb!vm:signed-word ,diff)))
                         (atomic-decf
                          `(- ,old-value (the sb!vm:signed-word ,diff))))))
             `(sb!sys:without-interrupts
               (let* ((,array ,(car args))
                      (,index ,(cadr args))
                      (,old-value (aref ,array ,index)))
                 (setf (aref ,array ,index)
                       (logand #.(1- (ash 1 sb!vm:n-word-bits))
                               ,incremented-value))
                 ,old-value)))))
        (t
         (when (cdr args)
           (invalid-place))
         (let ((dd (info :function :structure-accessor op)))
           (if dd
               (let* ((structure (dd-name dd))
                      (slotd (find op (dd-slots dd) :key #'dsd-accessor-name))
                      (index (dsd-index slotd))
                      (type (dsd-type slotd)))
                 (declare (ignorable structure index))
                 (unless (and (eq 'sb!vm:word (dsd-raw-type slotd))
                              (type= (specifier-type type) (specifier-type 'sb!vm:word)))
                   (error "~S requires a slot of type (UNSIGNED-BYTE ~S), not ~S: ~S"
                          name sb!vm:n-word-bits type place))
                 (when (dsd-read-only slotd)
                   (error "Cannot use ~S with structure accessor for a read-only slot: ~S"
                          name place))
                 #!+(or x86 x86-64 ppc)
                 `(truly-the sb!vm:word
                             (%raw-instance-atomic-incf/word
                              (the ,structure ,@args) ,index
                              (logand #.(1- (ash 1 sb!vm:n-word-bits))
                                      ,(ecase name
                                              (atomic-incf
                                               `(the sb!vm:signed-word ,diff))
                                              (atomic-decf
                                               `(- (the sb!vm:signed-word ,diff)))))))
                 ;; No threads outside x86 and x86-64 for now, so this is easy...
                 #!-(or x86 x86-64 ppc)
                 (with-unique-names (structure old)
                                    `(sb!sys:without-interrupts
                                      (let* ((,structure ,@args)
                                             (,old (,op ,structure)))
                                        (setf (,op ,structure)
                                              (logand #.(1- (ash 1 sb!vm:n-word-bits))
                                                      ,(ecase name
                                                              (atomic-incf
                                                               `(+ ,old (the sb!vm:signed-word ,diff)))
                                                              (atomic-decf
                                                               `(- ,old (the sb!vm:signed-word ,diff))))))
                                        ,old))))
             (invalid-place))))))))

(defmacro atomic-incf (place &optional (diff 1))
  #!+sb-doc
  "Atomically increments PLACE by DIFF, and returns the value of PLACE before
the increment.

The incrementation is done using word-size modular arithmetic: on 32 bit
platforms ATOMIC-INCF of #xFFFFFFFF by one results in #x0 being stored in
PLACE.

PLACE must be an accessor form whose CAR is the name of a DEFSTRUCT accessor
whose declared type is (UNSIGNED-BYTE 32) on 32 bit platforms,
and (UNSIGNED-BYTE 64) on 64 bit platforms or an AREF of a (SIMPLE-ARRAY
SB-EXT:WORD (*) -- the type SB-EXT:WORD can be used for this purpose.

DIFF defaults to 1, and must be a (SIGNED-BYTE 32) on 32 bit platforms,
and (SIGNED-BYTE 64) on 64 bit platforms.

EXPERIMENTAL: Interface subject to change."
  (expand-atomic-frob 'atomic-incf place diff))

(defmacro atomic-decf (place &optional (diff 1))
  #!+sb-doc
  "Atomically decrements PLACE by DIFF, and returns the value of PLACE before
the increment.

The decrementation is done using word-size modular arithmetic: on 32 bit
platforms ATOMIC-DECF of #x0 by one results in #xFFFFFFFF being stored in
PLACE.

PLACE must be an accessor form whose CAR is the name of a DEFSTRUCT accessor
whose declared type is (UNSIGNED-BYTE 32) on 32 bit platforms,
and (UNSIGNED-BYTE 64) on 64 bit platforms or an AREF of a (SIMPLE-ARRAY
SB-EXT:WORD (*) -- the type SB-EXT:WORD can be used for this purpose.

DIFF defaults to 1, and must be a (SIGNED-BYTE 32) on 32 bit platforms,
and (SIGNED-BYTE 64) on 64 bit platforms.

EXPERIMENTAL: Interface subject to change."
  (expand-atomic-frob 'atomic-decf place diff))

;; Interpreter stubs for ATOMIC-INCF.
#!+(or x86 x86-64 ppc)
(defun %array-atomic-incf/word (array index diff)
  (declare (type (simple-array word (*)) array)
           (fixnum index)
           (type sb!vm:signed-word diff))
  (%array-atomic-incf/word array index diff))

(defun spin-loop-hint ()
  #!+sb-doc
  "Hints the processor that the current thread is spin-looping."
  (spin-loop-hint))

(defun call-hooks (kind hooks &key (on-error :error))
  (dolist (hook hooks)
    (handler-case
        (funcall hook)
      (serious-condition (c)
        (if (eq :warn on-error)
            (warn "Problem running ~A hook ~S:~%  ~A" kind hook c)
            (with-simple-restart (continue "Skip this ~A hook." kind)
              (error "Problem running ~A hook ~S:~%  ~A" kind hook c)))))))

;;;; DEFGLOBAL

(defmacro-mundanely defglobal (name value &optional (doc nil docp))
  #!+sb-doc
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME both at compile- and load-time, but only if NAME is not
already bound.

Global variables share their values between all threads, and cannot be
locally bound, declared special, defined as constants, and neither bound
nor defined as symbol macros.

See also the declarations SB-EXT:GLOBAL and SB-EXT:ALWAYS-BOUND."
  `(progn
     (eval-when (:compile-toplevel)
       (let ((boundp (boundp ',name)))
         (%compiler-defglobal ',name (unless boundp ,value) boundp)))
     (eval-when (:load-toplevel :execute)
       (let ((boundp (boundp ',name)))
         (%defglobal ',name (unless boundp ,value) boundp ',doc ,docp
                     (sb!c:source-location))))))

(defun %compiler-defglobal (name value boundp)
  (sb!xc:proclaim `(global ,name))
  (unless boundp
    #-sb-xc-host
    (set-symbol-global-value name value)
    #+sb-xc-host
    (set name value))
  (sb!xc:proclaim `(always-bound ,name)))

(defun %defglobal (name value boundp doc docp source-location)
  (%compiler-defglobal name value boundp)
  (when docp
    (setf (fdocumentation name 'variable) doc))
  (sb!c:with-source-location (source-location)
    (setf (info :source-location :variable name) source-location))
  name)

;;;; WAIT-FOR -- waiting on arbitrary conditions

(defun %%wait-for (test stop-sec stop-usec)
  (declare (function test))
  (labels ((try ()
             (declare (optimize (safety 0)))
             (awhen (funcall test)
               (return-from %%wait-for it)))
           (tick (sec usec)
             (declare (fixnum sec usec))
             ;; TICK is microseconds
             (+ usec (* 1000000 sec)))
           (get-tick ()
             (multiple-value-call #'tick
               (decode-internal-time (get-internal-real-time)))))
    (let* ((timeout-tick (when stop-sec (tick stop-sec stop-usec)))
           (start (get-tick))
           ;; Rough estimate of how long a single attempt takes.
           (try-ticks (progn
                        (try) (try) (try)
                        (max 1 (truncate (- (get-tick) start) 3)))))
      ;; Scale sleeping between attempts:
      ;;
      ;; Start by sleeping for as many ticks as an average attempt
      ;; takes, then doubling for each attempt.
      ;;
      ;; Max out at 0.1 seconds, or the 2 x time of a single try,
      ;; whichever is longer -- with a hard cap of 10 seconds.
      ;;
      ;; FIXME: Maybe the API should have a :MAX-SLEEP argument?
      (loop with max-ticks = (max 100000 (min (* 2 try-ticks)
                                              (expt 10 7)))
            for scale of-type fixnum = 1
            then (let ((x (logand most-positive-fixnum (* 2 scale))))
                   (if (> scale x)
                       most-positive-fixnum
                       x))
            do (try)
               (let* ((now (get-tick))
                      (sleep-ticks (min (* try-ticks scale) max-ticks))
                      (sleep
                        (if timeout-tick
                            ;; If sleep would take us past the
                            ;; timeout, shorten it so it's just
                            ;; right.
                            (if (>= (+ now sleep-ticks) timeout-tick)
                                (- timeout-tick now)
                                sleep-ticks)
                            sleep-ticks)))
                 (declare (fixnum sleep))
                 (cond ((plusp sleep)
                        ;; microseconds to seconds and nanoseconds
                        (multiple-value-bind (sec nsec)
                            (truncate (* 1000 sleep) (expt 10 9))
                          (with-interrupts
                            (sb!unix:nanosleep sec nsec))))
                       (t
                        (return-from %%wait-for nil))))))))

(defun %wait-for (test timeout)
  (declare (function test))
  (tagbody
   :restart
     (multiple-value-bind (to-sec to-usec stop-sec stop-usec deadlinep)
         (decode-timeout timeout)
       (declare (ignore to-sec to-usec))
       (return-from %wait-for
         (or (%%wait-for test stop-sec stop-usec)
             (when deadlinep
               (signal-deadline)
               (go :restart)))))))

(defmacro wait-for (test-form &key timeout)
  #!+sb-doc
  "Wait until TEST-FORM evaluates to true, then return its primary value.
If TIMEOUT is provided, waits at most approximately TIMEOUT seconds before
returning NIL.

If WITH-DEADLINE has been used to provide a global deadline, signals a
DEADLINE-TIMEOUT if TEST-FORM doesn't evaluate to true before the
deadline.

Experimental: subject to change without prior notice."
  `(dx-flet ((wait-for-test () (progn ,test-form)))
     (%wait-for #'wait-for-test ,timeout)))

(defmacro with-progressive-timeout ((name &key seconds)
                                    &body body)
  #!+sb-doc
  "Binds NAME as a local function for BODY. Each time #'NAME is called, it
returns SECONDS minus the time that has elapsed since BODY was entered, or
zero if more time than SECONDS has elapsed. If SECONDS is NIL, #'NAME
returns NIL each time."
  (with-unique-names (deadline time-left sec)
    `(let* ((,sec ,seconds)
            (,deadline
              (when ,sec
                (+ (get-internal-real-time)
                   (round (* ,seconds internal-time-units-per-second))))))
       (flet ((,name ()
                (when ,deadline
                  (let ((,time-left (- ,deadline (get-internal-real-time))))
                    (if (plusp ,time-left)
                        (* (coerce ,time-left 'single-float)
                           ,(/ 1.0 internal-time-units-per-second))
                        0)))))
         ,@body))))

(defmacro atomic-update (place update-fn &rest arguments &environment env)
  #!+sb-doc
  "Updates PLACE atomically to the value returned by calling function
designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.

PLACE may be read and UPDATE-FN evaluated and called multiple times before the
update succeeds: atomicity in this context means that value of place did not
change between the time it was read, and the time it was replaced with the
computed value.

PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP.

Examples:

  ;;; Conses T to the head of FOO-LIST.
  (defstruct foo list)
  (defvar *foo* (make-foo))
  (atomic-update (foo-list *foo*) #'cons t)

  (let ((x (cons :count 0)))
     (mapc #'sb-thread:join-thread
           (loop repeat 1000
                 collect (sb-thread:make-thread
                          (lambda ()
                            (loop repeat 1000
                                  do (atomic-update (cdr x) #'1+)
                                     (sleep 0.00001))))))
     ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
     ;; atomic update with (INCF (CDR X)) above, the result becomes
     ;; unpredictable.
     x)
"
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,new)))))
