;;;; Tests for async signal safety.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(use-package :test-util)

(with-test (:name (:async-unwind :specials))
  (let ((*x0* nil) (*x1* nil) (*x2* nil) (*x3* nil) (*x4* nil))
    (declare (special *x0* *x1* *x2* *x3* *x4*))
    (loop repeat 10 do
          (loop repeat 10 do
                (catch 'again
                  (sb-ext:schedule-timer (sb-ext:make-timer
                                          (lambda ()
                                            (throw 'again nil)))
                                         (random 0.1))
                  (loop
                   (let ((*x0* (cons nil nil)) (*x1* (cons nil nil))
                         (*x2* (cons nil nil)) (*x3* (cons nil nil))
                         (*x4* (cons nil nil)))
                     (declare (special *x0* *x1* *x2* *x3* *x4*)))))
                (when (not (and (null *x0*) (null *x1*) (null *x2*) (null *x3*)
                                (null *x4*)))
                  (format t "~S ~S ~S ~S ~S~%" *x0* *x1* *x2* *x3* *x4*)
                  (assert nil)))
          (princ '*)
          (force-output))
    (terpri)))

(require :sb-posix)

(with-test (:name (:signal :errno))
  (let* (saved-errno
         (returning nil)
         (timer (make-timer (lambda ()
                              (sb-unix:unix-open "~!@#$%^&*[]()/\\" 0 0)
                              (assert (= sb-unix:enoent
                                         (sb-unix::get-errno)))
                              (setq returning t)))))
    (schedule-timer timer 0.2)
    ;; Fail and set errno.
    (sb-unix:nanosleep -1 -1)
    (setq saved-errno (sb-unix::get-errno))
    (assert (= saved-errno sb-posix:einval))
    ;; Wait, but not with sleep because that will be interrupted and
    ;; we get EINTR.
    (loop until returning)
    (loop repeat 1000000000)
    (assert (= saved-errno (sb-unix::get-errno)))))

(with-test (:name :handle-interactive-interrupt)
  (assert (eq :condition
              (handler-case
                  (sb-thread::kill-safely
                   (sb-thread::thread-os-thread sb-thread::*current-thread*)
                   sb-unix:sigint)
                (sb-sys:interactive-interrupt ()
                  :condition)))))

(with-test (:name :bug-640516)
  ;; On Darwin interrupting a SLEEP so that it took longer than
  ;; the requested amount caused it to hang.
  (assert
   (handler-case
       (sb-ext:with-timeout 10
         (let (to)
           (handler-bind ((sb-ext:timeout (lambda (c)
                                            (unless to
                                              (setf to t)
                                              (sleep 2)
                                              (continue c)))))
             (sb-ext:with-timeout 0.1 (sleep 1) t))))
     (sb-ext:timeout ()
       nil))))

(with-test (:name :nested-sleeps)
  (let* ((*level* -1)
         (timer-count 20) ; tuning parameters
         (min-sleep 0.0001)
         (max-sleep 0.001)
         (target 10)
         (nesting-table (make-array 0 :initial-element 0 :adjustable t :fill-pointer 0))
         (firing-table (make-array timer-count :initial-element 0))
         (count 0)
         (make-timekeeper
           (lambda (id)
             (lambda ()
               (let* ((level (1+ *level*))
                      (*level* level))
                 (declare (special *level*))
                 (if (< level (length nesting-table))
                     (incf (aref nesting-table level))
                     (vector-push-extend 1 nesting-table))
                 (incf count)
                 (incf (aref firing-table id))
                 (let ((sec (+ min-sleep (random max-sleep))))
                   (sb-sys:with-interrupts
                     (sleep sec)))))))
         (timers (loop for i from 0 below timer-count
                       collect
                          (sb-ext:make-timer (funcall make-timekeeper i)
                                             :name (format nil "Sleep Timer ~S" i)))))
    (declare (special *level*))
    (sb-sys:without-interrupts
      (dolist (timer timers)
        (sb-ext:schedule-timer timer (+ min-sleep (random max-sleep))
                               :repeat-interval (+ min-sleep (random max-sleep)))))
    (sleep 0.1)
    (sb-sys:without-interrupts
      (loop for nesting = (length nesting-table)
            for mincount = (reduce #'min firing-table)
            until (and (= nesting timer-count) (<= target mincount))
            do (format t "Spinning: Count ~D,  Mincount ~D, Nesting ~D~%"
                       count mincount nesting)
               (sb-sys:with-local-interrupts
                 (sleep 0.1))))
    (sb-sys:without-interrupts
      (dolist (timer timers)
        (sb-ext:unschedule-timer timer)))
    (values firing-table nesting-table)))
