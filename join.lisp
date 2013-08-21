;;; join.lisp --- join sequences on similar elements

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :join)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defun join (list-1 list-2 predicate
             &key (key #'car) (val #'cdr) (test #'eql) empty
               start end key-1 key-2 val-1 val-2 start-1 start-2 end-1 end-2)
  "Combine elements of LIST-1 and LIST-2 which have equal values for KEY.
LIST-1 and LIST-2 must be sorted.

Optional arguments:

Key indicates the fields of LIST-1 and LIST-2 used to join.

TEST is used to determine equality.

EMPTY may be set to a value used to replaces missing input fields.

KEY-1 indicates the field of LIST-1 used to join.

KEY-2 indicates the field of LIST-2 used to join.

START,END are bounding input designators of LIST-1 and LIST-2.

START-(1|2),END-(1|2) are bounding input designators of LIST-1 or LIST-2."
  (declare (optimize speed))
  (let ((ends (cons (or end-1 end (1- (length list-1)))
                    (or end-2 end (1- (length list-2)))))
        (inds (cons (1- (or start-1 start 0))
                    (1- (or start-2 start 0))))
        (keys (cons (or key-1 key) (or key-2 key)))
        (vals (cons (or val-1 val) (or val-2 val)))
        (lists (cons list-1 list-2))
        (ks (cons nil nil)) (vs (cons nil nil))
        result)
    (macrolet ((next (list)
                 (let ((it (gensym))
                       (fun (case list (1 'car) (2 'cdr))))
                   `(let ((,it (nth (incf (,fun inds)) (,fun lists))))
                      (if (< (,fun inds) (,fun ends))
                          (setf (,fun ks) (funcall (,fun keys) ,it)
                                (,fun vs) (funcall (,fun vals) ,it))
                          (setf (,fun ks) nil)))))
               (collect (side)
                 (let ((fun (case side (1 'car) (2 'cdr))))
                   `(let ((k  (,fun ks))
                          (v1 (,fun vs))
                          (v2 (mapcar (lambda (el)
                                        (declare (ignorable el))
                                        empty)
                                      (,fun vs))))
                      (push ,(case side
                                   (1 `(append (list k) v1 v2))
                                   (2 `(append (list k) v2 v1)))
                            result)))))
      (next 1) (next 2)
      (loop :while (and (car ks) (cdr ks)) :do
         (cond
           ;; equal key so add line from both
           ((funcall test (car ks) (cdr ks))
            (push (append (list (car ks)) (car vs) (cdr vs)) result)
            (next 1) (next 2))
           ;; left side is ahead
           ((funcall predicate (car ks) (cdr ks))
            (when (and empty (car vs)) (collect 1))
            (next 1))
           ;; right side is ahead
           (t
            (when (and empty (cdr vs)) (collect 2))
            (next 2))))
      (when empty ;; drain the remaining list
        (cond
          ((< (car inds) (car ends))
           (loop :while (car ks) :do (collect 1) (next 1)))
          ((< (cdr inds) (cdr ends))
           (loop :while (cdr ks) :do (collect 2) (next 2))))))
    (nreverse result)))
