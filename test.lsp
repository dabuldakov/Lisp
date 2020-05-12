(defun ff (D)
 (cond ((null D)          T  )
       ((symbolp (car D)) nil)
       ((> (car D) 0)     (ff (cdr D)))
       (t                 nil)
 )
)

(defun summm (x) 
 (cond ((numberp x) x)  
       ((atom    x) 0)
       (t           (+ (summm (car x)) (summm (cdr x))))
 )
)

(defun fl (d)
 (cond ((null d) nil)
       ((> (car d) 0) (cons (car d) (fl (cdr d))))
       (t (fl (cdr d)))
 )
)


(defun onlyminus2 (x) 
 (cond ((null x)       nil)
       ((listp (car x)) (cons (onlyminus2 (car x)) (onlyminus2 (cdr x))))
       ((< (car x) 0)  (cons (car x) (onlyminus2 (cdr x))))
       (t              (onlyminus2 (cdr x)))
 )
)

(defun sum_list (s)
 (cond ((null s) 0)
       (t       (+ (car s) (sum_list (cdr s))))
 )
)

(defun mf (l) (apply '+ (mapcar 'eval l)))

