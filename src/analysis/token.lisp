(in-package #:montezuma)

(defstruct (token
	     (:constructor make-token (image start end &key (increment 1) (type :word))))
  image
  start
  end
  (increment 1)
  (type :word))
;; 比较两个token
(defun token-compare (t1 t2)
  (let ((r (- (token-start t2) (token-start t1)))) ;; token开始位置比较
    (if (not (= r 0))
        r ;; 不为0,则返回距离差
        (let ((r (- (token-end t2) (token-end t1)))) ;; token结束位置比较
          (if (not (= r 0))
              r ;; 不为0,则返回距离差
              (string-compare (token-image t1) (token-image t2))))))) ;; 比较两个token的字串

(defun token= (t1 t2)
  (= 0 (token-compare t1 t2)))


(defgeneric term-text (token))

(defmethod term-text ((token token))
  (token-image token))
