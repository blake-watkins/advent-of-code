(in-package :aoc)

;;; Copy session cookie for AoC website to *SESSION-COOKIE* as a string "1F2C..."
(defparameter *session-cookie* nil)

(defun download-input (day &optional (year 2021))
  (let ((cookie  (if *session-cookie*
                     *session-cookie*
                     (restart-case
                         (error "Session cookie not set up in DOWNLOAD-INPUT.")
                       (use-value (value)
                         :interactive (lambda ()
                                        (list (progn (princ "Value: " *query-io*)
                                                     (read *query-io*))))
                         value)))))
    (let* ((url (format nil "https://adventofcode.com/~a/day/~a/input" year day))
           (cookie (make-instance 'drakma:cookie
			          :name "session"
			          :value cookie
                                  :domain ".adventofcode.com"))
           (cookie-jar (make-instance 'drakma:cookie-jar
                                      :cookies (list cookie)))
	   (response (multiple-value-list 
		      (drakma:http-request url :cookie-jar cookie-jar))))
      (if (= (second response) 200)
	  (first response)
	  nil))))
