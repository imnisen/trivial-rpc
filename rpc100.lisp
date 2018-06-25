(ql:quickload '(:usocket :flexi-streams))


(defvar data-max-length 65507)

(defclass transport-udp-server ()
  ( ;; (host :accessor :host
   ;;       :initform (error "No host given"))
   ;; (port :accessor :port
   ;;       :initform (error "No port given"))
   ;; (socket :accessor :socket
   ;;         :initform nil)
   (router :accessor router
           :initform nil)))



(defun load-msg (str)
  (first (multiple-value-list (read-from-string str))))

(defun dump-msg (msg)
  (write-to-string msg
                   :array t :base 10 :case :downcase :circle t
                   :escape t :gensym t :length nil :level nil
                   :lines nil :pretty nil :radix nil :readably nil))




(defclass transport-udp-client ()
  ((host :accessor host
         :initarg :host
         :initform (error "No host given"))
   (port :accessor port
         :initarg :port
         :initform (error "No port given"))
   ;; (socket :accessor :socket
   ;;         :initform nil)
   ))








;;; server api
(defun make-server ()
  (make-instance 'transport-udp-server))

(defgeneric expose (server rpc-name lambda-fun)
  (:documentation ""))

(defmethod expose ((server transport-udp-server) rpc-name lambda-fun)
  (push (cons rpc-name lambda-fun) (router server)))

(defun server-listen (server &key port)
  (let ((socket (usocket:socket-connect nil nil
                                        :protocol :datagram
                                        :local-host "127.0.0.1"
                                        :local-port port)))
    (unwind-protect
         (loop :for (msg . addr)
               := (multiple-value-bind (return-buffer return-length remote-host remote-port)
                      (usocket:socket-receive socket nil data-max-length)
                    (list (load-msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))
                          remote-host remote-port))
               :do (progn

                     ;; send result back to client
                     (format t "Trying to send packet from server~&")
                     (let* ((packet (dump-msg
                                     (handler-case
                                         (apply (assoc (car msg) (router server) :test #'equalp) (cdr msg))
                                       (condition (c) c))))
                            (buffer  (flexi-streams:string-to-octets packet :external-format :utf-8)))
                       (usocket:socket-send socket buffer (length buffer)
                                            :host (first addr)
                                            :port (second addr))))
               )
      (usocket:socket-close socket))))






;;; client api

(defun make-client (host port)
  (make-instance 'transport-udp-client
                 :host host
                 :port port))

(defun call (client rpc-name arg-list)
  (let ((socket (usocket:socket-connect (host client) (port client)
                                        :protocol :datagram)))
    (unwind-protect
         (progn
           (let* ((packet (dump-msg (cons rpc-name arg-list)))
                  (buffer (flexi-streams:string-to-octets packet :external-format :utf-8)))
             (usocket:socket-send socket buffer (length buffer))

             ;; receive result from server
             (multiple-value-bind (return-buffer return-length)
                 (usocket:socket-receive socket nil data-max-length)
               (load-msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8)))))
      (usocket:socket-close socket))))
