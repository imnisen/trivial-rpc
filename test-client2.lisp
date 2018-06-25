;; (ql:quickload :flexi-streams)

(defvar data-max-length 65507)
(defun server ()
  (let* ((socket (usocket:socket-connect nil nil
                                         :protocol :datagram
                                         :local-host "127.0.0.1"
                                         :local-port 9000)))
    (unwind-protect
         (multiple-value-bind (return-buffer return-length remote-host remote-port)
             (usocket:socket-receive socket nil data-max-length)
           ;; (format t "~A~%" return-buffer)
           ;; (format t "~A~%" return-length)
           (format t "~A~%" remote-host)
           (format t "~A~%" remote-port)
           (format t "~A~%" (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))
           ;; (usocket:socket-send socket (reverse buffer) size
           ;;                      :port receive-port
           ;;                      :host client)
           )
      (usocket:socket-close socket)))
  )


(defun client (string)
  (let ((socket (usocket:socket-connect "127.0.0.1" 9000
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8)))
        (buffer (flexi-streams:string-to-octets string :external-format :utf-8)))
    (unwind-protect
         (progn
           (format t "Sending data~%")
           (usocket:socket-send socket buffer (length buffer))
           ;; (format t "Receiving data~%")
           ;; (usocket:socket-receive socket buffer 8)
           ;; (format t "~A~%" buffer)
           )
      (usocket:socket-close socket))))
