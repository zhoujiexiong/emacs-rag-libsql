;;; emacs-rag-index-async --- 同步向量数据库 -*- lexical-binding: t -*-

(defvar my/rag-queue nil)
(defvar my/rag-is-running nil)

(defun my/rag-process-next ()
  "消费者：通过进程属性传递上下文，彻底解决 void-variable 错误。"
  (when (and (not my/rag-is-running) my/rag-queue)
    (setq my/rag-is-running t)
    (let* ((file-path (pop my/rag-queue))
           (api-url (concat (emacs-rag--server-url) "/index"))
           (payload (json-encode `((path . ,file-path))))
           ;; 创建进程
           (proc (make-process
                  :name "rag-curl-worker"
                  :buffer "*rag-curl-log*"
                  :command (list "curl" "-s" "-X" "POST" 
                                 "-H" "Content-Type: application/json"
                                 "-d" payload
                                 api-url)
                  :sentinel (lambda (proc event)
                              ;; 【关键修复】：从 proc 对象中提取存入的上下文
                              (let ((inner-path (process-get proc :file-path)))
                                (setq my/rag-is-running nil)
                                (my/rag-process-next)
                                
                                (when (string-match-p "finished" event)
                                  (message "RAG: 成功索引 %s" 
                                           (file-name-nondirectory inner-path))))))))
      
      ;; 【关键注入】：类似 Go 的 context.WithValue
      (process-put proc :file-path file-path))))

(defun my/advice-rag-async-trigger (orig-fun &rest args)
  "生产者 Advice。"
  (let ((file-path (buffer-file-name)))
    (when (and emacs-rag-auto-index-on-save
               file-path
               (emacs-rag--eligible-file-p file-path))
      (unless (member file-path my/rag-queue)
        (setq my/rag-queue (append my/rag-queue (list file-path))))
      (run-with-idle-timer 0.5 nil #'my/rag-process-next))))

(advice-add 'emacs-rag--maybe-index-on-save :around #'my/advice-rag-async-trigger)

(provide 'emacs-rag-index-async)
