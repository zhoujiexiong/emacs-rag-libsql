;;; emacs-rag-search.el --- Search interface for Emacs RAG -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides search commands and result navigation
;; for the RAG system.

;;; Code:

(require 'emacs-rag-server)

;;; Customization

(defcustom emacs-rag-search-limit 5
  "Default number of search results to return."
  :type 'integer
  :group 'emacs-rag)

(defcustom emacs-rag-search-enable-rerank t
  "Enable reranking in search by default."
  :type 'boolean
  :group 'emacs-rag)

(defcustom emacs-rag-result-display-width 80
  "Width for wrapping result content in display."
  :type 'integer
  :group 'emacs-rag)

;;; Helper Functions

(defun emacs-rag--read-query (prompt)
  "Read a search query from the user with PROMPT."
  (read-string prompt))

(defun emacs-rag--wrap-text (text width)
  "Wrap TEXT to WIDTH characters."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun emacs-rag--format-result (result index)
  "Format RESULT for display at INDEX."
  (let* ((path (alist-get 'source_path result))
         (basename (file-name-nondirectory path))
         (chunk (alist-get 'chunk_index result))
         (score (alist-get 'score result))
         (content (alist-get 'content result))
         (header (format "%2d. %-20s chunk %-3d score %.3f"
                        index basename chunk score))
         ;; 关键修改：将 content 中的所有换行符替换为空格，不要在 minibuffer 里做 wrap
         (clean-content (replace-regexp-in-string "\n+" " " content)))
    (concat header " | " clean-content)))

(defun orig/emacs-rag--format-result (result index)
  "Format RESULT for display at INDEX.
Returns a multiline string with header and wrapped content."
  (let* ((path (alist-get 'source_path result))
         (basename (file-name-nondirectory path))
         (chunk (alist-get 'chunk_index result))
         (score (alist-get 'score result))
         (content (alist-get 'content result))
         (header (format "%2d. %-20s chunk %-3d score %.3f"
                        index basename chunk score))
         (wrapped-content (emacs-rag--wrap-text
                          content
                          (- emacs-rag-result-display-width 4))))
    ;; (concat header "\n    "
    ;;         (replace-regexp-in-string "\n" "\n    "
    ;;                                   (replace-regexp-in-string "\n\\{2,\\}" "\n" wrapped-content)))))
    (concat header "\n    "
            (replace-regexp-in-string "\n" "\n    " wrapped-content))))

(defun emacs-rag--open-result (result)
  "Open the file and navigate to the location of RESULT."
  (let* ((path (alist-get 'source_path result))
         (line-number (alist-get 'line_number result)))
    (find-file path)
    (goto-char (point-min))
    (forward-line (1- line-number))
    (recenter)))

;;; Search Commands

(defun emacs-rag-search-vector (query &optional limit rerank)
  "Perform semantic vector search for QUERY.
LIMIT is the maximum number of results (defaults to `emacs-rag-search-limit').
RERANK enables reranking (defaults to `emacs-rag-search-enable-rerank').

With prefix argument, prompt for limit.
If region is active, use it as the default query."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (emacs-rag--read-query "Vector search: "))
                     (when current-prefix-arg
                       (read-number "Number of results: "
                                   emacs-rag-search-limit))
                     emacs-rag-search-enable-rerank))
  (let* ((limit (or limit emacs-rag-search-limit))
         (rerank (if rerank "true" "false"))
         (response (emacs-rag--request "GET" "/search/vector" nil
                                      `((query . ,query)
                                        (limit . ,limit)
                                        (rerank . ,rerank))))
         (results (alist-get 'results response)))
    (emacs-rag--display-results results "Vector" query)))

(defun emacs-rag-search-text (query &optional limit)
  "Perform full-text search for QUERY using FTS5.
LIMIT is the maximum number of results (defaults to `emacs-rag-search-limit').

With prefix argument, prompt for limit.
If region is active, use it as the default query."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (emacs-rag--read-query "Full-text search: "))
                     (when current-prefix-arg
                       (read-number "Number of results: "
                                   emacs-rag-search-limit))))
  (let* ((limit (or limit emacs-rag-search-limit))
         (response (emacs-rag--request "GET" "/search/text" nil
                                      `((query . ,query)
                                        (limit . ,limit))))
         (results (alist-get 'results response)))
    (emacs-rag--display-results results "Full-text" query)))

(defun emacs-rag-search-hybrid (query &optional limit vector-weight rerank)
  "Perform hybrid search for QUERY combining vector and full-text search.
LIMIT is the maximum number of results (defaults to `emacs-rag-search-limit').
VECTOR-WEIGHT is the weight for vector scores (0-1, defaults to 0.5).
RERANK enables reranking (defaults to `emacs-rag-search-enable-rerank').

With prefix argument, prompt for limit and vector weight.
If region is active, use it as the default query."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (emacs-rag--read-query "Hybrid search: "))
                     (when current-prefix-arg
                       (read-number "Number of results: "
                                   emacs-rag-search-limit))
                     (when current-prefix-arg
                       (read-number "Vector weight (0-1): " 0.5))
                     emacs-rag-search-enable-rerank))
  (let* ((limit (or limit emacs-rag-search-limit))
         (vector-weight (or vector-weight 0.5))
         (rerank (if rerank "true" "false"))
         (response (emacs-rag--request "GET" "/search/hybrid" nil
                                      `((query . ,query)
                                        (limit . ,limit)
                                        (vector_weight . ,vector-weight)
                                        (rerank . ,rerank))))
         (results (alist-get 'results response)))
    (emacs-rag--display-results results "Hybrid" query)))

;;; Result Display

(defun emacs-rag--display-results (results mode query)
  "Display RESULTS from MODE search for QUERY.
Uses completion interface to select and navigate to results."
  (if (null results)
      (message "No %s results for: %s" mode query)
    (let* ((candidates
            (cl-loop for result in results
                     for index from 1
                     collect (cons (emacs-rag--format-result result index)
                                  result)))
           (completion-fn (if (fboundp 'ivy-read)
                             'emacs-rag--display-with-ivy
                           'emacs-rag--display-with-completing-read)))
      (funcall completion-fn candidates mode query))))

(defun emacs-rag--display-with-completing-read (candidates mode query)
  "Display CANDIDATES using `completing-read'.
MODE and QUERY are used in the prompt."
  (let* ((choice (completing-read
                 (format "%s search for '%s': " mode query)
                 (mapcar #'car candidates)
                 nil t))
         (result (cdr (assoc choice candidates))))
    (when result
      (emacs-rag--open-result result))))

(defun emacs-rag--display-with-ivy (candidates mode query)
  "Display CANDIDATES using Ivy if available.
MODE and QUERY are used in the prompt."
  (ivy-read (format "%s search for '%s': " mode query)
            (mapcar #'car candidates)
            :action (lambda (choice)
                     (let ((result (cdr (assoc choice candidates))))
                       (when result
                         (emacs-rag--open-result result))))))

;;; Statistics

(defun emacs-rag-stats ()
  "Display database statistics."
  (interactive)
  (let* ((stats (emacs-rag--request "GET" "/stats"))
         (chunks (alist-get 'total_chunks stats))
         (files (alist-get 'total_unique_files stats)))
    (message "Index contains %s chunks across %s files" chunks files)))

;;; Database Management

(defun emacs-rag-delete-database ()
  "Delete the entire RAG database.
This operation cannot be undone!"
  (interactive)
  (when (yes-or-no-p "Delete entire database? This cannot be undone! ")
    (when (emacs-rag-server-running-p)
      (emacs-rag-stop-server)
      (sit-for 1))
    (let ((db-dir (expand-file-name emacs-rag-db-path)))
      (when (file-directory-p db-dir)
        (delete-directory db-dir t)
        (message "Database deleted: %s" db-dir)))))

(defun emacs-rag-rebuild-database ()
  "Delete and rebuild the RAG database.
This will stop the server, delete the database, restart the server
to recreate the schema, then optionally reindex open buffers."
  (interactive)
  (when (yes-or-no-p "Rebuild database? This will delete all indexed data! ")
    (let ((was-running (emacs-rag-server-running-p)))
      ;; Stop server if running
      (when was-running
        (message "Stopping server...")
        (emacs-rag-stop-server)
        (sit-for 1))

      ;; Delete database
      (let ((db-dir (expand-file-name emacs-rag-db-path)))
        (when (file-directory-p db-dir)
          (delete-directory db-dir t)
          (message "Database deleted: %s" db-dir)))

      ;; Restart server to recreate schema
      (message "Restarting server to recreate database schema...")
      (emacs-rag-start-server)
      (sit-for 2)

      ;; Optionally reindex open buffers
      (when (and (emacs-rag-server-running-p)
                 (yes-or-no-p "Reindex all open buffers? "))
        (emacs-rag-reindex-all-open-buffers))

      (message "Database rebuild complete"))))

(defun emacs-rag-rebuild-fts-index ()
  "Rebuild the FTS5 full-text search index.
This rebuilds the FTS5 index from existing documents without
requiring a full database rebuild. Useful when the FTS5 index
gets out of sync with the documents table."
  (interactive)
  (if (not (emacs-rag-server-running-p))
      (user-error "Server is not running. Start it first with `emacs-rag-start-server'")
    (when (yes-or-no-p "Rebuild FTS5 index? ")
      (message "Rebuilding FTS5 index...")
      (let* ((response (emacs-rag--request "POST" "/rebuild-fts"))
             (count (alist-get 'documents_reindexed response))
             (msg (alist-get 'message response)))
        (message "%s (%d documents)" msg count)))))

;;; File Navigation

(defun emacs-rag-open-indexed-file ()
  "Select and open a file from the database using Ivy/completing-read."
  (interactive)
  (if (not (emacs-rag-server-running-p))
      (user-error "Server is not running. Start it first with `emacs-rag-start-server'")
    (let* ((response (emacs-rag--request "GET" "/files"))
           (files (alist-get 'files response))
           (count (alist-get 'count response)))
      (if (zerop count)
          (message "No files indexed yet")
        (let ((choice (if (fboundp 'ivy-read)
                          (ivy-read (format "Open indexed file (%d total): " count)
                                   files
                                   :action (lambda (x) x))
                        (completing-read
                         (format "Open indexed file (%d total): " count)
                         files
                         nil t))))
          (when choice
            (find-file choice)))))))

(defun emacs-rag-jump-to-org-heading ()
  "Jump to an org heading from indexed files using fast database lookup."
  (interactive)
  (if (not (emacs-rag-server-running-p))
      (user-error "Server is not running. Start it first with `emacs-rag-start-server'")
    (let* ((response (emacs-rag--request "GET" "/org-headings"))
           (headings (alist-get 'headings response))
           (count (alist-get 'count response)))
      (if (zerop count)
          (message "No org headings found. Index some .org files first.")
        (let* ((candidates
                (mapcar (lambda (heading)
                          (let* ((text (alist-get 'heading_text heading))
                                 (tags (alist-get 'tags heading))
                                 (path (alist-get 'source_path heading))
                                 (line (alist-get 'line_number heading))
                                 (display (format "%-40s | %-20s | %s"
                                                (truncate-string-to-width text 40 nil nil "...")
                                                (or tags "")
                                                path)))
                            (cons display heading)))
                        headings))
               (selected (if (fboundp 'ivy-read)
                            (ivy-read (format "Jump to org heading (%d total): " count)
                                     (mapcar #'car candidates))
                          (completing-read (format "Jump to org heading (%d total): " count)
                                          (mapcar #'car candidates)
                                          nil t)))
               (choice (cdr (assoc selected candidates))))
          (when choice
            (let ((file (alist-get 'source_path choice))
                  (line (alist-get 'line_number choice)))
              (find-file file)
              (goto-char (point-min))
              (forward-line (1- line))
              (recenter))))))))

(defun emacs-rag-search-org-headings (query &optional limit)
  "Perform semantic search on org headings using QUERY.
LIMIT is the maximum number of results (defaults to 20).

With prefix argument, prompt for limit.
If region is active, use it as the default query.

When using Ivy, the search is dynamic - results update as you type."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       nil)
                     (when current-prefix-arg
                       (read-number "Number of results: " 20))))
  (if (not (emacs-rag-server-running-p))
      (user-error "Server is not running. Start it first with `emacs-rag-start-server'")
    (let ((limit (or limit 20)))
      (if (fboundp 'ivy-read)
          (emacs-rag--search-org-headings-ivy query limit)
        (emacs-rag--search-org-headings-static query limit)))))

(defun emacs-rag--search-org-headings-ivy (initial-query limit)
  "Dynamic Ivy interface for searching org headings.
INITIAL-QUERY is the starting query (can be nil).
LIMIT is the maximum number of results."
  (let ((candidates-cache nil))
    (ivy-read "Search org headings: "
              (lambda (input)
                (if (string-empty-p input)
                    (setq candidates-cache nil)
                  (let* ((response (emacs-rag--request "GET" "/search/org-headings" nil
                                                      `((query . ,input)
                                                        (limit . ,limit))))
                         (results (alist-get 'results response)))
                    (setq candidates-cache
                          (mapcar (lambda (result)
                                    (let* ((text (alist-get 'heading_text result))
                                           (tags (alist-get 'tags result))
                                           (path (alist-get 'source_path result))
                                           (line (alist-get 'line_number result))
                                           (score (alist-get 'score result))
                                           (basename (file-name-nondirectory path))
                                           (display (format "%.3f  %-40s | %-15s | %s"
                                                          score
                                                          (truncate-string-to-width text 40 nil nil "...")
                                                          (or tags "")
                                                          basename)))
                                      (cons display result)))
                                  results))))
                candidates-cache)
              :dynamic-collection t
              :initial-input initial-query
              :action (lambda (selection)
                       (let* ((choice (cdr (assoc selection candidates-cache))))
                         (when choice
                           (let ((file (alist-get 'source_path choice))
                                 (line (alist-get 'line_number choice)))
                             (find-file file)
                             (goto-char (point-min))
                             (forward-line (1- line))
                             (recenter))))))))

(defun emacs-rag--search-org-headings-static (query limit)
  "Static search interface for org headings (fallback when Ivy unavailable).
QUERY is the search query.
LIMIT is the maximum number of results."
  (let ((query (or query (emacs-rag--read-query "Search org headings: "))))
    (let* ((response (emacs-rag--request "GET" "/search/org-headings" nil
                                        `((query . ,query)
                                          (limit . ,limit))))
           (results (alist-get 'results response))
           (count (alist-get 'count response)))
      (if (zerop count)
          (message "No matching org headings found for: %s" query)
        (let* ((candidates
                (mapcar (lambda (result)
                          (let* ((text (alist-get 'heading_text result))
                                 (tags (alist-get 'tags result))
                                 (path (alist-get 'source_path result))
                                 (line (alist-get 'line_number result))
                                 (score (alist-get 'score result))
                                 (basename (file-name-nondirectory path))
                                 (display (format "%.3f  %-40s | %-15s | %s"
                                                score
                                                (truncate-string-to-width text 40 nil nil "...")
                                                (or tags "")
                                                basename)))
                            (cons display result)))
                        results))
               (selected (completing-read (format "Org headings for '%s' (%d results): " query count)
                                         (mapcar #'car candidates)
                                         nil t))
               (choice (cdr (assoc selected candidates))))
          (when choice
            (let ((file (alist-get 'source_path choice))
                  (line (alist-get 'line_number choice)))
              (find-file file)
              (goto-char (point-min))
              (forward-line (1- line))
              (recenter))))))))

;;; Search at Point

(defun emacs-rag-search-at-point ()
  "Search for the word or region at point."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'word t))))
    (if query
        (emacs-rag-search-vector query)
      (user-error "No word or region at point"))))

;;; Search History

(defvar emacs-rag-search-history nil
  "History of search queries.")

(defun emacs-rag-search-vector-with-history (query &optional limit)
  "Perform vector search with QUERY, using history.
LIMIT is the maximum number of results."
  (interactive (list (read-string "Vector search: "
                                  nil 'emacs-rag-search-history)
                     (when current-prefix-arg
                       (read-number "Number of results: "
                                   emacs-rag-search-limit))))
  (emacs-rag-search-vector query limit))

;;; emacs-rag: 单行 minibuffer display（把所有换行替为空格） + 原始多行 preview（最小改动）

(require 'cl-lib)
(require 'ivy)

(defun emacs-rag--show-preview-from-full (full)
  "在 *emacs-rag-preview* 中显示 FULL（原始多行格式），并设置为只读预览。"
  (let ((buf (get-buffer-create "*emacs-rag-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or full ""))
        (goto-char (point-min))
        (special-mode)))
    ;; 显示但不打断 minibuffer
    (display-buffer buf '(display-buffer-pop-up-window . ((inhibit-same-window . t))))))

(defun emacs-rag--display-results (results mode query)
  "Display RESULTS from MODE search for QUERY.

在 minibuffer 中为每个候选显示单行（把原始 formatted 的所有换行与多余空格替为单个空格），
以便补全过程中能看到整行信息（不会被多行撑满）。
同时保留原始 formatted 字符串用于 preview（*emacs-rag-preview*），按原始格式显示。"
  (let* ((pairs
          ;; pairs 是 (single-display . (result full-formatted)) 的 alist
          (cl-loop for result in results
                   for index from 1
                   collect
                   (let* ((full (orig/emacs-rag--format-result result index)) ; 原始 multi-line 字符串
                          ;; single: 把所有换行和连续空白替为单个空格，得到单行 display
                          (single (replace-regexp-in-string "[ \t\n]+" " " (string-trim (or full "")))))
                     (cons single (list result full))))))
    (if (null pairs)
        (message "No %s results for %s" mode query)
      (let* ((display-strings (mapcar #'car pairs))
             (count (length display-strings))
             ;; 因为每项为单行字符串，所以可以按项数控制 ivy-height（或换为你原有的 height 逻辑）
             (computed-height (min count (or emacs-rag-search-limit ivy-height))))
        ;;(let ((ivy-height (max 1 computed-height)))
        (let ((ivy-height ivy-height))
          (ivy-read (format "%s search for %s: " mode query)
                    display-strings
                    :action (lambda (choice)
                              ;; 找回对应的 result 并 open（cdr 是 (result full)）
                              (let* ((entry (assoc choice pairs))
                                     (payload (and entry (cdr entry)))
                                     (result (and payload (nth 0 payload))))
                                (if result
                                    (emacs-rag--open-result result)
                                  (message "emacs-rag: cannot find result for choice: %s" choice))))
                    :update-fn (lambda (&optional _str)
                                 ;; update-fn 可能被以 0 或 1 个参数调用，签名用 &optional
                                 (when (and (boundp 'ivy-last) ivy-last)
                                   (let ((cur (ivy-state-current ivy-last)))
                                     (when cur
                                       (let* ((entry (assoc cur pairs))
                                              (payload (and entry (cdr entry)))
                                              (full (and payload (nth 1 payload))))
                                         (when full
                                           (emacs-rag--show-preview-from-full full)))))))
                    :caller 'emacs-rag-search))))))

;; ;;; emacs-rag: 单行 minibuffer display（把所有换行替为空格） + 原始多行 preview（最小改动）

;; (require 'cl-lib)
;; (require 'ivy)

;; (defun emacs-rag--show-preview-from-full (full)
;;   "在 *emacs-rag-preview* 中显示 FULL（原始多行格式），并设置为只读预览。"
;;   (let ((buf (get-buffer-create "*emacs-rag-preview*")))
;;     (with-current-buffer buf
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert (or full ""))
;;         (goto-char (point-min))
;;         (special-mode)))
;;     ;; 显示但不打断 minibuffer
;;     (display-buffer buf '(display-buffer-pop-up-window . ((inhibit-same-window . t))))))


;; (defun emacs-rag--display-results (results mode query)
;;   "Display RESULTS from MODE search for QUERY.

;; 在 minibuffer 中为每个候选显示单行（把原始 formatted 的所有换行与多余空格替为单个空格），
;; 以便补全过程中能看到整行信息（不会被多行撑满）。
;; 同时保留原始 formatted 字符串用于 preview（*emacs-rag-preview*），按原始格式显示。"
;;   (let* ((pairs
;;           ;; pairs 是 (single-display . (result full-formatted)) 的 alist
;;           (cl-loop for result in results
;;                    for index from 1
;;                    collect
;;                    (let* ((full (emacs-rag--format-result result index)) ; 原始 multi-line 字符串
;;                           ;; single: 把所有换行和连续空白替为单个空格，得到单行 display
;;                           (single (replace-regexp-in-string "[ \t\n]+" " " (string-trim (or full "")))))
;;                      (cons single (list result full))))))
;;     (if (null pairs)
;;         (message "No %s results for %s" mode query)
;;       (let* ((display-strings (mapcar #'car pairs))
;;              (count (length display-strings))
;;              ;; 因为每项为单行字符串，所以可以按项数控制 ivy-height（或换为你原有的 height 逻辑）
;;              (computed-height (min count (or emacs-rag-search-limit ivy-height))))
;;         ;;(let ((ivy-height (max 1 computed-height)))
;;         (let ((ivy-height ivy-height))
;;           (ivy-read (format "%s search for %s: " mode query)
;;                     display-strings
;;                     :action (lambda (choice)
;;                               ;; 找回对应的 result 并 open（cdr 是 (result full)）
;;                               (let* ((entry (assoc choice pairs))
;;                                      (payload (and entry (cdr entry)))
;;                                      (result (and payload (nth 0 payload))))
;;                                 (if result
;;                                     (emacs-rag--open-result result)
;;                                   (message "emacs-rag: cannot find result for choice: %s" choice))))
;;                     :update-fn (lambda (&optional _str)
;;                                  ;; update-fn 可能被以 0 或 1 个参数调用，签名用 &optional
;;                                  (when (and (boundp 'ivy-last) ivy-last)
;;                                    (let ((cur (ivy-state-current ivy-last)))
;;                                      (when cur
;;                                        (let* ((entry (assoc cur pairs))
;;                                               (payload (and entry (cdr entry)))
;;                                               (full (and payload (nth 1 payload))))
;;                                          (when full
;;                                            (emacs-rag--show-preview-from-full full)))))))
;;                     :caller 'emacs-rag-search))))))

;;;;;;;;;;
;; 方案二
;;;;;;;;;;

;;; emacs-rag: header-id + display-property 版本 —— 多行在 minibuffer 可视化，
;;; 同时 ivy-occur 中保留可解析的 id 以保证 RET 跳转正确。

;; (require 'cl-lib)
;; (require 'ivy)

;; (defun emacs-rag--make-id (index)
;;   "基于 INDEX 生成短 id 字符串，例如 \"0001\"。"
;;   (format "%04d" index))

;; (defun emacs-rag--collapse-and-indent (s &optional indent)
;;   "折叠连续空行为单一换行并给每行添加 INDENT 空格前缀（默认 4）。
;; 返回处理后的字符串。"
;;   (let* ((indent (or indent 4))
;;          (prefix (make-string indent ?\s))
;;          ;; 先折叠多个空行为一个换行
;;          (collapsed (replace-regexp-in-string "\n\\{2,\\}" "\n" (or s "")))
;;          ;; 对每个换行后的行加缩进（不改变首行）
;;          (lines (split-string collapsed "\n")))
;;     (mapconcat #'identity
;;                (cl-loop for i from 0
;;                         for ln in lines
;;                         collect (if (= i 0) ln (concat prefix ln)))
;;                "\n")))

;; (defun new/emacs-rag--display-results (results mode query)
;;   "Display RESULTS from MODE search for QUERY while showing full multi-line content in minibuffer.

;; 策略：
;; - 每个候选的真实键为单行字符串 \"[id] header\"（作为匹配键与写入 *ivy-occur* 的可解析文本）。
;; - 同时将该字符串 propertize 为带有 'display property 的对象，display 值为包含 header + '\\n' + full-multiline-content 的字符串，
;;   从而在 minibuffer 中视觉上呈现为多行。
;; - action / update-fn 通过解析 id（优先使用 text-property，在 *ivy-occur* 场景回退到正则从文本解析）来找到原始 result 并 open/preview。
;; "
;;   (let* ((id->result (make-hash-table :test 'equal))
;;          (candidates
;;           ;; 构造 (propertized-display . result) 列表
;;           (cl-loop for result in results
;;                    for index from 1
;;                    collect
;;                    (let* ((id (emacs-rag--make-id index))
;;                           ;; header：简短单行（用于 id+header 的键）
;;                           (header (emacs-rag--format-result-header result index))
;;                           ;; full：完整多行内容（由 emacs-rag--format-result 生成，或者你也可直接构造）
;;                           (full (emacs-rag--format-result result index))
;;                           ;; 处理 full：折叠连续空行并在每行加缩进，避免空白行撑满空间
;;                           (full-processed (emacs-rag--collapse-and-indent
;;                                            (if (stringp full)
;;                                                ;; full 包含 header 行在最上面（如果 emacs-rag--format-result 已包含 header）
;;                                                ;; 我们要显示 header once; 若 full 已包含 header，strip first header line
;;                                                (let ((lines (split-string full "\n")))
;;                                                  ;; 若 full 首行看起来像 header（以 "index." 开头），移除首行，避免重复
;;                                                  (if (and lines (string-match-p "^[[:space:]]*[0-9]+\\. " (car lines)))
;;                                                      (mapconcat #'identity (cdr lines) "\n")
;;                                                    full))
;;                                              "")
;;                                            4))
;;                           ;; display-value: 可见内容：先放 [id] header，然后换行并放 processed full 内容
;;                           (display-value (if (and full-processed (not (string= full-processed "")))
;;                                              (concat "[" id "] " header "\n" full-processed)
;;                                            ;; 若没有 full 内容，仅显示 header 前缀
;;                                            (concat "[" id "] " header)))
;;                           ;; base-key: 实际键（单行），带 id 前缀，供 assoc / occur 用
;;                           (base-key (concat "[" id "] " header))
;;                           ;; propertize base-key：设置 id property 与 display property（视觉替代）
;;                           (propertized (propertize base-key
;;                                                    'emacs-rag-id id
;;                                                    'display display-value)))
;;                      (puthash id result id->result)
;;                      (cons propertized result)))))
;;     (if (null results)
;;         (message "No %s results for %s" mode query)
;;       (let* ((display-strings (mapcar #'car candidates))
;;              (count (length display-strings))
;;              ;; 这里按单行候选数计算高度（因每个候选有显式 id 前缀），你也可替换为更复杂的 height 计算
;;              (computed-height (min count (or emacs-rag-search-limit ivy-height))))
;;         ;; by zjx
;;         ;;(let ((ivy-height (max 1 computed-height)))
;;         (let ((ivy-height ivy-height))
;;           (ivy-read (format "%s search for %s: " mode query)
;;                     display-strings
;;                     :action (lambda (choice)
;;                               ;; 在 action 中解析 id：优先用 text-prop（minibuffer 内可用），回退用正则解析（来自 *ivy-occur* 的纯文本）
;;                               (let* ((id (or (get-text-property 0 'emacs-rag-id choice)
;;                                              (when (string-match "^\\[\\([0-9]+\\)\\]" choice)
;;                                                (match-string 1 choice))))
;;                                      (res (and id (gethash id id->result))))
;;                                 (if res
;;                                     (emacs-rag--open-result res)
;;                                   (message "emacs-rag: cannot find result for id %s" id))))
;;                     ;; update-fn 兼容 Ivy 不同调用路径，接受可选参数
;;                     :update-fn (lambda (&optional _str)
;;                                  (when (and (boundp 'ivy-last) ivy-last)
;;                                    (let ((cur (ivy-state-current ivy-last)))
;;                                      (when cur
;;                                        (let* ((id (or (get-text-property 0 'emacs-rag-id cur)
;;                                                       (when (string-match "^\\[\\([0-9]+\\)\\]" cur)
;;                                                         (match-string 1 cur))))
;;                                               (res (and id (gethash id id->result))))
;;                                          (when res (emacs-rag--show-preview res)))))))
;;                     :caller 'emacs-rag-search))))))

;;;;;;;;;;
;; 方案一
;;;;;;;;;;

;; ;;; Header-id + preview friendly emacs-rag--display-results
;; ;;; 替换后完整实现（含用到的 helper），确保 ivy-occur / RET 跳转与 preview 正常工作。

;; (require 'cl-lib)
;; (require 'ivy)

;; (defun emacs-rag--make-id (index)
;;   "基于 INDEX 生成短 id 字符串，例如 \"0001\"。"
;;   (format "%04d" index))

;; (defun emacs-rag--format-result-header (result index)
;;   "为 RESULT 生成单行 header（不含 id），用于构建 display 字符串的一部分。"
;;   (let* ((path (alist-get 'source_path result))
;;          (basename (file-name-nondirectory (or path "")))
;;          (chunk (or (alist-get 'chunk_index result) 0))
;;          (score (or (alist-get 'score result) 0.0)))
;;     (format "%2d. %-30s chunk %-3d score %.3f" index basename chunk score)))

;; (defun emacs-rag--show-preview (result)
;;   "在 *emacs-rag-preview* buffer 中显示 RESULT 的完整多行内容（wrapped，不截断）。"
;;   (let* ((buf (get-buffer-create "*emacs-rag-preview*"))
;;          (path (alist-get 'source_path result))
;;          (line (alist-get 'line_number result))
;;          (content (alist-get 'content result)))
;;     (with-current-buffer buf
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert (format "Source: %s\nLine: %s\n\n" (or path "") (or line "")))
;;         (if (fboundp 'emacs-rag--wrap-text)
;;             (insert (emacs-rag--wrap-text (or content "") emacs-rag-result-display-width))
;;           (insert (or content "")))
;;         (goto-char (point-min))
;;         (special-mode)))
;;     ;; 显示到新窗口，但不打断 minibuffer（display-buffer-pop-up-window）
;;     (display-buffer buf '(display-buffer-pop-up-window . ((inhibit-same-window . t))))))

;; (defun emacs-rag--display-results (results mode query)
;;   "Display RESULTS from MODE search for QUERY using header-id + properties.

;; 实现要点：
;; - 每个候选的显示字符串以可见 id 前缀开头（例如 \"[0001] ...\"），
;;   这保证 *ivy-occur* 中每一行都能作为匹配键。
;; - 同时在显示字符串上设置 'emacs-rag-id text-property（在 minibuffer 中可直接读���），
;;   如果 property 不可用（例如从 *ivy-occur* 回来的纯文本），会用正则从文本解析 id。
;; - 使用 id->result 哈希表在 action/update-fn 中查找原始 result，并用于打开/预览。
;; - 该实现保留原始 result 的完整多行内容用于 preview / open，不改变原始数据。"
;;   (let* ((id->result (make-hash-table :test 'equal))
;;          (candidates
;;           ;; 构造 (display-string . result) 列表；display-string 包含 id 前缀并被 propertize
;;           (cl-loop for result in results
;;                    for index from 1
;;                    collect
;;                    (let* ((id (emacs-rag--make-id index))
;;                           (header (emacs-rag--format-result-header result index))
;;                           ;; display 包含可见 id 前缀，且加上 text property 方便在 minibuffer 中读取
;;                           (display (propertize (format "[%s] %s" id header)
;;                                                'emacs-rag-id id)))
;;                      (puthash id result id->result)
;;                      (cons display result)))))
;;     (if (null results)
;;         (message "No %s results for %s" mode query)
;;       (let* ((display-strings (mapcar #'car candidates))
;;              (count (length display-strings))
;;              ;; 因为每个候选是单行 header，计算高度直接按项数或你现有的高度计算逻辑
;;              ;; by zjx (computed-height (min count (or emacs-rag-search-limit ivy-height))))
;;              (computed-height count))
;;         (let ((ivy-height (max 1 computed-height)))
;;           (ivy-read (format "%s search for %s: " mode query)
;;                     display-strings
;;                     :action (lambda (choice)
;;                               ;; 先尝试直接读取 text property（在 minibuffer 中通常存在）
;;                               (let* ((id (or (get-text-property 0 'emacs-rag-id choice)
;;                                              ;; 若 property 不存在（如来自 *ivy-occur*），回退到从文本解析 id
;;                                              (when (string-match "^\\[\\([0-9]+\\)\\]" choice)
;;                                                (match-string 1 choice))))
;;                                      (result (and id (gethash id id->result))))
;;                                 (if result
;;                                     (emacs-rag--open-result result)
;;                                   (message "emacs-rag: cannot find result for id %s" id))))
;;                     ;; 注意：:update-fn 可能会以 0 或 1 个参数被调用，签名应兼容
;;                     :update-fn (lambda (&optional _str)
;;                                  (when (and (boundp 'ivy-last) ivy-last)
;;                                    (let ((cur (ivy-state-current ivy-last)))
;;                                      (when cur
;;                                        (let* ((id (or (get-text-property 0 'emacs-rag-id cur)
;;                                                       (when (string-match "^\\[\\([0-9]+\\)\\]" cur)
;;                                                         (match-string 1 cur))))
;;                                               (res (and id (gethash id id->result))))
;;                                          (when res (emacs-rag--show-preview res)))))))
;;                     :caller 'emacs-rag-search))))))

(provide 'emacs-rag-search)
;;; emacs-rag-search.el ends here
