;;; trunk-io-tree.el --- Treemacs interface for trunk.io's trunk check
;; Copyright (C) 2024 Joe Sadusk
;; Author Joe Sadusk <joe@sadusk.com>
;; Version 0.0.1
;; Package-Requires: ((dash "2.19") (treemacs "3.1"))

;;; Commentary
;; This package provides a navigable interface to the meta lint tool "trunk check"
;; provided by https://trunk.io

;;; Code:

(require 'dash)
(require 'treemacs)
(require 'treemacs-treelib)

(defun group-by-key (key hashlist)
  (-map
   'cdr
   (-group-by
    (lambda (elem)
      (gethash key elem)
      )
    (-filter
     (lambda (elem)
       (gethash key elem nil)
       )
     hashlist)
    )
   )
  )

(defun trunk-check-sections (trunk-result)
  (let* (
        (trunk-result-issues (gethash "issues" trunk-result))
        (trunk-result-failures ())
        )
    `(
     ("Issues" . ,trunk-result-issues)
     ("Failures" . ,trunk-result-failures)
     )
    )
  )

(defun linecol-for-issue (issue)
  (format "%s:%s" (gethash "line" issue "**") (gethash "column" issue "**"))
  )

(defun trunk-issues-by-linecol (issues-for-file)
  (let (
        (issues-by-linecol (make-hash-table :test 'equal))
        (ret ())
        )
    (seq-doseq (issue issues-for-file)
      (let* (
             (linecol (linecol-for-issue issue))
             (issues-for-linecol (gethash linecol issues-by-linecol ()))
             )
        (push issue issues-for-linecol)
        (puthash linecol issues-for-linecol issues-by-linecol)
        )
      )
    (maphash (lambda (linecol issues-for-linecol)
               (push issues-for-linecol ret)
               )
             issues-by-linecol)

    (sort ret (lambda (a b)
                (let (
                      (aline (string-to-number (gethash "line" (car a))))
                      (bline (string-to-number (gethash "line" (car b))))
                      (acol (string-to-number (gethash "column" (car a))))
                      (bcol (string-to-number (gethash "column" (car b))))
                      )
                  (if (= aline bline)
                      (< acol bcol)
                    (< aline bline)
                    )
                  )
                )
          )
    )
  )

(defun trunk-issues-by-file (issues)
  (let (
        (issues-by-file (make-hash-table :test 'equal))
        (ret ())
        )
    (seq-doseq (issue issues)
          (let* (
                (file (gethash "file" issue))
                (issues-for-file (gethash file issues-by-file ()))
                )
            (if (string= "ISSUE_CLASS_NEW" (gethash "issueClass" issue))
                (progn
                  (push issue issues-for-file)
                  (puthash file issues-for-file issues-by-file)
                  )
              )
            )
          )
    (maphash (lambda (file issues-for-file)
               (push `(,file . ,issues-for-file) ret)
               )
             issues-by-file)
    (sort ret (lambda (a b) (string< (car a) (car b))))
    )
  )

(defun trunk-tree-issue-properties (issue)
  `(
     ,(format "[%s - %s] %s:%s"
              (gethash "linter" issue)
              (gethash "level" issue)
              (gethash "line" issue)
              (gethash "column" issue))
     .
     (
      ,(gethash "message" issue)
      )
    )
  )

(defun trunk-tree-level (level-id)
  (cond
   ((string= level-id "LEVEL_HIGH")
    (propertize "high" 'face 'error));'(:foreground "red")))
   ((string= level-id "LEVEL_WARN")
    (propertize "warn" 'face 'warning));'(:foreground "yellow")))
   ((string= level-id "LEVEL_INFO")
    (propertize "info" 'face 'success));'(:foreground "green")))
   (t level-id)
   )
  )

(defun trunk-tree-issue-label (issue)
  (let* (
         (linecol (format "%s %s : %s %s"
                          (propertize "line" 'face 'shadow)
                          (propertize (gethash "line" issue "**") 'face 'bold)
                          (propertize "col" 'face 'shadow)
                          (propertize (gethash "column" issue "**") 'face 'bold)))
         (lintwarn (format "[%s - %s]"
                           (propertize (gethash "linter" issue)
                                       'face 'font-lock-string-face)
                           (trunk-tree-level (trunk-tree-level
                                              (gethash "level" issue)))))
         (gap-len (- (window-body-width) (+ (length linecol) (length lintwarn) 7)))
         (gap (make-string gap-len ?\s))
         )
    (format "%s%s%s" linecol gap lintwarn)
    )
  )

(defun trunk-tree-issue-linecol-label (issue)
  (let* (
         (linecol (format "%s %s : %s %s"
                          (propertize "line" 'face 'shadow)
                          (propertize (gethash "line" issue "**") 'face 'bold)
                          (propertize "col" 'face 'shadow)
                          (propertize (gethash "column" issue "**") 'face 'bold)))
         )
    linecol
    )
  )

(defun trunk-tree-issue-lintlevel-label (issue)
  (let* (
         (lintwarn (format "[%s - %s]"
                           (propertize (gethash "linter" issue)
                                       'face 'font-lock-string-face)
                           (trunk-tree-level (trunk-tree-level
                                              (gethash "level" issue)))))
         )
    lintwarn
    )
  )

(defun trunk-tree-RET-issue-action (&optional _)
  (let* (
         (file (-some-> (treemacs-current-button)
                     (treemacs-button-get :file)))
         (line (-some-> (treemacs-current-button)
                     (treemacs-button-get :line)))
         (column (-some-> (treemacs-current-button)
                     (treemacs-button-get :column)))
         )
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- (string-to-number line)))
    (move-to-column (1- (string-to-number column)))
    )
  )

(treemacs-define-variadic-entry-node-type trunk-result-variadic
  :key 'trunk-result-variadic
  :children (trunk-check-sections trunk-result)
  :child-type 'trunk-tree-section-group)

(treemacs-define-expandable-node-type
 trunk-tree-section-group
 :closed-icon "+ "
 :open-icon "- "
 :label (propertize (car item) 'face 'font-lock-keyword-face)
 :key 'trunk-tree-section-group
 :children (if (string= (car item) "Issues") (trunk-issues-by-file (cdr item)) ()) 
 :child-type 'trunk-tree-file-issue-group
 )

(treemacs-define-expandable-node-type
 trunk-tree-file-issue-group
 :closed-icon "+ "
 :open-icon "- "
 :label (propertize (car item) 'face 'font-lock-variable-name-face)
 :key 'trunk-tree-file-issue-group
 :children (trunk-issues-by-linecol (cdr item))
 :child-type 'trunk-tree-issue-linecol-group
 )

(treemacs-define-expandable-node-type
 trunk-tree-failure-group
 :closed-icon "+ "
 :open-icon "- "
 :label ""
 :key 'trunk-tree-failure-group
 :children '()
 :child-type 'trunk-tree-failure-leaf
 )

(treemacs-define-leaf-node-type
  trunk-tree-failure-leaf
  :icon ""
  :label ""
  :key item
  )


(treemacs-define-expandable-node-type
 trunk-tree-issue-linecol-group
 :closed-icon "+ "
 :open-icon "- "
 :label (trunk-tree-issue-linecol-label (car item))
 :key 'trunk-tree-issue-group
 :children (group-by-key "linter" item )
 :child-type 'trunk-tree-issue-group
 :more-properties `(
                    :file ,(gethash "file" (car item))
                    :line ,(gethash "line" (car item))
                    :column ,(gethash "column" (car item))
                    :column1 ,(gethash "column" (car item))
                    )
 :ret-action #'trunk-tree-RET-issue-action
 )

(treemacs-define-expandable-node-type
 trunk-tree-issue-group
 :closed-icon "+ "
 :open-icon "- "
 :label (trunk-tree-issue-lintlevel-label (car item))
 :key 'trunk-tree-issue-group
 :children item
 :child-type 'trunk-tree-message-leaf
 :more-properties `(
                    :file ,(gethash "file" (car item))
                    :line ,(gethash "line" (car item))
                    :column ,(gethash "column" (car item))
                    :column2 ,(gethash "column" (car item))
                    )
 :ret-action #'trunk-tree-RET-issue-action
 )

(treemacs-define-leaf-node-type
  trunk-tree-message-leaf
  :icon ""
  :label (gethash "message" item)
  :key item
  :more-properties `(
                     :file ,(gethash "file" item)
                     :line ,(gethash "line" item)
                     :column ,(gethash "column" item)
                    :column3 ,(gethash "column" item)
                    )
  :ret-action #'trunk-tree-RET-issue-action
  )

(defun matches-in-buffer (regexp &optional buffer)
  "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (match-string 0) matches)))))
      matches)))

(defun json-at-last-line (buffer)
  (with-current-buffer buffer
    (progn
      (goto-char (point-max))
      (beginning-of-line)
      (json-parse-buffer)        
      )
    )
  )



(defun trunk-check ()
  (interactive)
  (message "Running trunk check")
  (let* (
         (default-directory (project-root (project-current)))
         (buffer-name (concat "*trunk check <" (project-root (project-current)) ">*"))
         (trunk-buffer (get-buffer-create buffer-name))
         (trunk-command "tools/trunk check --no-progress --output=json |grep '^{.*}$'|head -n1 | sed -z '$ s/\\n$//'")
         )
    (message trunk-command)
    (with-current-buffer trunk-buffer
      (if buffer-read-only
          (read-only-mode)
        )
      (dolist (thismode local-minor-modes)
        (funcall thismode 0)
        )
      (erase-buffer)
      (insert "Running trunk check\n")
      (comint-mode)
      )
    (let (
          (trunk-process (start-file-process-shell-command buffer-name trunk-buffer trunk-command))
          )
      (set-process-sentinel
       trunk-process
       (lambda (process event)
         (let* (
                (trunk-buffer (process-buffer process))
                (trunk-result (json-at-last-line trunk-buffer))
                )
           (message "Completed trunk check")
           (save-selected-window
             (pop-to-buffer trunk-buffer)
             (let (
                   (treemacs-user-mode-line-format (format "*** Trunk check <%s> ***" (project-root (project-current))))
                   )
               (treemacs-initialize
                   trunk-result-variadic
                 :with-expand-depth 'all
                 :and-do (setf treemacs-space-between-root-nodes t)
                 )
               )
             )
           )
         )
       )
      )
    )
  )

(provide 'trunk-io-tree)
