;;; org-bake-materialize.el --- Built-in materializers for org-bake  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (async "1.9.9") (ox-json "1"))
;; URL: https://github.com/bitbloxhub/org-bake
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Built-in materializers for org-bake.

;;; Code:

(require 'org-bake-process)
(declare-function org-bake-process-register-materializer
                  "org-bake-process"
                  (name &rest properties))


(defun org-bake-materialize--json-get (object key)
  "Return KEY from parsed JSON OBJECT."
  (when (listp object)
    (let ((string-key
           (if (stringp key)
               key
             (symbol-name key)))
          (symbol-key
           (if (symbolp key)
               key
             (intern key))))
      (or (alist-get key object nil nil #'equal)
          (alist-get string-key object nil nil #'equal)
          (alist-get symbol-key object nil nil #'equal)))))

(defun org-bake-materialize--sequence-list (value)
  "Return VALUE as list."
  (cond
   ((null value)
    nil)
   ((vectorp value)
    (append value nil))
   ((listp value)
    value)
   (t
    (list value))))

(defun org-bake-materialize--walk-org-ast (node fn)
  "Walk org AST NODE recursively, calling FN for each node object."
  (when (listp node)
    (when (org-bake-materialize--json-get node "type")
      (funcall fn node))
    (dolist (child
             (org-bake-materialize--sequence-list
              (org-bake-materialize--json-get node "contents")))
      (org-bake-materialize--walk-org-ast child fn))))

(defun org-bake-materialize--id-document-table (documents)
  "Return hash table mapping Org :ID: values to document ids for DOCUMENTS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (document documents)
      (let* ((document-id
              (org-bake-materialize--json-get document "id"))
             (ast
              (org-bake-materialize--json-get document "document"))
             (document-drawer
              (org-bake-materialize--json-get ast "drawer"))
             (document-id-property
              (org-bake-materialize--json-get document-drawer "ID")))
        (when (stringp document-id-property)
          (puthash document-id-property document-id table))
        (org-bake-materialize--walk-org-ast
         ast
         (lambda (node)
           (let* ((node-type
                   (org-bake-materialize--json-get node "type"))
                  (drawer
                   (org-bake-materialize--json-get node "drawer"))
                  (headline-id
                   (org-bake-materialize--json-get drawer "ID")))
             (when (and (stringp node-type)
                        (string-equal node-type "headline")
                        (stringp headline-id))
               (puthash headline-id document-id table)))))))
    table))

(defun org-bake-materialize-ids-builder (_workspace documents)
  "Build ID to document-id materialization data from DOCUMENTS."
  (org-bake-materialize--id-document-table documents))

(org-bake-process-register-materializer
 'ids
 :version "v1"
 :builder 'org-bake-materialize-ids-builder
 :feature 'org-bake-materialize
 :description "Map Org :ID: values (top-level and headings) to document ids.")

(defun org-bake-materialize-tags-builder (_workspace documents)
  "Build tags materialization data from DOCUMENTS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (document documents)
      (let* ((document-id
              (org-bake-materialize--json-get document "id"))
             (ast
              (org-bake-materialize--json-get document "document"))
             (properties
              (org-bake-materialize--json-get ast "properties"))
             (tags
              (or (org-bake-materialize--json-get
                   properties "filetags")
                  (org-bake-materialize--json-get
                   properties "file_tags"))))
        (dolist (tag (org-bake-materialize--sequence-list tags))
          (puthash
           tag (cons document-id (gethash tag table)) table))))
    (let ((result (make-hash-table :test #'equal)))
      (maphash
       (lambda (tag document-ids)
         (puthash tag (vconcat (nreverse document-ids)) result))
       table)
      result)))

(org-bake-process-register-materializer
 'tags
 :version "v1"
 :builder 'org-bake-materialize-tags-builder
 :feature 'org-bake-materialize
 :description "Map document filetags to document ids.")

(defun org-bake-materialize-filepaths-builder (_workspace documents)
  "Build file paths materialization data from DOCUMENTS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (document documents)
      (let* ((document-id
              (org-bake-materialize--json-get document "id"))
             (source
              (org-bake-materialize--json-get document "source"))
             (path (org-bake-materialize--json-get source "path")))
        (puthash document-id path table)))
    table))

(org-bake-process-register-materializer
 'filepaths
 :version "v1"
 :builder 'org-bake-materialize-filepaths-builder
 :feature 'org-bake-materialize
 :description "Map document ids to document paths.")

(defun org-bake-materialize-backlinks-builder (_workspace documents)
  "Build backlinks materialization data from DOCUMENTS.

Result shape is an alist with key `edge_list'.
Each edge is `(source_id target_id via count)'."
  (let ((id-table (org-bake-materialize--id-document-table documents))
        (path-table (make-hash-table :test #'equal))
        (edge-counts (make-hash-table :test #'equal))
        (edge-meta (make-hash-table :test #'equal))
        edge-order)
    (dolist (document documents)
      (let* ((document-id
              (org-bake-materialize--json-get document "id"))
             (source
              (org-bake-materialize--json-get document "source"))
             (source-path
              (org-bake-materialize--json-get source "path")))
        (when (and (stringp document-id) (stringp source-path))
          (puthash
           (expand-file-name source-path) document-id path-table))))
    (dolist (document documents)
      (let* ((source-document-id
              (org-bake-materialize--json-get document "id"))
             (ast
              (org-bake-materialize--json-get document "document"))
             (source
              (org-bake-materialize--json-get document "source"))
             (source-path
              (org-bake-materialize--json-get source "path"))
             (source-directory
              (when (stringp source-path)
                (file-name-directory source-path))))
        (when (and (stringp source-document-id) ast)
          (org-bake-materialize--walk-org-ast
           ast
           (lambda (node)
             (let* ((node-type
                     (org-bake-materialize--json-get node "type"))
                    (properties
                     (org-bake-materialize--json-get
                      node "properties"))
                    (link-type
                     (org-bake-materialize--json-get
                      properties "type"))
                    (link-path
                     (org-bake-materialize--json-get
                      properties "path"))
                    target-document-id
                    via)
               (when (and (stringp node-type)
                          (string-equal node-type "link")
                          (stringp link-type)
                          (stringp link-path))
                 (cond
                  ((string-equal link-type "id")
                   (setq target-document-id
                         (gethash link-path id-table))
                   (setq via "id"))
                  ((and (string-equal link-type "file")
                        source-directory)
                   (setq target-document-id
                         (gethash
                          (expand-file-name link-path
                                            source-directory)
                          path-table))
                   (setq via "file")))
                 (when (and (stringp target-document-id)
                            (stringp via)
                            (not
                             (string-equal
                              target-document-id source-document-id)))
                   (let* ((edge-key
                           (format "%s|%s|%s"
                                   source-document-id
                                   target-document-id
                                   via))
                          (count (gethash edge-key edge-counts 0)))
                     (when (= count 0)
                       (puthash
                        edge-key
                        `((source_id . ,source-document-id)
                          (target_id . ,target-document-id)
                          (via . ,via))
                        edge-meta)
                       (push edge-key edge-order))
                     (puthash
                      edge-key (1+ count) edge-counts))))))))))
    (let (edge-list)
      (dolist (edge-key (nreverse edge-order))
        (let* ((meta (gethash edge-key edge-meta))
               (source-id
                (org-bake-materialize--json-get meta "source_id"))
               (target-id
                (org-bake-materialize--json-get meta "target_id"))
               (via (org-bake-materialize--json-get meta "via"))
               (count (gethash edge-key edge-counts 0)))
          (push `((source_id . ,source-id)
                  (target_id . ,target-id)
                  (via . ,via)
                  (count . ,count))
                edge-list)))
      `((edge_list . ,(vconcat (nreverse edge-list)))))))

(org-bake-process-register-materializer
 'backlinks
 :version "v1"
 :builder 'org-bake-materialize-backlinks-builder
 :feature 'org-bake-materialize
 :description "Build backlinks edge list with source/target, link kind, and count.")

(defun org-bake-materialize--headline-row
    (document-id source-path node)
  "Return heading row alist from headline NODE.

DOCUMENT-ID and SOURCE-PATH identify source document."
  (let* ((properties
          (org-bake-materialize--json-get node "properties"))
         (drawer (org-bake-materialize--json-get node "drawer"))
         (title
          (org-bake-materialize--json-get properties "raw-value"))
         (todo-keyword
          (org-bake-materialize--json-get properties "todo-keyword"))
         (priority
          (org-bake-materialize--json-get properties "priority"))
         (tags
          (org-bake-materialize--sequence-list
           (org-bake-materialize--json-get properties "tags")))
         (begin (org-bake-materialize--json-get properties "begin"))
         (level (org-bake-materialize--json-get properties "level"))
         (source-id (org-bake-materialize--json-get drawer "ID")))
    `((document_id . ,document-id)
      (source_path . ,source-path)
      (source_id . ,source-id)
      (begin . ,begin)
      (level . ,level)
      (title . ,title)
      (todo . ,todo-keyword)
      (priority . ,priority)
      (tags . ,(vconcat tags)))))

(defun org-bake-materialize-headings-builder (_workspace documents)
  "Build heading rows from DOCUMENTS."
  (let (items)
    (dolist (document documents)
      (let* ((document-id
              (org-bake-materialize--json-get document "id"))
             (source
              (org-bake-materialize--json-get document "source"))
             (source-path
              (org-bake-materialize--json-get source "path"))
             (ast
              (org-bake-materialize--json-get document "document")))
        (when (and (stringp document-id) (stringp source-path) ast)
          (org-bake-materialize--walk-org-ast
           ast
           (lambda (node)
             (let ((node-type
                    (org-bake-materialize--json-get node "type")))
               (when (and (stringp node-type)
                          (string-equal node-type "headline"))
                 (push (org-bake-materialize--headline-row
                        document-id source-path node)
                       items))))))))
    (vconcat (nreverse items))))

(org-bake-process-register-materializer
 'headings
 :version "v1"
 :builder 'org-bake-materialize-headings-builder
 :feature 'org-bake-materialize
 :description "Flatten all headlines with metadata for fast lookup/use.")


(defun org-bake-materialize--timestamp-raw-value (timestamp-node)
  "Return raw timestamp text from TIMESTAMP-NODE, or nil."
  (let ((properties
         (org-bake-materialize--json-get
          timestamp-node "properties")))
    (or (org-bake-materialize--json-get timestamp-node "raw-value")
        (org-bake-materialize--json-get properties "raw-value"))))

(defun org-bake-materialize--headline-agenda-item
    (document-id source-path node)
  "Return agenda item alist from headline NODE, or nil.

DOCUMENT-ID and SOURCE-PATH identify source document."
  (let* ((properties
          (org-bake-materialize--json-get node "properties"))
         (drawer (org-bake-materialize--json-get node "drawer"))
         (todo-keyword
          (org-bake-materialize--json-get properties "todo-keyword"))
         (scheduled-raw
          (org-bake-materialize--timestamp-raw-value
           (org-bake-materialize--json-get properties "scheduled")))
         (deadline-raw
          (org-bake-materialize--timestamp-raw-value
           (org-bake-materialize--json-get properties "deadline")))
         (title
          (org-bake-materialize--json-get properties "raw-value"))
         (priority
          (org-bake-materialize--json-get properties "priority"))
         (tags
          (org-bake-materialize--sequence-list
           (org-bake-materialize--json-get properties "tags")))
         (begin (org-bake-materialize--json-get properties "begin"))
         (level (org-bake-materialize--json-get properties "level"))
         (source-id (org-bake-materialize--json-get drawer "ID")))
    (when (or (stringp todo-keyword)
              (stringp scheduled-raw)
              (stringp deadline-raw))
      `((document_id . ,document-id)
        (source_path . ,source-path)
        (source_id . ,source-id)
        (begin . ,begin)
        (level . ,level)
        (title . ,title)
        (todo . ,todo-keyword)
        (priority . ,priority)
        (scheduled . ,scheduled-raw)
        (deadline . ,deadline-raw)
        (tags . ,(vconcat tags))))))

(defun org-bake-materialize-agenda-items-builder
    (_workspace documents)
  "Build agenda item rows from DOCUMENTS."
  (let (items)
    (dolist (document documents)
      (let* ((document-id
              (org-bake-materialize--json-get document "id"))
             (source
              (org-bake-materialize--json-get document "source"))
             (source-path
              (org-bake-materialize--json-get source "path"))
             (ast
              (org-bake-materialize--json-get document "document")))
        (when (and (stringp document-id) (stringp source-path) ast)
          (org-bake-materialize--walk-org-ast
           ast
           (lambda (node)
             (let ((node-type
                    (org-bake-materialize--json-get node "type")))
               (when (and (stringp node-type)
                          (string-equal node-type "headline"))
                 (let ((item
                        (org-bake-materialize--headline-agenda-item
                         document-id source-path node)))
                   (when item
                     (push item items))))))))))
    (vconcat (nreverse items))))

(org-bake-process-register-materializer
 'agenda-items
 :version "v1"
 :builder 'org-bake-materialize-agenda-items-builder
 :feature 'org-bake-materialize
 :description "Flatten actionable/scheduled headlines for generated agenda views.")


(provide 'org-bake-materialize)
;;; org-bake-materialize.el ends here
