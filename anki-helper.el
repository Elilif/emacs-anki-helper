;;; anki-helper.el --- create anki cards and sync them -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: flashcards
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'thunk)
(require 'org)
(require 'org-element)
(require 'org-macs)

(defgroup anki-helper nil
  ""
  :group 'applications)

(defcustom anki-helper-default-note-type "Basic"
  "Default note type."
  :type 'string
  :group 'anki-helper)

(defcustom anki-helper-default-match "+LEVEL=1"
  "Default match used in `org-map-entries` for sync all."
  :type 'string
  :group 'org-anki)

(defcustom anki-helper-default-deck "Default"
  ""
  :type 'string
  :group 'anki-helper)

(defcustom anki-helper-inherit-tags t
  ""
  :type 'boolen
  :group 'anki-helper)

(defcustom anki-helper-note-types '(("Basic" "Front" "Back")
                                    ("Basic (and reversed card)" "Front" "Back")
                                    ("Basic (optional reversed card)" "Front" "Back")
                                    ("Cloze" "Text" "Back Extra"))
  ""
  :group 'anki-helper
  :type '(repeat (list (repeat string))))

(defcustom anki-helper-media-directory "~/.local/share/Anki2/User 1/collection.media/"
  ""
  :group 'anki-helper)

;; see:
;; https://github.com/ankitects/anki/blob/main/qt/aqt/editor.py#L62
(defcustom anki-helper-audio-formats '("3gp" "aac" "avi" "flac" "flv" "m4a"
                                       "mkv" "mov" "mp3" "mp4" "mpeg" "mpg"
                                       "oga" "ogg" "ogv" "ogx" "opus" "spx"
                                       "swf" "wav" "webm")
  ""
  :group 'anki-helper)

(cl-defstruct anki-helper--note maybe-id fields tags deck model orig-pos callback)

(defvar anki-helper--cloze-counter 0)
(defvar anki-helper--org2html-image-counter 0)
(defconst anki-helper-prop-note-id "ANKI_NOTE_ID")
(defconst anki-helper-prop-deck "ANKI_DECK")
(defconst anki-helper-match "ANKI_MATCH")
(defconst anki-helper-note-type "ANKI_NOTE_TYPE")
(defconst anki-helper-prop-global-tags "ANKI_TAGS")

(defvar anki-helper-action-alist
  '((addNote . anki-helper--action-addnote)
    (addNotes . anki-helper--action-addnotes)
    (deleteNotes . anki-helper--action-deletenotes)))

(defun anki-helper--get-note-fields (note)
  (cdr (assoc note anki-helper-note-types)))

(defun anki-helper--body (action &optional params)
  "Wrap ACTION and PARAMS to a json payload AnkiConnect expects."
  (if params
      `(("action" . ,action)
        ("version" . 6)
        ("params" . ,params))
    `(("action" . ,action)
      ("version" . 6))))

(defun anki-helper--note-to-json (note)
  `(("deckName" . ,(anki-helper--note-deck note))
    ("modelName" . ,(anki-helper--note-model note))
    ("fields"    . ,(anki-helper--note-fields note))
    ("tags" . ,(or (anki-helper--note-tags note) ""))
    ("options" .
     (("allowDuplicate" . :json-false)
      ("duplicateScope" . "deck")))))

(defun anki-helper--action-addnote (note)
  "Create an `addNote' json structure to be added to DECK with
card FRONT and BACK strings."
  (anki-helper--body
   "addNote"
   `(("note" .
      ,(anki-helper--note-to-json note)))))

(defun anki-helper--action-addnotes (notes)
  "Create an `addNote' json structure to be added to DECK with
card FRONT and BACK strings."
  (anki-helper--body
   "addNotes"
   `(("notes" .
      (,@notes)))))

(defun anki-helper--action-deletenotes (ids)
  (anki-helper--body
   "deleteNotes"
   `(("notes" .
      (,@ids)))))

(defun anki-helper--get-global-keyword (keyword)
  (cadar (org-collect-keywords (list keyword))))

(defun anki-helper--find-prop (name default)
  "Find property with NAME from
1. item,
2. inherited from parents
3. in-buffer setting
4. otherwise use DEFAULT"
  (thunk-let
      ((prop-item (org-entry-get nil name t))
       (keyword-global (anki-helper--get-global-keyword name)))
    (cond
     ((stringp prop-item) prop-item)
     ((stringp keyword-global) keyword-global)
     ((stringp default) default)
     (t (error "No property '%s' in item nor file nor set as default!"
               name)))))

(defun anki-helper--get-tags ()
  (delete-dups
   (split-string
    (let ((global-tags (anki-helper--get-global-keyword anki-helper-prop-global-tags)))
      (concat
       (if anki-helper-inherit-tags
           (substring-no-properties (or (org-entry-get nil "ALLTAGS") ""))
         (org-entry-get nil "TAGS"))
       global-tags)) ":" t)))

(defun anki-helper--get-match ()
  (let ((file-global (anki-helper--get-global-keyword anki-helper-match)))
    (if (stringp file-global)
        file-global
      anki-helper-default-match)))

(defun anki-helper--org-html-verbatim (verbatim _contents _info)
  (cl-incf anki-helper--cloze-counter)
  (format "{{c%d::%s}}"
          anki-helper--cloze-counter
          (org-html-encode-plain-text (org-element-property :value verbatim))))

(defun anki-helper--org2html-link (text backend info)
  (when (eq backend 'html)
    (when-let*
        ((link (nth anki-helper--org2html-image-counter
                    (org-element-map (plist-get info :parse-tree) 'link 'identity)))
         (link-path (org-element-property :path link))
         (file-exists-p (file-exists-p link-path))
         (file-extension (file-name-extension link-path))
         (link-type (org-element-property :type link))
         (hash (md5 (format "%s%s%s" (random) text (recent-keys))))
         (new-name (file-name-with-extension hash file-extension))
         (full-path (file-name-concat
                     anki-helper-media-directory
                     new-name)))
      (cond
       ((and (plist-get info :html-inline-images)
             (org-export-inline-image-p link
                                        (plist-get info :html-inline-image-rules)))
        (copy-file link-path full-path)
        (setq text (replace-regexp-in-string "img src=\"\\(.*?\\)\"" new-name text
                                             nil nil 1)))
       ((member file-extension anki-helper-audio-formats)
        (copy-file link-path full-path)
        (setq text (format "<br>[sound:%s]" new-name))))))
  (cl-incf anki-helper--org2html-image-counter)
  text)

(defun anki-helper--org2html (string)
  (cl-letf (((symbol-function #'org-html-final-function) #'(lambda (contents &rest _args)
                                                             contents))
            ((symbol-function #'org-babel-exp-process-buffer) #'ignore))
    (let ((org-export-filter-link-functions '(anki-helper--org2html-link))
          (anki-helper--org2html-image-counter 0))
      (org-export-string-as string 'html t '(:with-toc nil)))))

(defun anki-helper-make-cloze (string)
  (cl-letf (((symbol-function #'org-html-verbatim) #'anki-helper--org-html-verbatim)
            (anki-helper--cloze-counter 0))
    (anki-helper--org2html string)))

(defvar anki-helper--process-alist nil)
(defvar anki-helper--result nil)
(defvar anki-helper--pos nil)

(defun anki-helper--set-note-id ()
  (dolist (pair (seq-mapn #'cons anki-helper--pos anki-helper--result))
    (let ((marker (car pair))
          (id (cdr pair)))
      (save-excursion
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (org-set-property anki-helper-prop-note-id (number-to-string id)))))))

(defun anki-helper--curl-sentinel (process _status)
  (let ((proc-buf (process-buffer process)))
    (when (eq (process-status process) 'exit)
      (with-current-buffer proc-buf
        (goto-char (point-min))
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               result)
          (setq eli-result (buffer-string))
          (setq result (json-read))
          (if-let ((err (plist-get result :error)))
              (message err)
            (setq anki-helper--result (plist-get result :result))
            (when (member (car (alist-get process anki-helper--process-alist))
                          '(addNote addNotes))
              (run-with-idle-timer 1 nil #'anki-helper--set-note-id))
            ))))
    (setf (alist-get process anki-helper--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun anki-helper--request-args (action body)
  (let* ((func (alist-get action anki-helper-action-alist))
         (file-name (make-temp-name "/tmp/anki")))
    (with-temp-file file-name
      (insert (json-encode (funcall func body))))
    (list
     "http://127.0.0.1:8765"
     (format "-X%s" "POST")
     (format "-d@%s" file-name))))

(defun anki-helper-request (action body)
  (let* ((args (anki-helper--request-args action body))
         (process (apply #'start-process
                         "anki-helper"
                         (generate-new-buffer "*anki-helper*")
                         "curl"
                         args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process anki-helper--process-alist)
            (cons action body))
      (set-process-sentinel process #'anki-helper--curl-sentinel))))

;; (defun anki-helper--entry-get-fields ()
;;   (let* ((note-type (anki-helper--find-prop
;;                      anki-helper-note-type
;;                      anki-helper-default-note-type))
;;          (front (org-get-heading t t t t))
;;          (back (org-get-entry))
;;          (hash (md5 (format "%s%s%s" (random) front back)))
;;          (html (anki-helper--org2html (concat
;;                                        front
;;                                        (format "\n\n%s\n\n" hash)
;;                                        back)))
;;          (result (string-split html (format "<p>\n%s\n</p>" hash)
;;                                t "\n+")))
;;     (list (cons (car (anki-helper--get-note-fields
;;                       anki-helper-default-note-type))
;;                 (car result))
;;           (cons (cadr (anki-helper--get-note-fields
;;                        anki-helper-default-note-type))
;;                 (cadr result)))))

;; (defun anki-helper-sync-entry ()
;;   (interactive)
;;   (let* ((maybe-id (org-entry-get nil anki-helper-prop-note-id))
;;          (deck (anki-helper--find-prop
;;                 anki-helper-prop-deck
;;                 anki-helper-default-deck))
;;          (tags (anki-helper--get-tags))
;;          (model (anki-helper--find-prop
;;                  anki-helper-note-type
;;                  anki-helper-default-note-type))
;;          (fields (anki-helper--entry-get-fields))
;;          (orig-pos (point-marker)))
;;     (anki-helper-request 'addNote
;;                          (make-anki-helper--note
;;                           :maybe-id maybe-id
;;                           :fields fields
;;                           :tags tags
;;                           :deck deck
;;                           :model model
;;                           :orig-pos orig-pos))))

(defun anki-helper--entry-get-content ()
  (let* ((note-type (anki-helper--find-prop
                     anki-helper-note-type
                     anki-helper-default-note-type))
         (front (org-no-properties (org-get-heading t t t t)))
         (back (org-no-properties (org-get-entry)))
         (hash (md5 (format "%s%s%s" (random) front back))))
    (list :content (format "%s\n\n%s\n\n%s\n\n" front hash back)
          :hash hash :pos (point-marker) :note-type note-type)))

(defun anki-helper--get-fields (plist)
  (let ((note-type (plist-get plist :note-type))
        (pair (plist-get plist :pair)))
    (list (cons (car (anki-helper--get-note-fields
                      note-type))
                (car pair))
          (cons (cadr (anki-helper--get-note-fields
                       note-type))
                (cadr pair)))))

(defun anki-helper--make-note (elt)
  (let ((fields (anki-helper--get-fields (list
                                          :note-type (plist-get (car elt) :note-type)
                                          :pair (string-split (cdr elt) (format "<p>\n%s\n</p>" (plist-get (car elt) :hash)) t "\n+"))))
        (deck anki-helper-default-deck)
        (model (plist-get (car elt) :note-type))
        (orig-pos (plist-get (car elt) :pos)))
    (anki-helper--note-to-json (make-anki-helper--note
                                :maybe-id nil
                                :fields fields
                                :tags nil
                                :deck deck
                                :model model
                                :orig-pos orig-pos))))

(defun anki-helper--entry-get-all ()
  (let* ((contents (org-map-entries
                    #'anki-helper--entry-get-content
                    (concat (format "-%s={.+}" org-anki-prop-note-id)
                            (anki-helper--get-match))))
         (hash (md5 (format "%s%s" (random) (recent-keys))))
         (html (anki-helper--org2html (mapconcat
                                       (lambda (elt)
                                         (plist-get elt :content))
                                       contents (format "\n\n%s\n\n" hash))))
         (pair (seq-mapn #'cons
                         (mapcar #'cddr contents)
                         (string-split html (format "<p>\n%s\n</p>" hash)
                                       t "\n+"))))
    (setq anki-helper--pos (mapcar (lambda (plist)
                                     (plist-get plist :pos))
                                   contents))
    (mapcar #'anki-helper--make-note pair)))

;;;###autoload
(defun anki-helper-entry-sync ()
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (anki-helper-entry-sync-all))))

;;;###autoload
(defun anki-helper-entry-sync-all ()
  (interactive)
  (anki-helper-request 'addNotes (anki-helper--entry-get-all)))

;;;###autoload
(defun anki-helper-entry-delete ()
  (interactive)
  (when-let ((id (string-to-number
                  (org-entry-get nil org-anki-prop-note-id))))
    (anki-helper-request 'deleteNotes (list id))
    (org-entry-delete nil org-anki-prop-note-id)))

;;;###autoload
(defun anki-helper-entry-delete-all ()
  (interactive)
  (when-let ((ids (org-map-entries
                   (lambda ()
                     (let ((id (string-to-number
                                (org-entry-get nil org-anki-prop-note-id))))
                       (org-entry-delete nil org-anki-prop-note-id)
                       id))
                   (concat
                    (format "%s={.+}" org-anki-prop-note-id)
                    (anki-helper--get-match)))))
    (anki-helper-request 'deleteNotes ids)))

(defun anki-helper-create-card (front back &optional id tags deck model)
  (let ((fields (list (cons (car (anki-helper--get-note-fields
                                  anki-helper-default-note-type))
                            front)
                      (cons (cadr (anki-helper--get-note-fields
                                   anki-helper-default-note-type))
                            back)))
        (deck (or deck anki-helper-default-deck))
        (model (or model anki-helper-default-note-type))
        (orig-pos (point-marker)))
    (make-anki-helper--note
     :maybe-id id
     :fields fields
     :tags tags
     :deck deck
     :model model
     :orig-pos orig-pos)))

;;;###autoload
(defun anki-helper-set-front-region ()
  (interactive)
  (letrec ((ah-delete-sec-region (lambda ()
                                   (delete-overlay mouse-secondary-overlay)
                                   (advice-remove 'keyboard-quit ah-delete-sec-region))))
    (if (not (region-active-p))
        (user-error "Please select a region!")
      (secondary-selection-from-region)
      (advice-add 'keyboard-quit :before ah-delete-sec-region)
      (deactivate-mark t))))

;;;###autoload
(defun anki-helper-on-demand ()
  (interactive)
  (unless (region-active-p)
    (user-error "Please select a region!"))
  (let* ((front (buffer-substring-no-properties
                 (overlay-start mouse-secondary-overlay)
                 (overlay-end mouse-secondary-overlay)))
         (back (buffer-substring-no-properties
                (region-beginning)
                (region-end))))
    (anki-helper-request 'addNote (anki-helper-create-card front back))))


(provide 'anki-helper)
;;; anki-helper.el ends here
