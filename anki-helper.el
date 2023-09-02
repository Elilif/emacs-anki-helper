;;; anki-helper.el --- create anki cards and sync them -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs

;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: flashcards
;; SPDX-License-Identifier: GPL-3.0-or-later

(cl-defstruct anki-helper--note maybe-id fields tags deck model orig-pos)
(defgroup anki-helper nil
  ""
  :group 'applications)

(defcustom anki-helper-default-note-type "Basic"
  "Default note type."
  :type 'string
  :group 'anki-helper)

(defcustom anki-helper-default-deck "Default"
  ""
  :type 'string
  :group 'anki-helper)

(defcustom anki-helper-note-types '(("Basic" "Front" "Back")
                                    ("Basic (and reversed card)" "Front" "Back")
                                    ("Basic (optional reversed card)" "Front" "Back")
                                    ("Cloze" "Text" "Back Extra"))
  ""
  :group 'anki-helper
  :type '(repeat (list (repeat string))))

(defvar anki-helper-decks nil)

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
  ;; :: Note -> JSON
  `(("modelName" . ,(anki-helper--note-model note))
    ("fields"    . ,(anki-helper--note-fields note))))

(defun anki-helper--action-addnote (note)
  "Create an `addNote' json structure to be added to DECK with
card FRONT and BACK strings."
  (anki-helper--body
   "addNote"
   `(("note" .
      (("deckName" . ,(anki-helper--note-deck note))
       ,@(anki-helper--note-to-json note)
       ("tags" . ,(or (anki-helper--note-tags note) ""))
       ("options" .
        (("allowDuplicate" . :json-false)
         ("duplicateScope" . "deck"))))))))

(defun anki-helper--url-get-results ()
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (result))
    (goto-char (point-min))
    (search-forward-regexp "^$")
    (setq result (json-read))
    (if-let ((err (plist-get result :error)))
        (user-error "%s" err)
      (plist-get result :result))))

(defun anki-helper-create-card (front back &optional tags deck model)
  (let ((note (make-anki-helper--note
               :maybe-id nil
               :fields (list (cons (car (anki-helper--get-note-fields
                                         anki-helper-default-note-type))
                                   front)
                             (cons (cadr (anki-helper--get-note-fields
                                          anki-helper-default-note-type))
                                   back))
               :tags (or tags (split-string (read-string "Tags: ")
                                            " " t))
               :deck (or deck anki-helper-default-deck)
               :model (or model anki-helper-default-note-type)
               :orig-pos (point-marker))))
    (anki-helper--action-addnote note)))

(defvar anki-helper--cloze-counter 0)

(defun anki-helper--org-html-verbatim (verbatim _contents _info)
  (cl-incf anki-helper--cloze-counter)
  (format "{{c%d::%s}}"
          anki-helper--cloze-counter
          (org-html-encode-plain-text (org-element-property :value verbatim))))

(defun anki-helper-make-cloze (string)
  (cl-letf (((symbol-function #'org-html-verbatim) #'anki-helper--org-html-verbatim)
            (anki-helper--cloze-counter 0))
    (org-export-string-as string 'html t)))

(defun anki-helper-sync (body &optional callback note)
  (let* ((url-request-method "POST")
         (url-request-data (json-encode body)))
    (url-retrieve
     "http://127.0.0.1:8765"
     (lambda (status)
       (when callback
         (funcall callback (anki-helper--url-get-results) note))
       (kill-buffer))
     nil t)))

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
    (anki-helper-sync (anki-helper-create-card front back))))


(provide 'anki-helper)
;;; anki-helper.el ends here
