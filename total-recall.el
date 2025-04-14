;;; total-recall.el --- Spaced repetition system -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Pierre-Henry FRÖHRING
;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Maintainer: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Homepage: https://github.com/phf-1/total-recall
;; Package-Version: 0.2
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides `total-recall'.
;; 
;; The command `M-x total-recall' uses Ripgrep to search for Org files in
;; the directory specified by `total-recall-root-dir' that contain
;; exercises. For each file found, it lists the exercises and presents a
;; user interface to display them.
;; 
;; For each exercise, it first shows the question, followed by the
;; answer. The user's performance — whether they provided a correct
;; answer — is recorded and stored in an SQLite database at
;; `total-recall-database'. This data determines whether an exercise
;; should be reviewed sooner or later.
;; 
;; An exercise is defined as any heading in an Org file that meets the
;; following criteria:
;; - It has a `TYPE' property with the value `total-recall-type-id'.
;; - It has an `ID' property with a UUID value.
;; - It contains two subheadings:
;;   - The first subheading encodes a question.
;;   - The second subheading encodes an answer.
;; - The file is located under `total-recall-root-dir'.
;; 
;; Example of an exercise:
;; 
;; * Emacs
;; :PROPERTIES: 
;; :TYPE: b0d53cd4-ad89-4333-9ef1-4d9e0995a4d8
;; :ID: ced2b42b-bfba-4af5-913c-9d903ac78433
;; :END:
;; 
;; ** What is GNU Emacs?
;; 
;; [optional content]
;; 
;; ** Answer
;; 
;; An extensible, customizable, free/libre text editor — and more.  At
;; its core is an interpreter for Emacs Lisp, a dialect of the Lisp
;; programming language with extensions to support text editing.
;;
;;; Code:

;; Dependencies

(unless (sqlite-available-p)
  (error "Emacs must be compiled with built-in support for SQLite databases"))
(require 'org)
(require 'time-date)
(require 'parse-time)

;; Configuration

(defgroup total-recall nil
  "Customization options for Total Recall.
This package provides `total-recall' for spaced repetition in Emacs."
  :group 'convenience
  :prefix "total-recall-")

(defcustom total-recall-database (file-name-concat user-emacs-directory "total-recall.sqlite3")
  "Path to the SQLite database for storing exercise data."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-ripgrep-cmd "rg"
  "Name or path of the Ripgrep executable."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-root-dir (expand-file-name "~")
  "Root directory where Ripgrep searches for Org files."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-type-id "b0d53cd4-ad89-4333-9ef1-4d9e0995a4d8"
  "Type ID for Org headings representing exercises."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-window-width 160
  "Width of the Total Recall UI in characters."
  :type 'integer
  :group 'total-recall)

(defcustom total-recall-window-height 90
  "Height of the Total Recall UI in characters."
  :type 'integer
  :group 'total-recall)

;; Utils

(defun total-recall--time-to-iso8601 (time)
  "Convert TIME to an ISO 8601 formatted string.
TIME is a Lisp timestamp. Returns a string in the format YYYY-MM-DDTHH:MM:SSZ."
  (format-time-string "%FT%TZ" (time-convert time 'list) t))

(defun total-recall--iso8601-to-time (iso8601)
  "Convert ISO8601 string to a Lisp timestamp.
ISO8601 is a string in ISO 8601 format. Returns a Lisp timestamp."
  (parse-iso8601-time-string iso8601))

(defun total-recall--time-init ()
  "Return a Lisp timestamp for January 1, 1970, 00:00:00 UTC."
  (encode-time 0 0 0 1 1 1970 0))

;; Search

(defun total-recall--search (dir ext type-id)
  "Search for files containing TYPE-ID with extension EXT in directory DIR.
DIR is a string path to the directory.
EXT is a string file extension (e.g., \"org\").
TYPE-ID is a string identifier to search for.
Returns a list of file paths."
  (let ((cmd (format "%s -g '*.%s' -i --no-heading -n --color=never '%s' %s"
                     total-recall-ripgrep-cmd ext type-id dir))
        matches)
    (with-temp-buffer
      (call-process-shell-command cmd nil `(,(current-buffer) nil) nil)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (match (split-string line ":")))
          (push (car match) matches))
        (forward-line 1))
      (delete-dups matches))))

;; Measure

(defun total-recall--measure-mk (id time)
  "Build a measure that records ID and TIME.
ID is a string identifier.
TIME is a Lisp timestamp."
  (record 'total-recall-measure id time))

(defun total-recall--measure-p (measure)
  "Return t if MEASURE is a valid measure structure, else nil."
  (memq (type-of measure)
        '(total-recall-measure
          total-recall-measure-success
          total-recall-measure-failure
          total-recall-measure-skip)))

(defun total-recall--measure-id (measure)
  "Return the ID of MEASURE."
  (total-recall--measure-rcv measure :id))

(defun total-recall--measure-time (measure)
  "Return the time of MEASURE."
  (total-recall--measure-rcv measure :time))

(defun total-recall--measure-rcv (measure msg)
  "Implement the MEASURE interface selected by MSG."
    (pcase msg
      (:id (aref measure 1))
      (:time (aref measure 2))))

(defun total-recall--success-measure-mk (id time)
  "Build a success measure that records ID and TIME."
  (record 'total-recall-measure-success id time))

(defun total-recall--success-measure-p (measure)
  "Return t if MEASURE is a success measure, else nil."
  (eq (type-of measure) 'total-recall-measure-success))

(defun total-recall--failure-measure-mk (id time)
  "Build a failure measure that records ID and TIME."
  (record 'total-recall-measure-failure id time))

(defun total-recall--failure-measure-p (measure)
  "Return t if MEASURE is a failure measure, else nil."
  (eq (type-of measure) 'total-recall-measure-failure))

(defun total-recall--skip-measure-mk (id time)
  "Build a skip measure that records ID and TIME."
  (record 'total-recall-measure-skip id time))

(defun total-recall--skip-measure-p (measure)
  "Return t if MEASURE is a skip measure, else nil."
  (eq (type-of measure) 'total-recall-measure-skip))

;; UI

(defun total-recall--ui-mk ()
  "Build the Total Recall UI."
  (let ((frame (make-frame `((width . ,total-recall-window-width)
                             (height . ,total-recall-window-height))))
        (buffer (get-buffer-create "*total-recall*")))
    (record 'total-recall-ui buffer frame :state)))

(defun total-recall--ui-p (ui)
  "Return t if UI is a valid UI structure, else nil."
  (eq (type-of ui) 'total-recall-ui))

(defun total-recall--ui-set-state (ui state)
  "Set the state of UI to STATE and return UI."
  (aset ui 3 state)
  ui)

(defun total-recall--ui-init (ui)
  "Initialize UI."
  (total-recall--ui-rcv ui :init))

(defun total-recall--ui-no-exercises (ui)
  "Display a /no exercises/ message in UI."
  (total-recall--ui-rcv ui :no-exercises))

(defun total-recall--ui-display-question (ui id subject question)
  "Display QUESTION identified by ID about SUBJECT in UI.
QUESTION is a string.
SUBJECT is a string."
  (total-recall--ui-rcv ui `(:display :question ,id ,subject ,question)))

(defun total-recall--ui-display-answer (ui answer)
  "Display ANSWER in UI.
ANSWER is a string."
  (total-recall--ui-rcv ui `(:display :answer ,answer)))

(defun total-recall--ui-state (ui)
  "Return the state of UI."
  (total-recall--ui-rcv ui :state))

(defun total-recall--ui-kill (ui)
  "Close UI."
  (total-recall--ui-rcv ui :kill))

(defun total-recall--ui-rcv (ui msg)
  "Implement the UI API selected by MSG."
  (unless (total-recall--ui-p ui) (error "Not a UI structure"))
  (let ((buffer (aref ui 1))
        (frame (aref ui 2))
        (state (aref ui 3)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'org-mode) (org-mode))
    (pcase msg
      (:init
       (erase-buffer)
       (insert "* Total Recall *\n\n\n")
       (goto-char (point-min))
       (total-recall--ui-set-state ui :init))

      (:no-exercises
       (save-window-excursion
         (unless (eq (total-recall--ui-state ui) :init)
           (total-recall--ui-rcv ui :init))
         (goto-char (point-max))
         (insert "No exercises found.\n")
         (goto-char (point-min)))
       (total-recall--ui-set-state ui :no-exercises))

      (`(:display :question ,id ,subject ,question)
       (unless (eq (total-recall--ui-state ui) :init)
         (total-recall--ui-rcv ui :init))
       (goto-char (point-max))
       (insert (format "[[ref:%s][%s]]\n\n\n" id subject))
       (insert (format "%s\n\n\n" question))
       (goto-char (point-min))
       (total-recall--ui-set-state ui :question))

      (`(:display :answer ,answer)
       (unless (eq (total-recall--ui-state ui) :question)
         (error "UI state is not :question"))
       (goto-char (point-max))
       (insert (format "%s\n\n\n" answer))
       (goto-char (point-min))
       (total-recall--ui-set-state ui :answer))

      (:kill
       (kill-buffer buffer)
       (delete-frame frame)
       (total-recall--ui-set-state ui :dead))

      (:state state))))

;; DB

(defun total-recall--db-mk (path)
  "Open an SQLite database at PATH.
PATH is a string file path. Returns an SQLite database handle."
  (sqlite-open path))

(defun total-recall--db-p (x)
  "Return t if X is an SQLite database handle, else nil."
  (sqlitep x))

(defun total-recall--db-save (db measure)
  "Save MEASURE to database DB.
DB is an SQLite database handle. MEASURE is a measure structure. Returns t."
  (total-recall--db-rcv db `(:save ,measure)))

(defun total-recall--db-select (db id)
  "Retrieve measures for exercise ID from database DB.
DB is an SQLite database handle. ID is a string exercise identifier.
Returns a list of measure structures."
  (total-recall--db-rcv db `(:select :measures ,id)))

(defun total-recall--db-close (db)
  "Close database DB.
DB is an SQLite database handle. Returns t."
  (total-recall--db-rcv db :close))

(defun total-recall--db-rcv (db msg)
  "Handle MSG for SQLite database DB.
DB is an SQLite database handle.
Returns the result of the operation."
  (unless (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table' AND name='exercise_log'")
    (sqlite-execute db
                    "CREATE TABLE exercise_log (
                       type TEXT NOT NULL,
                       id TEXT NOT NULL,
                       time TEXT NOT NULL)"))

  (pcase msg
    (`(:measure-to-row ,measure)
     (pcase measure
       ((pred total-recall--measure-p)
        (let ((type
               (cond
                ((total-recall--success-measure-p measure) "success")
                ((total-recall--failure-measure-p measure) "failure")))
              (id (total-recall--measure-id measure))
              (time (total-recall--time-to-iso8601 (total-recall--measure-time measure))))
          (list type id time)))
       (_ (error "MEASURE is not a Measure. %S" measure))))

    (`(:row-to-measure ,row)
     (pcase row
       (`(,type ,id ,time)
        (pcase type
          ("success" (total-recall--success-measure-mk id (total-recall--iso8601-to-time time)))
          ("failure" (total-recall--failure-measure-mk id (total-recall--iso8601-to-time time)))))))

    (`(:save ,measure)
     (pcase measure
       ((pred total-recall--measure-p)
        (sqlite-execute
         db
         "INSERT INTO exercise_log (type, id, time) VALUES (?, ?, ?)"
         (total-recall--db-rcv db `(:measure-to-row ,measure)))
        t)
       (_ (error "Unexpected value: %S" measure))))

    (`(:select :measures ,id)
     (let (rows)
       (setq rows
             (sqlite-select
              db
              "SELECT type, id, time FROM exercise_log WHERE id = ? ORDER BY time ASC"
              (list id)))
       (mapcar
        (lambda (row) (total-recall--db-rcv db `(:row-to-measure ,row)))
        rows)))

    (:close
     (sqlite-close db)
     t)

    (_ (error "Unknown message: %S" msg))))

;; Exercise

(defun total-recall--exercise-mk (subject id question answer)
  "Create an exercise with SUBJECT, ID, QUESTION, and ANSWER.
SUBJECT, ID, QUESTION, and ANSWER are strings. Signals an error if any argument
is not a string. Returns an exercise structure."
  (unless (stringp subject) (error "Subject is not a string"))
  (unless (stringp id) (error "ID is not a string"))
  (unless (stringp question) (error "Question is not a string"))
  (unless (stringp answer) (error "Answer is not a string"))
  (record 'total-recall-exercise subject id question answer))

(defun total-recall--exercise-p (ex)
  "Return t if EX is an exercise structure, else nil."
  (eq (type-of ex) 'total-recall-exercise))

(defun total-recall--exercise-subject (exercise)
  "Return the subject of EXERCISE.
EXERCISE is an exercise structure. Returns a string."
  (total-recall--exercise-rcv exercise :subject))

(defun total-recall--exercise-id (exercise)
  "Return the ID of EXERCISE.
EXERCISE is an exercise structure. Returns a string."
  (total-recall--exercise-rcv exercise :id))

(defun total-recall--exercise-question (exercise)
  "Return the question of EXERCISE.
EXERCISE is an exercise structure. Returns a string."
  (total-recall--exercise-rcv exercise :question))

(defun total-recall--exercise-answer (exercise)
  "Return the answer of EXERCISE.
EXERCISE is an exercise structure. Returns a string."
  (total-recall--exercise-rcv exercise :answer))

(defun total-recall--exercise-scheduled (exercise db)
  "Return the scheduled review time for EXERCISE using database DB.
EXERCISE is an exercise structure. DB is an SQLite database handle.
Returns a Lisp timestamp."
  (total-recall--exercise-rcv exercise `(:scheduled ,db)))

(defun total-recall--exercise-rcv (exercise msg)
  "Handle MSG for EXERCISE.
EXERCISE is an exercise structure. MSG can be :subject, :id, :question, :answer,
or (:scheduled DB). Returns the corresponding value (e.g., string or timestamp)."
  (let ((subject (aref exercise 1))
        (id (aref exercise 2))
        (question (aref exercise 3))
        (answer (aref exercise 4)))

    (pcase msg
      (:subject subject)

      (:id id)

      (:question question)

      (:answer answer)

      (`(:scheduled ,db)
       (let (measures (last-failure-index -1) nbr last-success-time)
         (setq measures (total-recall--db-select db id))

         (let ((i -1))
           (dolist (measure measures)
             (setq i (+ i 1))
             (when (total-recall--failure-measure-p measure)
               (setq last-failure-index i))))

         (setq nbr
               (if (< last-failure-index 0)
                   (length measures)
                 (- (length measures) (1+ last-failure-index))))

         (setq last-success-time
               (when (> nbr 0)
                 (let ((last-measure (nth (1- (length measures)) measures)))
                   (if (total-recall--success-measure-p last-measure)
                       (total-recall--measure-time last-measure)
                     (error "Last measure is not a success despite NBR > 0")))))

         (if (zerop nbr)
             (total-recall--time-init)
           (let* ((delta-days (expt 2 (- nbr 1)))
                  (delta-secs (* delta-days 24 60 60))
                  (t-secs (time-to-seconds last-success-time))
                  (result-secs (+ t-secs delta-secs)))
             (seconds-to-time result-secs))))))))

;; Filesystem

(defun total-recall--fs-list-exercises (path)
  "List exercises in PATH.
PATH is a string file or directory path. Returns a list of exercise structures."
  (total-recall--fs-rcv path :list-exercises))

(defun total-recall--fs-rcv (path msg)
  "Handle MSG for PATH.
PATH is a string file or directory path. MSG is a symbol like :list-exercises.
Delegates to directory or file handlers. Returns the handler’s result."
  (cond
   ((file-directory-p path)
    (total-recall--dir-rcv path msg))
   ((file-exists-p path)
    (total-recall--file-rcv path msg))))

(defun total-recall--dir-list-exercises (dir)
  "List exercises in Org files under directory DIR.
DIR is a string directory path. Returns a list of exercise structures."
  (total-recall--dir-rcv dir :list-exercises))

(defun total-recall--dir-rcv (dir msg)
  "Handle MSG for directory DIR.
DIR is a string directory path. MSG is a symbol like :list-exercises.
Returns a list of exercise structures for :list-exercises."
  (pcase msg
    (:list-exercises
     (mapcan
      (lambda (file-path) (total-recall--file-rcv file-path :list-exercises))
      (total-recall--search dir "org" total-recall-type-id)))))

(defun total-recall--file-list-exercises (file)
  "List exercises in Org file FILE.
FILE is a string file path. Returns a list of exercise structures."
  (total-recall--file-rcv file :list-exercises))

(defun total-recall--file-rcv (file msg)
  "Handle MSG for Org file FILE.
FILE is a string file path. MSG is a symbol like :list-exercises.
Returns a list of exercise structures for :list-exercises."
  (pcase msg
    (:list-exercises
     (with-temp-buffer
       (insert-file-contents file)
       (org-mode)
       (org-fold-show-all)
       (let ((org-element-use-cache nil)
             (exercises '()))
         (org-map-entries
          (lambda ()
            (let ((id (org-entry-get nil "ID"))
                  (subject (org-format-outline-path (org-get-outline-path t) 10000))
                  question answer)
              (save-restriction
                (org-narrow-to-subtree)
                (org-next-visible-heading 1)
                (unless (org-at-heading-p) (error "Question not found"))
                (save-restriction
                  (org-narrow-to-subtree)
                  (let ((init-lvl (org-current-level)))
                    (while (> (org-current-level) 1) (org-promote-subtree))
                    (org-mark-subtree)
                    (setq question
                          (string-trim
                           (buffer-substring-no-properties (point) (mark))))
                    (while (< (org-current-level) init-lvl) (org-demote-subtree))))
                (org-goto-sibling)
                (unless (org-at-heading-p) (error "Answer not found"))
                (save-restriction
                  (org-narrow-to-subtree)
                  (let ((init-lvl (org-current-level)))
                    (while (> (org-current-level) 1) (org-promote-subtree))
                    (org-mark-subtree)
                    (setq answer
                          (string-trim
                           (buffer-substring-no-properties (point) (mark))))
                    (while (< (org-current-level) init-lvl) (org-demote-subtree))))
                (push (total-recall--exercise-mk subject id question answer) exercises))))
          (format "TYPE=\"%s\"" total-recall-type-id))
         (reverse exercises))))))

;; total-recall

;;;###autoload
(defun total-recall ()
  "Provide spaced repetitions capabilities to Emacs.

This package provides `total-recall'.

The command `M-x total-recall' uses Ripgrep to search for Org files in
the directory specified by `total-recall-root-dir' that contain
exercises. For each file found, it lists the exercises and presents a
user interface to display them.

For each exercise, it first shows the question, followed by the
answer. The user's performance — whether they provided a correct
answer — is recorded and stored in an SQLite database at
`total-recall-database'. This data determines whether an exercise
should be reviewed sooner or later.

An exercise is defined as any heading in an Org file that meets the
following criteria:
- It has a `TYPE' property with the value `total-recall-type-id'.
- It has an `ID' property with a UUID value.
- It contains two subheadings:
  - The first subheading encodes a question.
  - The second subheading encodes an answer.
- The file is located under `total-recall-root-dir'.

Example of an exercise:

* Emacs
:PROPERTIES: 
:TYPE: b0d53cd4-ad89-4333-9ef1-4d9e0995a4d8
:ID: ced2b42b-bfba-4af5-913c-9d903ac78433
:END:

** What is GNU Emacs?

[optional content]

** Answer

An extensible, customizable, free/libre text editor — and more.  At
its core is an interpreter for Emacs Lisp, a dialect of the Lisp
programming language with extensions to support text editing."
  (interactive)

  (unless (executable-find total-recall-ripgrep-cmd)
    (user-error "Ripgrep (rg) is not installed. Please install it to use this package"))

  (let ((exercises (total-recall--fs-list-exercises total-recall-root-dir))
        (db (total-recall--db-mk total-recall-database))
        (ui (total-recall--ui-mk))
        (use-dialog-box nil)
        exercise
        scheduled
        choice)
    (total-recall--ui-init ui)
    (if (null exercises)
        (total-recall--ui-no-exercises ui)
      (while exercises
        (setq exercise (pop exercises))
        (setq scheduled (total-recall--exercise-scheduled exercise db))
        (when (time-less-p scheduled (current-time))
          (total-recall--ui-display-question
           ui
           (total-recall--exercise-id exercise)
           (total-recall--exercise-subject exercise)
           (total-recall--exercise-question exercise))
          (setq choice
                (car
                 (read-multiple-choice
                  "What would you like to do?"
                  '((?r "Reveal answer" "Display the answer to the question")
                    (?s "Skip" "Skip this exercise")
                    (?q "Quit" "Quit Total Recall")))))
          (pcase choice
            (?r
             (total-recall--ui-display-answer ui (total-recall--exercise-answer exercise))
             (setq choice
                   (car
                    (read-multiple-choice
                     "What would you like to do?"
                     '((?s "Success" "You successfully answered the question")
                       (?f "Failure" "You failed to answer the question")
                       (?q "Quit" "Quit Total Recall")))))
             (pcase choice
               (?s
                (total-recall--db-save db (total-recall--success-measure-mk (total-recall--exercise-id exercise) (current-time))))
               (?f
                (total-recall--db-save db (total-recall--failure-measure-mk (total-recall--exercise-id exercise) (current-time))))
               (?q
                (setq exercises nil))))
            (?s
             nil)
            (?q
             (setq exercises nil))))))
    (total-recall--db-close db)
    (total-recall--ui-kill ui)))

(provide 'total-recall)

;;; total-recall.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
