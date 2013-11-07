;;; wiktionary-translate.el --- Translate words using wiktionary

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: convenience
;; Version: 0.1
;; Created: 7th November 2013
;; Package-requires: ((dash "2.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Translate words using wiktionary. Right now, only italian is
;; supported.

;; It's very much a work in progress, please heed that warning.

;;; Code:

(require 'dash)

;;;_* Start
(defconst wd-base-query
  "http://en.wiktionary.org/w/api.php?action=query&format=xml&prop=revisions&rvprop=content&titles="
  "Append the word to this query to retrieve the data.")

;;;_* UI

;;;###autoload
(defun wd-show-translation (word)
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Word: " (or (word-at-point) ""))
                       (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end))
                         (or (word-at-point)
                             (read-from-minibuffer "Word: "))))))
  (-if-let (meaning (wd-translate-word word))
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*Wiktionary*")
         (if (fboundp 'read-only-mode)
             (read-only-mode -1)
           (setq buffer-read-only nil))
         (erase-buffer)
         (insert meaning)
         (special-mode)
         (goto-char (point-min))
         (current-buffer)))
    (message "Failed to find a translation")))

(defun wd-show-raw-translation (word)
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Word: " (or (word-at-point) ""))
                       (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end))
                         (or (word-at-point)
                             (read-from-minibuffer "Word: "))))))
  (-if-let (raw-data (wd-get-raw-page-data word))
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*Wiktionary*")
         (if (fboundp 'read-only-mode)
             (read-only-mode -1)
           (setq buffer-read-only nil))
         (erase-buffer)
         (insert raw-data)
         (special-mode)
         (goto-char (point-min))
         (current-buffer)))
    (message "Failed to find a translation")))

;;;_* helpers
(defun wd-get-page-text (buffer)
  "Get the text from the wiktionary dump."
  (with-current-buffer buffer
    ;; old export query
    (caddr (nth 2 (caddr (caddr (caddr (caddr (libxml-parse-xml-region (point-min) (point-max))))))))))

(defun wd-query-for-word (word)
  (let ((data (url-retrieve-synchronously (concat wd-base-query word))))
    (with-current-buffer data
      (goto-char (point-min))
      (search-forward "<?xml version=\"1.0\"?>")
      (delete-region (point-min) (point)))
    data))

(defun wd-translate-word (word)
  (-when-let (meaning (wd-process-word word))
    (concat "Translations of " word "\n"
            "===============\n\n"
            (wd-sanitize-translation (wd-dewikify-markup meaning)))))

(defun wd-get-raw-page-data (word)
  (-when-let* ((raw-xml-buffer (wd-query-for-word word))
               (text (wd-get-page-text raw-xml-buffer)))
    text))

(defun wd-process-word (word &optional extra)
  (-if-let (text (wd-get-raw-page-data word))
      (with-temp-buffer
        (insert text)
        (let ((ita (wd-extract-language "Italian"))
              (extra-verb (or (plist-get extra :verb) ""))
              (extra-nominal (or (plist-get extra :nominal) "")))
          (when ita
            (concat (--if-let (wd-process-italian-verb ita) (concat extra-verb "[" word "] " it) "")
                    (--if-let (wd-process-italian-noun ita) (concat extra-nominal "[" word "] " it) "")
                    (--if-let (wd-process-italian-adjective ita) (concat extra-nominal "[" word "] " it) "")
                    (wd-process-italian-invar ita)))))
    ;; try to run the decapitalized version of the word
    (if (not (equal (downcase word) word))
        (wd-process-word (downcase word))
      (message "Failed to retrieve the page"))))

(defun wd-extract-language (language &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (when (search-forward (concat "==" language "==") nil t)
      (let ((start (line-beginning-position)))
        (if (re-search-forward "^==[^=].?*[^=]==$" nil t)
            (buffer-substring-no-properties start (line-beginning-position))
          (buffer-substring-no-properties start (point-max)))))))

(defun wd-dewikify-markup (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (save-excursion (replace-regexp "\\[\\[\\(.*?\\)\\]\\]" "\\1"))
    (save-excursion (replace-regexp "{{context|\\(.*?\\)|\\(.*?\\)|lang=.*?}}" "(Context: \\1 \\2)"))
    (save-excursion (replace-regexp "{{context|\\(.*?\\)|lang=.*?}}" "(Context: \\1)"))
    (save-excursion (replace-regexp "{{qualifier|\\(.*?\\)}}" "(Qualifier: \\1)"))
    (buffer-string)))

(defun wd-sanitize-translation (text)
  ;; "feminine of ... verb" happens when there is an adjective same as
  ;; verb form in some finite form.
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward "feminine of .*?verb:" nil t)
        (beginning-of-line)
        (let (kill-ring) (kill-paragraph 1))
        (delete-blank-lines)))
    (buffer-string)))

;;;_* Italian
(defun wd-process-italian-meanings ()
  (when (search-forward "#" nil t)
    (beginning-of-line)
    (let ((text "") (i 1) done)
      (while (not done)
        ;; process a sub-header
        (if (looking-at "#\\(:+\\)")
            (let ((depth (length (match-string 0))))
              (forward-char depth)
              (setq text (concat
                          text
                          (make-string (* 4 (1- depth)) ?\ )
                          (buffer-substring-no-properties (point) (line-end-position)) "\n"))
              (forward-line 1)
              (setq done (not (looking-at "#"))))
          (forward-char 2)
          (setq text (concat text (int-to-string i) ". " (buffer-substring-no-properties (point) (line-end-position)) "\n"))
          (forward-line 1)
          (setq done (not (looking-at "#")))
          (setq i (1+ i))))
      text)))

;;;_** Verbs
(defun wd-process-italian-verb (text)
  (with-temp-buffer
    (insert text)
    (wd--process-italian-verb)))

(defun wd--process-italian-verb ()
  (goto-char (point-min))
  (when (re-search-forward (regexp-opt '("===Verb==="
                                         "===Verb form===")) nil t)
    (save-restriction
      ;;(--when-let (re-search-forward "^===[^=]" nil t) (narrow-to-region (point) it))
      (forward-line 1)
      ;; first test if the page contains "conjugation" info
      (let (start)
        (cond
         ;; # {{conjugation of|togliere||1|s|pres|ind|lang=it}} ''tolgo''
         ((setq start (save-excursion (search-forward "conjugation of|" nil t)))
          (goto-char start)
          (let ((what (save-excursion
                        (buffer-substring-no-properties
                         (progn
                           (re-search-forward "[123]" nil t)
                           (backward-char 1)
                           (point))
                         (progn
                           (search-forward "|lang" nil t)
                           (backward-char 5)
                           (point))))))
            (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                             (list :verb (concat what " of ")))))
         ;; # {{form of|first-, second- and third-person singular subjunctive present tense|essere|lang=it}}
         ((setq start (save-excursion (search-forward "{{form of|" nil t)))
          (goto-char start)
          (let ((what (buffer-substring-no-properties
                       (point)
                       (1- (search-forward "|")))))
            (wd-process-word (buffer-substring-no-properties
                              (point)
                              (1- (search-forward "|")))
                             (list :verb (concat what " of ")))))
         ;; # ''first-, second-person singular subjunctive imperfect of [[amare]]''
         ((setq start (save-excursion (re-search-forward (regexp-opt '("-person singular"
                                                                       "-person plural")) nil t)))
          (goto-char start)
          (let ((what (buffer-substring-no-properties
                       (+ (save-excursion (search-backward "''" nil t)) 2)
                       (- (save-excursion (search-forward " of " nil t)) 3))))
            (when (search-forward " of [[" nil t)
              (let ((s (point)))
                (when (search-forward "]]" nil t)
                  (forward-char -2)
                  (wd-process-word (buffer-substring-no-properties s (point))
                                   (list :verb (concat what " of "))))))))
         ;; past participle {{past participle of|subordinare|lang=it}}
         ((setq start (save-excursion (search-forward "past participle of|" nil t)))
          (goto-char start)
          (let ((parent (buffer-substring-no-properties start (1- (search-forward "|" nil t)))))
            (wd-process-word parent (list :verb "past participle of "))))
         ;; # {{gerund of|lasciare|lang=it}}
         ((setq start (save-excursion (search-forward "gerund of|" nil t)))
          (goto-char start)
          (let ((parent (buffer-substring-no-properties start (1- (search-forward "|" nil t)))))
            (wd-process-word parent (list :verb "gerund of "))))
         ;; # [[past participle]] of [[andare]]
         ((setq start (save-excursion (search-forward "[[past participle]] of" nil t)))
          (goto-char start)
          (let ((parent (buffer-substring-no-properties (search-forward "[[") (- (search-forward "]]") 2))))
            (wd-process-word parent (list :verb "past participle of "))))
         ;; feminine form of pp {{feminine of|subordinare|lang=it}}
         ((setq start (save-excursion (search-forward "feminine of|" nil t)))
          (goto-char start)
          (let ((parent (buffer-substring-no-properties start (1- (search-forward "|" nil t)))))
            (wd-process-word parent (list :verb "feminine form of "))))
         ;; normal definition
         (t (concat "verb:\n" (wd-process-italian-meanings) "\n")))))))

;;;_** Nominals
(defun wd-process-italian-noun (text)
  (with-temp-buffer
    (insert text)
    (wd--process-italian-nominal (regexp-opt '("===Noun==="
                                               "===Noun form==="))
                                 "noun")))

(defun wd-process-italian-adjective (text)
  (with-temp-buffer
    (insert text)
    (wd--process-italian-nominal (regexp-opt '("===Adjective==="
                                               "===Adjective form==="))
                                 "adjective")))

(defun wd--process-italian-nominal (category-regexp pos-name)
  (goto-char (point-min))
  (when (re-search-forward category-regexp nil t)
    (forward-line 1)
    (let (start)
      (cond
       ;; plural form # {{plural of|cavallo|lang=it}}
       ((setq start (save-excursion (search-forward "plural of|" nil t)))
        (goto-char start)
        (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                         (list :nominal "plural of ")))
       ;; plural form 2 # Plural form of [[compito]]
       ((setq start (save-excursion (search-forward "Plural form of [[" nil t)))
        (goto-char start)
        (wd-process-word (buffer-substring-no-properties
                          start
                          (- (search-forward "]]" nil t) 2))
                         (list :nominal "plural of ")))
       ;; feminine form of # {{feminine of|compito|lang=it}}
       ((setq start (save-excursion (search-forward "feminine of|" nil t)))
        (goto-char start)
        (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t))) (list :nominal "feminine of ") ))
       ;; normal definition
       (t (concat pos-name ":\n" (wd-process-italian-meanings) "\n"))))))

;;;_** Invariable: conjunctions, prepositions, adverbs
(defun wd-process-italian-invar (text)
  (with-temp-buffer
    (insert text)
    (or (wd--process-italian-invar) "")))

(defun wd--process-italian-invar ()
  (let ((re ""))
    (goto-char (point-min))
    (when (search-forward "===Adverb===" nil t)
      (setq re (concat "Adverb:\n" (wd-process-italian-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Preposition===" nil t)
      (setq re (concat "Preposition:\n" (wd-process-italian-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Interjection===" nil t)
      (setq re (concat "Interjection:\n" (wd-process-italian-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Conjunction===" nil t)
      (setq re (concat "Conjunction:\n" (wd-process-italian-meanings) "\n" re)))
    re))

(provide 'wiktionary-translate)

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: ";;;_\\([*]\\)+"
;; End:

;;; wiktionary-translate.el ends here
