;;; flycheck-racket-review.el --- Flycheck checker using racket-review -*- lexical-binding: t -*-


;; Authors: Bogdan Popa <bogdan@defn.io>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1") (flycheck "32"))
;; Homepage: https://github.com/Bogdanp/racket-review
;; Keywords: convenience processes tools
;; SPDX-License-Identifier: BSD-3-Clause



;;; Commentary:


;; Flycheck checker for Racket source code using racket-review.



;;; Code:


(require 'flycheck)


;;;###autoload
(flycheck-define-checker racket-review
  "Flycheck checker for Racket source code using racket-review."
  :modes racket-mode
  :command ("raco" "review" source)
  :error-patterns
  ((error
    line-start (file-name) ":" line ":" column ":error:" (message) line-end)
   (warning
    line-start (file-name) ":" line ":" column ":warning:" (message) line-end)))

;;;###autoload
(add-to-list 'flycheck-checkers 'racket-review)


(provide 'flycheck-racket-review)



;;; flycheck-racket-review.el ends here
