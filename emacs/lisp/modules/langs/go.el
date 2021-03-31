;;; go.el --- Go module                              -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AmirrezaAskarpour

;; Author: AmirrezaAskarpour <raskarpour@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(pkg! go-mode
      :straight t
      :mode ("\\.go\\'" . go-mode)
      :hook
      (go-mode . amirreza/go-hook)
      (go-mode . amirreza/lsp)
      :bind
      (:map go-mode-map
            ("C-c m g t" . amirreza/snippet-go-tf)
            ("C-c m g h" . amirreza/snippet-go-hh)
            ("C-c m g f" . amirreza/snippet-go-for)
            ("C-c m g i" . amirreza/snippet-go-if)
            ("C-c m g p l" . amirreza/snippet-go-pl)
            ("C-c m g p f" . amirreza/snippet-go-pf))
      :config
      (defun amirreza/go-ggtags ()
        (interactive)
        (shell-command-to-string (format"gogtags -p %s" (amirreza/find-root))))
       
      (defun amirreza/go-hook ()
        (interactive)
        (amirreza/defsnippet "go" "fmain" "" "func main() {" _ \n "}")
        (amirreza/defsnippet "go" "pkgm" "Package: " "package " str \n)
        (amirreza/defsnippet "go" "pl" "" "fmt.Println(\"" _ "\")")
        (amirreza/defsnippet "go" "pf" "" "fmt.Printf(\"" _ "\")")
        (amirreza/defsnippet "go" "ifer" "" "if err != nil {" \n _ \n "}")
        (amirreza/defsnippet "go" "if" "" "if " _ "{" \n "}")
        (amirreza/defsnippet "go" "for" "" "for " _ " := range {" \n \n "}")
        (amirreza/defsnippet "go" "fn" "" "func " _ "() {" \n \n "}")
        (amirreza/defsnippet "go" "tf" "" "func " _ "(t *testing.T) {" \n \n "}")
        (amirreza/defsnippet "go" "hh" "" "func " _ "(w http.ResponseWriter, r *http.Request) {" \n \n "}")

        (define-key go-mode-map (kbd "<f5> r")
          (lambda () (interactive)
            (start-process "GoRun" "*GoRun*" "go" "run" (format "%s" buffer-file-name))))

        ;; add go binaries to exec-path
        (add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))

        ;; show lambdas instead of funcs
        (setq-local prettify-symbols-alist '(("func" . 955)))
        (add-hook 'before-save-hook (lambda ()
                                      (when (amirreza/lsp?)
                                        (lsp-format-buffer)

                                        (lsp-organize-imports))) t t)))

(pkg! go-add-tags :straight t :bind (:map go-mode-map ("C-c m s" . go-add-tags)))

(pkg! gotest :straight t 
      :after go-mode
      :config
      (define-key go-mode-map (kbd "C-c m t f") 'go-test-current-file) 
      (define-key go-mode-map (kbd "C-c m t t") 'go-test-current-test))

(provide 'modules/langs/go)
;;; go.el ends here
