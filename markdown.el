;; Copy-modified from
;; https://github.com/syl20bnr/spacemacs/blob/488e2e3b665365a668ab528014ea7a21a24dd860/layers/%2Blang/markdown/packages.el#L45
(use-package markdown-mode
  :mode ("\\.m[k]d" . markdown-mode)
  :defer t)

;; Copy-modified from
;; https://github.com/syl20bnr/spacemacs/blob/488e2e3b665365a668ab528014ea7a21a24dd860/layers/%2Blang/markdown/packages.el#L163
;;
;; FIXME: https://github.com/raxod502/straight.el/issues/305
;;
;; (use-package mmm-mode
;;    :commands mmm-mode
;;    :init (add-hook 'markdown-mode-hook 'spacemacs/activate-mmm-mode)
;;    :config
;;    (progn
;;      (spacemacs|hide-lighter mmm-mode)
;;      (mmm-add-classes '((markdown-ini
;; 			 :submode conf-unix-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```ini[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-python
;; 			 :submode python-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```python[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-html
;; 			 :submode web-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```html[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-java
;; 			 :submode java-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```java[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-ruby
;; 			 :submode ruby-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```ruby[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-c
;; 			 :submode c-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```c[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-c++
;; 			 :submode c++-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```c\+\+[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-elisp
;; 			 :submode emacs-lisp-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```elisp[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-javascript
;; 			 :submode javascript-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```javascript[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-ess
;; 			 :submode R-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```{?r.*}?[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-classes '((markdown-rust
;; 			 :submode rust-mode
;; 			 :face mmm-declaration-submode-face
;; 			 :front "^```rust[\n\r]+"
;; 			 :back "^```$")))
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-javascript)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ess)
;;      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-rust)))


;; TODO:
;;
;; * https://github.com/syl20bnr/spacemacs/blob/488e2e3b665365a668ab528014ea7a21a24dd860/layers/%2Blang/markdown/packages.el#L237
;;
