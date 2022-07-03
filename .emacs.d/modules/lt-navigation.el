;;; init.el -*- lexical-binding: t; -*-

(lt/install-package 'avy)
(lt/install-package 'hydra)
(lt/install-package 'ace-window)

(require 'hydra)
(defhydra hydra-avy (:exit t :hint nil)
  "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_C_] timed char  [_c_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
  ("C" avy-goto-char-timer)
  ("c" avy-goto-char)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

(global-set-key (kbd "C-j") #'hydra-avy/body)
(global-set-key (kbd "C-<return>") #'newline-and-indent)


(provide 'lt-navigation)
