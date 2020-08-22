;;; INIT-WORK --- for working
;;
;; Author: ZhiCheng Lee <gccll.love@gmail.com>
;; Copyright © 2020, ZhiCheng Lee, all rights reserved.
;; Created: 21 August 2020
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;----------------------------------------------------------------------------
;; `habitica', todo tool
;;----------------------------------------------------------------------------
(use-package habitica
	:config
	(setq habitica-uid "97a330f5-ac2e-4630-a870-3324763faec9")
	(setq habitica-token "7abacc62-88f0-48a4-85dd-0275e63d54c4")
	(setq habitica-show-completed-todo t)
	;; (setq habitica-turn-on-highlighting t)
	(setq habitica-show-streak t))
;; -END



(provide 'init-work)
;;; init-work.el ends here
