;;; -*- Mode:Common-Lisp; Package:Net; Fonts:(Cptfont Cptfontb Hl12bi); Base:10 -*-

;1;;*			1      RESTRICTED RIGHTS LEGEND*

;1;; Use, duplication, or disclosure by the Government is subject to*
;1;; restrictions as set forth in subdivision (b)(3)(ii) of the Rights in*
;1;; Technical Data and Computer Software clause at 52.227-7013.*
;1;;*
;1;;*			1TEXAS INSTRUMENTS INCORPORATED.*
;1;;*				1 P.O. BOX 2909*
;1;;*			1      AUSTIN, TEXAS 78769*
;1;;*				1    MS 2151*
;1;;*
;1;; Copyright (C) 1985, 1988 Texas Instruments Incorporated. All rights reserved.*

(DEFVAR *RESET-INITIALIZATION-LIST* NIL
  "2The list to used to reset the network*")

(ADD-INITIALIZATION "Reset controllers" '(NET:RESET-CONTROLLER-LIST) NIL '*RESET-INITIALIZATION-LIST*)