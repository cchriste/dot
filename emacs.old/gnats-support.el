;;;
;;; File:	gnats-support.el
;;; Author:	Robert Mecklenburg
;;; Created:	April 16, 1997
;;; 
;;; Description: Set gnats autoload stuff.
;;;               
;;; (c) Copyright 1997, Parametric Technology Corporation, all rights reserved.
;;;

(autoload 'edit-pr "gnats" "Command to edit a problem report." t)
(autoload 'view-pr "gnats" "Command to view a problem report." t)
(autoload 'unlock-pr "gnats" "Unlock a problem report." t)
(autoload 'query-pr "gnats"
	  "Command to query information about problem reports." t)
(autoload 'send-pr-mode "send-pr" "Major mode for sending problem reports." t)
(autoload 'send-pr "send-pr" "Command to create and send a problem report." t)

