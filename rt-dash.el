(require 'widget)
(require 'rt-liberation)

(eval-when-compile
  (require 'wid-edit))

;;; Customizable Variables
(defcustom rt-dash-saved-queries
  '((:name "Opened Tickets"
	   :query "Status = 'open'"))
  "A plist of saved searches to show in the dashboard"
  :type '(plist))

(custom-set-variables '(rt-dash-saved-queries
      '((:name "My Tickets"
	       :query "( Status = 'open' OR Status = 'new' ) AND Owner = 'mcenturion'"
	       :key "u")
	(:name "Today's Tickets"
	       :query "( Status = 'open' OR Status = 'new' ) AND Owner = 'mcenturion' AND LastUpdated > 'yesterday'"
	       :key "t")
	(:name "No Tomados"
	       :query "Owner = 'Nobody' AND (  Status = 'new' OR Status = 'open' ) AND Queue != 'abuse' AND Queue != 'staff_imfia' AND Queue != 'staff_iimpi' AND Queue != 'soporte_telefonia' AND Queue != 'soporte_fhce' AND Queue != 'staff_imerl'"))))

;;; rt-dash Major Mode
(defvar rt-dash-mode-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "q" 'rt-dash-bury-or-kill-this-buffer)
    map)
  "Keymap for \"rt-dash\" buffers")

(define-derived-mode rt-dash-mode read-only-mode "rt-dash"
  "major mode for rt-dashboard")

;;; rt-dashboard
(defun rt-dashboard ()
  "Create a dashboard with saved rt-liberation queries."
  (interactive)
  (switch-to-buffer "*RT Dashboard*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer)
  (remove-overlays)
  (widget-insert "RT Dashboard\n\n")
  (rt-dash-insert-search)
  (rt-dash-insert-query-buttons rt-dash-saved-queries)
  (rt-dash-add-bindings rt-dash-saved-queries)
  (widget-setup))
  (rt-dash-mode)
  )

(defun rt-dash-insert-search ()
  "Insert a search widget."
  (widget-insert "Search: ")
  (widget-create 'editable-field
		 :action (lambda (widget &rest ignore)
			   (rt-dash-search (widget-value widget))))
  (widget-insert ".")
  (put-text-property (1- (point)) (point) 'invisible t)
  (widget-insert "\n\n"))

(defun rt-dash-insert-query-buttons (queries)
  "Adds a button for every query in the queries plist"
  (dolist (q queries)
    (let ((name  (plist-get q :name))
	  (query (plist-get q :query))
	  (key   (plist-get q :key)))
      (widget-insert (format "[%d]\t" (length (rt-liber-rest-run-ls-query query))))
      (widget-create 'push-button
		     :notify `(lambda (&rest ignore)
				(rt-liber-browse-query ,query))
		     name)
      (if key
	  (widget-insert (format "(%s) \n" key))
	(widget-insert "\n")))))

(defun rt-dash-add-bindings (queries)
  "Adds bindings to rt-dash-mode-map to jump to the provided QUERIES"
  (let ((map rt-dash-mode-map))
    (dolist (q queries)
      (let ((key   (plist-get q :key))
	    (query (plist-get q :query)))
	(if key
	    (define-key map (kbd (format "j %s" key)) `(lambda () (interactive)(rt-liber-browse-query ,query))))))))

;;; Auxiliary functions
(defun rt-dash-search (term)
  "Searches for the given TERM.

If TERM is a number, show the ticket with that number.

If TERM is anything other, run a search based on subject"
  (interactive "MSearch: ")
  (if (string= term (number-to-string (string-to-number term)))
      (rt-dash-browse-ticket-by-id term)
    (rt-liber-browse-query (format "subject LIKE '%s'" term))))

(defun rt-dash-browse-ticket-by-id (id)
  "Opens the TICKET-ID ticket"
  (interactive "MTicket id: ")
  (let ((ticket-alist `((,id "."))))
    (rt-liber-display-ticket-history (car (rt-liber-rest-run-show-base-query ticket-alist)))))

(defun rt-dash-bury-or-kill-this-buffer ()
  "Undisplay the current buffer.

Bury the current buffer, unless there is only one window showing
it, in which case it is killed."
  (interactive)
  (if (> (length (get-buffer-window-list nil nil t)) 1)
      (bury-buffer)
    (kill-buffer)))

(provide 'rt-dash)
