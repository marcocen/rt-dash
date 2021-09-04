(require 'widget)
(require 'rt-liberation)

(eval-when-compile
  (require 'wid-edit))

(defcustom rt-dash-saved-queries
  '((:name "Opened Tickets"
	   :query "Status = 'open'"))
  "A plist of saved searches to show in the dashboard"
  :type '(plist))

(custom-set-variables '(rt-saved-queries
      '((:name "My Tickets"
	       :query "( Status = 'open' OR Status = 'new' ) AND Owner = 'mcenturion'")
	(:name "Todays Tickets"
	       :query "( Status = 'open' OR Status = 'new' ) AND Owner = 'mcenturion' AND LastUpdated > 'yesterday'"))))

(defvar rt-dash-mode-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "q" 'rt-dash-bury-or-kill-this-buffer)
    map)
  "Keymap for \"rt-dash\" buffers")

(defun rt-dash-mode ()
  "A mode for rt-dashboard buffers"
  (interactive)
  (kill-all-local-variables)
  (use-local-map rt-dash-mode-map)
  (setq major-mode 'rt-dash-mode
	mode-name "rt-dash"))

(defun rt-dash-bury-or-kill-this-buffer ()
  "Undisplay the current buffer.

Bury the current buffer, unless there is only one window showing
it, in which case it is killed."
  (interactive)
  (if (> (length (get-buffer-window-list nil nil t)) 1)
      (bury-buffer)
    (kill-buffer)))


(defun rt-dashboard ()
  "Create a dashboard with saved rt-liberation queries."
  (interactive)
  (switch-to-buffer "*RT Dashboard*")
  (kill-all-local-variables)
					;  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer)
  (remove-overlays)
  (rt-dash-mode)
  (widget-insert "RT Dashboard\n\n")
  (rt-dash-insert-search)
  (rt-dash-insert-query-buttons rt-dash-saved-queries)
  (use-local-map widget-keymap)
  (widget-setup)))

(defun rt-dash-insert-search ()
  "Insert a search widget."
  (widget-insert "Search: ")
  (widget-create 'editable-field
		 ;; Leave some space at the start and end of the
		 ;; search boxes.

		 ;;:size (length "Search: ")
		 :action (lambda (widget &rest ignore)
			   (rt-dash-search (widget-value widget))))
  ;; Add an invisible dot to make `widget-end-of-line' ignore
  ;; trailing spaces in the search widget field.  A dot is used
  ;; instead of a space to make `show-trailing-whitespace'
  ;; happy, i.e. avoid it marking the whole line as trailing
  ;; spaces.
  (widget-insert ".")
  (put-text-property (1- (point)) (point) 'invisible t)
  (widget-insert "\n\n"))

(defun rt-dash-search (term)
  "Searches for the given TERM.
If TERM is a number, show the ticket with that number.

If TERM is anything other, run a search based on subject"
  (if (string= term (number-to-string (string-to-number term)))
      (rt-dash-browse-ticket-by-id term)
    (rt-liber-browse-query (format "subject LIKE '%s'" term))))

(defun rt-dash-insert-query-buttons (queries)
  "Adds a button for every query in the queries plist"
  (dolist (q queries)
    (let ((name (plist-get q :name))
	  (query (plist-get q :query)))
      (widget-insert (format "[%d] " (length (rt-liber-rest-run-ls-query query))))
      (widget-create 'push-button
		     :notify `(lambda (&rest ignore)
				(rt-liber-browse-query ,query))
		     name)
      (widget-insert "\n"))))




(defun rt-dash-browse-ticket-by-id (id)
  "Opens the TICKET-ID ticket"
  (interactive "MTicket id: ")
  (let ((ticket-alist `((,id "."))))
  (rt-liber-display-ticket-history (car (rt-liber-rest-run-show-base-query ticket-alist)))))
