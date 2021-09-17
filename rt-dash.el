(require 'widget)
(require 'rt-liberation)
(require 'magit)

(eval-when-compile
  (require 'wid-edit))

;;; Customizable Variables
(defcustom rt-dash-saved-queries
  '((:name "My Opened Tickets"
	   :query "Status = 'open' and Owner = 'mcenturion'"))
  "A plist of saved searches to show in the dashboard"
  :type '(plist))

(defcustom rt-dash-jump-key "j"
  "The leading keybinding for query jump functions")

(defcustom rt-dash-queues nil
  "A list of queues to add in the queues section of the dashboard"
  :type '(list))

(custom-set-variables '(rt-dash-queues '("drupal")))

;;; rt-dash Major Mode
(defvar rt-dash-mode-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "q" 'rt-dash-bury-or-kill-this-buffer)
    (define-key map "g" 'rt-dashboard)
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
  (let ((magit-insert-section--parent magit-root-section))
    (widget-insert "RT Dashboard\n\n")
    (magit-insert-section (saved-queries nil t)
      (magit-insert-heading "Saved Queries")
      (rt-dash-insert-queries))
    (widget-insert "\n")
    (magit-insert-section (queues nil t)
      (magit-insert-heading "Queues")
      (magit-insert-section-body (rt-dash-insert-queues))))
  (magit-section-mode)
  (goto-char 0))

(defun rt-dash-insert-queries ()
  "Inserts all query sections"
  (dolist (q rt-dash-saved-queries)
    (rt-dash-insert-query q)))

(defun rt-dash-insert-query (q)
  (let* ((name   (plist-get q :name))
	 (query  (plist-get q :query))
	 (key    (plist-get q :key))
	 (ticketlist (rt-liber-rest-run-ls-query query)))
    (magit-insert-section (magit-section name t)
      (magit-insert-heading (propertize name 'query query))
      (let ((tickets (rt-liber-rest-run-show-base-query ticketlist)))
	(dolist (ticket tickets)
	  (rt-dash-insert-ticket-link ticket))))))

(defun rt-dash-insert-ticket-link (ticket)
  "Insert a link to view TICKET history"
  (let ((subject (cdr (assoc "Subject" ticket)))
	(id (car (cdr (split-string (cdr (assoc "id" ticket)) "/")))))
    (magit-insert-section (ticket nil t)
      (widget-create 'link
		     :notify `(lambda (&rest ignore)
				(rt-liber-display-ticket-history ',ticket))
		     :button-prefix ""
		     :button-suffix ""
		     (format "[#%s] %s\n" id subject)))))

(defun rt-dash-insert-queues ()
  "Inserts all saved queues links"
  (dolist (queue rt-dash-queues)
    (rt-dash-insert-queue-link queue)))

(defun rt-dash-insert-queue-link (queue)
  "Insert a link to view a queue"
  (magit-insert-section (queue nil t)
    (widget-create 'link
		   :notify `(lambda (&rest ignore)
			      (rt-liber-browse-query
			       (format "Queue = '%s' and Status = 'open'"
				       queue)))
		   :button-prefix ""
		   :button-suffix ""
		   (format "%s\n" queue))))

(defun rt-dash-insert-search ()
  "Insert a search widget."
  (widget-insert "Search: ")
  (widget-create 'editable-field
		 :action (lambda (widget &rest ignore)
			   (rt-dash-search (widget-value widget))))
  (widget-insert ".")
  (put-text-property (1- (point)) (point) 'invisible t)
  (widget-insert "\n"))

(defun rt-dash-insert-queue-button (q)
  "Inserts a button to see all tickets in QUEUE"
  (widget-create 'link
		 :notify `(lambda (&rest ignore)
			    (rt-liber-browse-query
			     (format "Queue = '%s'" ,q)))
		 :button-prefix ""
		 :button-suffix ""
		 q))

(defun rt-dash-insert-query-buttons (queries)
  "Adds a button for every query in the queries plist"
  (dolist (q queries)
    (rt-dash-insert-query-button q)))

(defun rt-dash-insert-query-button (q)
  "Inserts a query widget for Q"
  (let* ((name   (plist-get q :name))
	 (query  (plist-get q :query))
	 (key    (plist-get q :key))
	 (count  (length (rt-liber-rest-run-ls-query query))))
    ;; to left-align the ticket counts.  3 is a magic number and should
    ;; eventually be replaced by the max length of the query counts
    (widget-insert (make-string (- 3 (length (number-to-string count)))
				? ))
    (widget-insert (number-to-string count))
    (widget-insert " ")
    (widget-create 'link
		   :notify `(lambda (&rest ignore)
			      (rt-liber-browse-query ,query))
		   :button-prefix ""
		   :button-suffix ""
		   name)
    (if key
	(widget-insert (format " (%s %s)\n" rt-dash-jump-key key))
      (widget-insert "\n"))))

(defun rt-dash-add-bindings (queries)
  "Adds bindings to rt-dash-mode-map to jump to the provided QUERIES"
  (let ((map rt-dash-mode-map))
    (dolist (q queries)
      (let ((key   (plist-get q :key))
	    (query (plist-get q :query)))
	(if key
	    (define-key map (kbd (format "%s %s" rt-dash-jump-key key))
	      `(lambda () (interactive)(rt-liber-browse-query ,query))))))))

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
