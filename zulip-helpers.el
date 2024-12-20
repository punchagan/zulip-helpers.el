(require 'request)
;; Used for exporting org to markdown format
(require 'ox-gfm)
(require 'cl)
(require 's)

(defvar zulip-rc-directory "~/.config/zuliprc.d"
  "Directory where zuliprc files for realms are stored")

(defun zulip--create-auth-header (email token)
  (format "Basic %s" (base64-encode-string
                      (format "%s:%s" email token))))

(defun zulip--get-config-path (realm)
  (expand-file-name (format "%s.zuliprc" realm) zulip-rc-directory))

(defun zulip--parse-config (path)
  (with-current-buffer (find-file-noselect path)
    (let (email token)
      (goto-char (point-min))
      (re-search-forward "^email=")
      (setq email (buffer-substring (point) (line-end-position)))
      (re-search-forward "^key=")
      (setq token (buffer-substring (point) (line-end-position)))
      `(,email ,token))))

(defun zulip-send-message (realm email token stream topic message &optional type success-hook)
  "Send a message to a given realm, stream & topic"
  (let ((url (format "https://%s/api/v1/messages" realm)))
    (request
     url
     :type "POST"
     :data `(("type" . ,(or type "stream"))
             ("to" . ,stream)
             ("subject" . ,topic)
             ("content" . ,message))
     :headers `(("Authorization" . ,(zulip--create-auth-header email token)))
     :parser 'json-read
     :success success-hook)))

(defun zulip-update-message (realm email token message-id message &optional success-hook)
  "Update a message with new content"
  (let ((url (format "https://%s/api/v1/messages/%s" realm message-id)))
    (request
     url
     :type "PATCH"
     :data `(("content" . ,message))
     :headers `(("Authorization" . ,(zulip--create-auth-header email token)))
     :parser 'json-read
     :success success-hook)))

(defun zulip-get-messages (realm email token num_before num_after &optional anchor success-hook)
  "Get messages"
  (let* ((params `(("num_before" ,num_before)
                   ("num_after" ,num_after)
                   ,(if anchor
                        `("anchor" ,anchor)
                      `("use_first_unread_anchor" "true"))))
         (query-string (url-build-query-string params))
         (url (format "https://%s/api/v1/messages?%s" realm query-string)))
    (request
     url
     :type "GET"
     :headers `(("Authorization" . ,(zulip--create-auth-header email token)))
     :parser 'json-read
     :success success-hook)))

(defun zulip-get-all-streams (realm email token &optional success-hook)
  "Get all streams for the realm"
  (let ((url (format "https://%s/api/v1/streams" realm)))
    (request
     url
     :type "GET"
     :headers `(("Authorization" . ,(zulip--create-auth-header email token)))
     :parser 'json-read
     :success success-hook)))

(defun zulip-get-stream-id (realm email token stream-name &optional success-hook)
  "Get stream id using the name"
  (let ((url (format "https://%s/api/v1/get_stream_id?stream=%s"
                     realm
                     (url-hexify-string stream-name))))
    (request
     url
     :type "GET"
     :headers `(("Authorization" . ,(zulip--create-auth-header email token)))
     :parser 'json-read
     :success success-hook)))

(defun zulip-get-stream-topics (realm email token stream-id &optional success-hook)
  "Get topics for a stream"
  (let ((url (format "https://%s/api/v1/users/me/%s/topics" realm stream-id)))
    (request
     url
     :type "GET"
     :headers `(("Authorization" . ,(zulip--create-auth-header email token)))
     :parser 'json-read
     :success success-hook)))

(defun zulip-get-users (realm email token &optional success-hook)
  "Get all users"
  (let ((url (format "https://%s/api/v1/users" realm)))
    (request
     url
     :type "GET"
     :headers `(("Authorization" . ,(zulip--create-auth-header email token)))
     :parser 'json-read
     :success success-hook)))

;; Org specific helpers

(defun zulip-org-get-subtree-content ()
  (let ((org-export-with-toc nil)
        (org-export-with-sub-superscripts nil)
        (switch-to-buffer-preserve-window-point t)
        (buf (current-buffer))
        (p (point))
        content)
    (org-gfm-export-as-markdown nil t t)
    (setq content (progn
                    (goto-char (point-min))
                    (while (re-search-forward "\\\\\\([*#_]\\)" nil t)
                      (replace-match "\\1"))
                    (org-no-properties (buffer-string))))
    (kill-buffer)
    (switch-to-buffer-other-window buf)
    (goto-char p)
    content))

(defun zulip-org-send-message-subtree ()
  (let* ((realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (expand-file-name (format "%s.zuliprc" realm) zulip-rc-directory))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth))
         (stream (org-entry-get (point) "ZULIP_STREAM"))
         (send-to (org-entry-get (point) "ZULIP_SEND_TO"))
         (to (or send-to stream))
         (type (if send-to "private" "stream"))
         (topic (org-entry-get (point) "ZULIP_TOPIC"))
         (message (zulip-org-get-subtree-content)))
    (zulip-send-message realm email token to topic message type #'zulip-org-send-success-hook)))

(defun zulip-org-update-message-subtree ()
  (let* ((realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (expand-file-name (format "%s.zuliprc" realm) "~/.zulip.d"))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth))
         (message-id (org-entry-get (point) "ZULIP_MESSAGE_ID"))
         (message (zulip-org-get-subtree-content)))
    (zulip-update-message realm email token message-id message #'zulip-org-update-success-hook)))

(defun zulip-org-dwim-subtree ()
  (interactive)
  (let* ((message-id (org-entry-get (point) "ZULIP_MESSAGE_ID")))
    (if message-id
        (zulip-org-update-message-subtree)
      (zulip-org-send-message-subtree))))

(cl-defun zulip-org-send-success-hook (&key data &allow-other-keys)
  (let ((id (cdr (assoc 'id data))))
    (org-set-property "ZULIP_MESSAGE_ID" (number-to-string id)))
  (message "Successfully posted message."))

(cl-defun zulip-org-update-success-hook (&key data &allow-other-keys)
  (message "Successfully updated message."))

;;; streams

(defun zulip-org-set-stream-subtree ()
  (interactive)
  (let* ((realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (zulip--get-config-path realm))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth)))
    (zulip-get-all-streams realm email token #'zulip-org-set-stream-hook)))

(cl-defun zulip-org-set-stream-hook (&key data &allow-other-keys)
  (let* ((streams (cdr (assoc 'streams data)))
         (names (mapcar (lambda (stream) (cdr (assoc 'name stream))) streams))
         (stream (completing-read "Stream: " names nil t)))
    (org-set-property "ZULIP_STREAM" stream)))

(defun zulip-org-insert-stream-name ()
  (interactive)
  (let* ((realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (zulip--get-config-path realm))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth)))
    (zulip-get-all-streams realm email token #'zulip-org-insert-stream-name-hook)))

(cl-defun zulip-org-insert-stream-name-hook (&key data &allow-other-keys)
  (let* ((streams (cdr (assoc 'streams data)))
         (names (mapcar (lambda (stream) (cdr (assoc 'name stream))) streams))
         (stream (completing-read "Stream: " names nil t)))
    (insert (format "#**%s**" stream))))

(defun zulip-org-set-send-to-subtree ()
  (interactive)
  (let* ((realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (zulip--get-config-path realm))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth)))
    (zulip-get-users realm email token #'zulip-org-set-send-to-hook)))

(cl-defun zulip-org-set-send-to-hook (&key data &allow-other-keys)
  (let* ((users (cdr (assoc 'members data)))
         (names (mapcar (lambda (user) (cons (cdr (assoc 'full_name user))
                                             (cdr (assoc 'email user))))
                        users))
         (user (completing-read "User: " names nil t))
         (email (cdr (assoc user names))))
    (org-set-property "ZULIP_SEND_TO" email)))

;;; topics

(defun zulip-org-set-topic-subtree ()
  (interactive)
  (let* ((realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (zulip--get-config-path realm))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth))
         (stream (org-entry-get (point) "ZULIP_STREAM")))
    (zulip-get-stream-id realm email token stream #'zulip-org-get-stream-id-hook)))

(defun zulip-org-set-topic-subtree-from-headline ()
  (interactive)
  (let* ((heading (org-get-heading t t t t)))
    (org-set-property "ZULIP_TOPIC" heading)))

(cl-defun zulip-org-get-stream-id-hook (&key data &allow-other-keys)
  (let* ((stream-id (cdr (assoc 'stream_id data)))
         (realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (zulip--get-config-path realm))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth)))
    (zulip-get-stream-topics realm email token stream-id #'zulip-org-stream-topics-hook)))

(cl-defun zulip-org-stream-topics-hook (&key data &allow-other-keys)
  (let* ((topics (cdr (assoc 'topics data)))
         (names (mapcar (lambda (topic) (cdr (assoc 'name topic))) topics))
         (topic (completing-read "Topic: " names nil t)))
    (org-set-property "ZULIP_TOPIC" topic)))

;;; users

(defun zulip-org-insert-mention ()
  (interactive)
  (let* ((realm (org-entry-get (point) "ZULIP_REALM"))
         (config-path (zulip--get-config-path realm))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth)))
    (zulip-get-users realm email token #'zulip-org-insert-mention-hook)))

(cl-defun zulip-org-insert-mention-hook (&key data &allow-other-keys)
  (let* ((users (cdr (assoc 'members data)))
         (names (mapcar (lambda (user) (cdr (assoc 'full_name user))) users))
         (user (completing-read "User: " names nil t)))
    (insert (format "@**%s**" user))))

;; subtree from url

(defun zulip-org-set-subtree-properties-from-url (url)
  (interactive "sURL: ")
  (let* ((parsed-url (url-generic-parse-url url))
         (realm (url-host parsed-url))
         (config-path (zulip--get-config-path realm))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth))
         (target (s-split "/" (url-target parsed-url)))
         (message-id (nth 6 target)))
    (org-set-property "ZULIP_REALM" realm)
    (when message-id
      (zulip-get-messages realm email token 0 0 message-id #'zulip-org-set-subtree-properties-hook))))

(cl-defun zulip-org-set-subtree-properties-hook (&key data &allow-other-keys)
  (let* ((messages (cdr (assoc 'messages data)))
         (message (aref messages 0))
         (stream (cdr (assoc 'display_recipient message)))
         (topic (cdr (assoc 'subject message)))
         (message-id (cdr (assoc 'id message))))
    (org-set-property "ZULIP_STREAM" stream)
    (org-set-property "ZULIP_TOPIC" topic)
    (org-set-property "ZULIP_MESSAGE_ID" (number-to-string message-id))))

(provide 'zulip-helpers)
