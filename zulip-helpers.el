(require 'request)

(defun zulip--create-auth-header (email token)
  (format "Basic %s" (base64-encode-string
                      (format "%s:%s" email token))))

(defun zulip--parse-config (path)
  (with-current-buffer (find-file-noselect path)
    (let (email token)
      (goto-char (point-min))
      (re-search-forward "^email=")
      (setq email (buffer-substring (point) (line-end-position)))
      (re-search-forward "^key=")
      (setq token (buffer-substring (point) (line-end-position)))
      `(,email ,token))))

(defun zulip-send-message (realm email token stream topic message &optional success-hook)
  "Send a message to a given realm, stream & topic"
  (let ((url (format "https://%s/api/v1/messages" realm)))
    (request
     url
     :type "POST"
     :data `(("type" . "stream")
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
  (let* ((realm (cdr (assoc "ZULIP_REALM" (org-entry-properties (point) "ZULIP_REALM"))))
         (config-path (expand-file-name (format "%s.zrc" realm) "~/.zulip.d"))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth))
         (stream (cdr (assoc "ZULIP_STREAM" (org-entry-properties (point) "ZULIP_STREAM"))))
         (topic (cdr (assoc "ZULIP_TOPIC" (org-entry-properties (point) "ZULIP_TOPIC"))))
         (message (zulip-org-get-subtree-content)))
    (zulip-send-message realm email token stream topic message #'zulip-org-send-success-hook)))

(defun zulip-org-update-message-subtree ()
  (let* ((realm (cdr (assoc "ZULIP_REALM" (org-entry-properties (point) "ZULIP_REALM"))))
         (config-path (expand-file-name (format "%s.zrc" realm) "~/.zulip.d"))
         (auth (zulip--parse-config config-path))
         (email (car auth))
         (token (cadr auth))
         (message-id (cdr (assoc "ZULIP_MESSAGE_ID" (org-entry-properties (point) "ZULIP_MESSAGE_ID"))))
         (message (zulip-org-get-subtree-content)))
    (zulip-update-message realm email token message-id message #'zulip-org-update-success-hook)))

(defun zulip-org-dwim-subtree ()
  (interactive)
  (let* ((message-id (cdr (assoc "ZULIP_MESSAGE_ID" (org-entry-properties (point) "ZULIP_MESSAGE_ID")))))
    (if message-id
        (zulip-org-update-message-subtree)
      (zulip-org-send-message-subtree))))

(cl-defun zulip-org-send-success-hook (&key data &allow-other-keys)
  (let ((id (cdr (assoc 'id data))))
    (org-set-property "ZULIP_MESSAGE_ID" (number-to-string id)))
  (message "Successfully posted message."))

(cl-defun zulip-org-update-success-hook (&key data &allow-other-keys)
  (message "Successfully updated message."))


(provide 'zulip-helpers)
