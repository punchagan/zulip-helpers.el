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

(provide 'zulip-helpers)
