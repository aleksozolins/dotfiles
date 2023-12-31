(defun my-ledger ()
  "Open the ledger file located at ~/docs/finances/ledger/2024--my-ledger.txt."
  (interactive)
  (find-file "~/docs/finances/ledger/2024--my-ledger.txt")
  (goto-char (point-max)))

(defun my-recurring-ledger ()
  "Open the ledger file located at ~/docs/finances/ledger/2024--my-recurring-ledger.txt."
  (interactive)
  (find-file "~/docs/finances/ledger/2024--my-recurring-ledger.txt")
  (goto-char (point-max)))

(provide 'ledger-functions)
