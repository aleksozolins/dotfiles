(defun my-ledger ()
  "Open the ledger file located at ~/docs/finances/ledger/my_ledger.txt."
  (interactive)
  (find-file "~/docs/finances/ledger/my_ledger.txt")
  (goto-char (point-max)))

(defun my-recurring-ledger ()
  "Open the ledger file located at ~/docs/finances/ledger/my_recurring_ledger.txt."
  (interactive)
  (find-file "~/docs/finances/ledger/my_recurring_ledger.txt")
  (goto-char (point-max)))

(provide 'ledger-functions)
