;;; init-calendar.el --- Calendar and diary configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun +diary-weekdayp (date)
  "Return t if DATE is a day of the week (Mon-Fri)."
  (member (calendar-day-of-week date)
          '(1 2 3 4 5)))

(defun +diary-last-day-of-month-p (date)
  "Return t if DATE is the last day of the month."
  (= (calendar-extract-day date)
     (calendar-last-day-of-month (calendar-extract-month date)
                                 (calendar-extract-year date))))

(provide 'init-calendar)
;;; init-calendar.el ends here
