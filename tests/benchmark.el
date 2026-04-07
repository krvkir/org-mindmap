
(require 'org-mindmap)

(defun benchmark-map-12 ()
  (with-temp-buffer
    (org-mode)
    (insert "
* Test Case 12: Horizontal Recovery Cross-Branch Contamination (Large Map)
#+begin_mindmap :layout centered
                          ╭─ мышления
                          ├─ изучения
┬─ Письмо ── помощник для ┼─ генерации идей
│                         ├─ чтения
│                         ╰─ понимания
├─ Мышление ── происходит на бумаге
├─ Правила ── держать ручку наготове
│                            ╭─ мимолётные :: напоминания о мыслях ┬─ помести в одно место
│                            │               ╭─ когда читаете      ╰─ обработай позже
│                            │               ├─ кратко
│                            ├─ о литературе ┼─ избирательно ── для своих тем
│                            │               ├─ своими словами
│                            │               ├─ с библиографическими данными
│                            │               ╰─ в картотеку     ╭─ мимолётные
│                            │             ╭─ просмотри заметки ┼─ о литературе
│          ╭─ собери заметки ┤             │                    ╰─ раз в день
│          │                 │             │                               ╭─ исследованиями
│          │                 │             ├─ подумай ── как соотносятся с ┼─ размышлениями
│          │                 │             ├─ одну для каждой идеи         ╰─ интересами
│          │                 │             │                  ╭─ полные предложения
╰─ Процесс ┤                 │             │                  ├─ источники
           │                 │             ├─ как для другого ┼─ ссылки
           │                 │             │                  ├─ точно
           │                 │             │                  ├─ ясно
           │                 ╰─ постоянные ┤                  ╰─ кратко
           ├─ преврати в черновик          ├─ выбрось ── мимолётные
           ╰─ отредактируй                 ├─ добавь ┬─ позади заметки, к которой относится напрямую
                                           │         ╰─ ссылки
                                           ├─ убедись ── что сможешь найти ┬─ в указателе
                                           │                               ╰─ в точке входа
                                           │                    ╭─ снизу вверх
                                           │                    ├─ посмотри ┬─ что есть
                                           │                    │           ╰─ какие вопросы возникают
                                           │                    │               ╭─ аргументы ┬─ поставить под сомнение
                                           ╰─ разрабатывай темы ┼─ читай, чтобы ┤            ╰─ укрепить
                                                                │               ╰─ доводы ┬─ расширить
                                                                ├─ делай больше заметок   ╰─ изменить
                                                                ├─ развивай идеи ── смотри, куда приведут
                                                                ├─ следуй за интересом
                                                                ╰─ выбирай путь, который ── обещает больше идей
#+end_mindmap")
    (let* ((region (org-mindmap-parser-get-region))
           (start (car region))
           (end (cdr region))
           (roots nil)
           (t-parse 0)
           (t-layout 0)
           (t-draw 0)
           (t-buffer-update 0))

      ;; Benchmark Parsing
      (let ((st (float-time)))
        (setq roots (org-mindmap-parser-parse-region start end))
        (setq t-parse (* 1000 (- (float-time) st))))

      ;; Benchmark Layout
      (let ((st (float-time)))
        (org-mindmap-build-tree-layout roots 'centered 1)
        (setq t-layout (* 1000 (- (float-time) st))))

      ;; Benchmark Redrawing into temp buffer (simulating render-tree)
      (let ((st (float-time)))
        (with-temp-buffer
          (setq indent-tabs-mode nil)
          (let ((inhibit-read-only t))
            (dolist (root roots)
              (org-mindmap--draw-node root))))
        (setq t-draw (* 1000 (- (float-time) st))))

      (message "BENCHMARK RESULTS (Test Case 12):")
      (message "  Parsing: %7.2f ms" t-parse)
      (message "  Layout:  %7.2f ms" t-layout)
      (message "  Drawing: %7.2f ms" t-draw)
      (message "  Total:   %7.2f ms" (+ t-parse t-layout t-draw)))))

(benchmark-map-12)
