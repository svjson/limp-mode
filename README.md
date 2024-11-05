
# limp-mode.el

`limp` is an interactive REPL mode for the Lisple embeddable programming language. It provides a managed
connection to REPL server and a REPL buffer.

## Lisple Buffer Commands

| Binding   | Command                   | Description                                                                                                                  |
| --------- | ------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| C-c C-e   | limp-mode-eval-last-sexp  | Sends the Lisple expression preceding the point to the REPL server for evaluation and displays the result in the mini-buffer |
| C-c e     | limp-moe-evaluate-prompt  | Opens a mini-buffer prompt for evaluating arbitrary Lisple expressions.                                                      |
| C-c r     | limp-mode-start-repl      | Starts the Lisple REPL in a new window.                                                                                      |

## Lisple REPL Commands

| Binding   | Command                     | Description                                                                                                                  |
| --------- | --------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| RET       | limp-repl-send-input        | Sends input to the REPL server for evaluation.                                                                               |
| M-p       | limp-repl-previous-command  | Navigate to the previous item in the command history                                                                         |
| M-n       | limp-repl-next-command      | Navigate to the next item in the command history                                                                             |

## Configuration Variables
| Variable                                   | Default | Description                                                                                                               |
| ------------------------------------------ | ------- | ------------------------------------------------------------------------------------------------------------------ |
| limp-mode-reconnect-interval               |       5 | Interval (in seconds) for attempting to reconnect if the REPL server connection is lost.                                  |
| limp-repl-stand-alone-server-port          |    8100 | Default port to connect to a Lisple REPL Server at.                                                                       |
| limp-repl-ns                               |  "user" | Default(and current) lisple namespace to connect to.                                                                      |
| limp-repl-format-fn                        |     nil | (Optional) Function used for formatting Lisple expressions and data before output to the REPL                             |
| limp-repl-history-max-size                 |     100 | Maximum number of commands to store in the command history before evicting old entries.                                   |

