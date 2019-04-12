              +-----------------------------------------------------------------------------------------+
global env -> |                                                                                         |
              |   w1                                        w2                                          |
              +---|-----------------------------------------|-------------------------------------------+
                  |                               ^         |                               ^
                  |          (make-withdraw 100)  |         |                               |
                  |                               |         |                               |
                  |                    +--------------+     |                      +--------------+
                  |                    |              |     |                      |              |
                  |             E1 ->  | initial: 100 |     |               E1 ->  | initial: 100 |
                  |                    |              |     |                      |              |
                  |                    +--------------+     |                      +--------------+
                  |                               ^         |                               ^
                  | ((lambda (balance) ...) 100)  |         | ((lambda (balance) ...) 100)  |
                  |                               |         |                               |
                  |                    +--------------+     |                      +--------------+
                  |                    |              |     |                      |              |
                  |             E2 ->  | balance: 50  |     |               E2 ->  | balance: 100 |
                  |                    |              |     |                      |              |
                  |                    +--------------+     |                      +--------------+
                  |                      |        ^         |                          |       ^
                  |                      |        |         |                          |       |
                  |                      v        |         |                          v       |
                  +------------------> [*][*]-----+         +----------------------->[*][*]----+
                                        |                                             |
                                        |                                             |
                                        v                                             v
                         parameters: amount                             parameters: amount
                         body: (if (>= balance amount)                  body: (if (>= balance amount)
                                   (begin (set! balance                           (begin (set! balance
                                                (- balance amount))                      (- balance amount))
                                          balance)                                balance)
                                   "Insufficient funds")                          "Insufficient funds")