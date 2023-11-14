# stdio

This folder contains a small script for seeing what is needed to terminate a child process with the different `stdio` options. It also helps verify when a `stream` will have a value and when it will be `null`.

Conclusions:

| stdio | Stream | Read | Write | Program terminates when |
| - | - | - | - | - | 
| `inherit` | `stdin` | Error | Error | `Ctrl+D` pressed |
| `ignore` | `stdin` | Error | Error | Event Loop finishes |
| `pipe` | `stdin` | Error | Success | `stdin.end()` called |
| `inherit` | `stdout` | Error | Error | Event Loop finishes |
| `ignore` | `stdout` | Error | Error | Event Loop finishes |
| `pipe` | `stdout` | Success | Error | Event Loop finishes |
| `inherit` | `stderr` | Error | Error | Event Loop finishes |
| `ignore` | `stderr` | Error | Error | Event Loop finishes |
| `pipe` | `stderr` | Success | Error | Event Loop finishes |
