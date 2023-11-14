# ipc

This folder contains a small script for seeing when `ipc` stdio value can be used.

Conclusions:

| Function | Result |
| - | - |
| `spawn` | works |
| `spawnSync` | runtime error |
| `fork` | works |
