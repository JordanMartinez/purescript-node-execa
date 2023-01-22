# purescript-node-execa

PureScript port of the [`execa`](https://github.com/sindresorhus/execa) ([NPM library](https://www.npmjs.com/package/execa)) JavaScript library and the relevant parts of all of its relevant dependencies when such dependencies essentially share the same license (i.e. MIT/ISC).

## License requirements

- [`cross-spawn`](https://github.com/moxystudio/node-cross-spawn) - MIT
- [`get-stream`](https://github.com/sindresorhus/get-stream) - MIT
- [`is-exe`](https://github.com/isaacs/isexe) - ISC
- [`merge-stream`](https://github.com/grncdr/merge-stream) - MIT
- [`npm-run-path`](https://github.com/sindresorhus/npm-run-path) - MIT
- [`shebang-command`](https://github.com/kevva/shebang-command) - MIT
- [`signal-exit`](https://github.com/tapjs/signal-exit) - ISC
- [`strip-final-newline`](https://github.com/sindresorhus/strip-final-newline) - MIT
- [`which`](https://github.com/npm/node-which) - ISC

The below dependencies of `execa` did not need to be ported since such functionality was implemented primarily via `Aff`.
- `mimic-fn` - functionality unneeded as `Aff` `Fiber`s are a more flexible implementation than `Promise`s.
- `onetime` - functionality provided via `Aff`'s `joinFiber`
- `is-stream` - functionality provided via PureScript's types
- `path-key` - functionality is no longer needed in current versions of Node
