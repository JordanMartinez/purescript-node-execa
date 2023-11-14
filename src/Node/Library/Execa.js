export function setTimeoutImpl(timeout, cb) {
  const t = setTimeout(cb, timeout);
  // Guarded because there's no `.unref()` when `execa` is used in the renderer
	// process in Electron. This cannot be tested since we don't run tests in
	// Electron.
  return t.unref ? t : { unref: () => {} };
}

const undefinedVal = undefined;
export { undefinedVal as undefined };
