export const buildCustomErrorImpl = (msg, obj) => 
  Object.assign(new Error(msg), obj);

