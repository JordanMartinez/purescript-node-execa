import process from "process";

export const getUidImpl = (nothing, just) => {
  if (process.getuid) {
    just(process.getuid());
  } else {
    return nothing;
  }
};

export const getGidImpl = (nothing, just) => {
  if (process.getgid) {
    just(process.getgid());
  } else {
    return nothing;
  }
}
