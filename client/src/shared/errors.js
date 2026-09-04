module.exports =
  class GingkoError extends Error {
    constructor(message, data) {
      super(message);
      this.name = "GingkoError";
      this.data = data;
    }
  };
