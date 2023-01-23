/**
 * @param {int} status
 * @param {Array} errors
 */
const ApiError = (status, errors) => {
  this.status = status;
  this.errors = errors;
};

module.exports = ApiError;
