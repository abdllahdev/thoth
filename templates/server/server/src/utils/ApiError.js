/**
 * @param {int} status
 * @param {Array} errors
 */
const ApiError = function (status, msg) {
  this.status = status;
  this.msg = msg;
};

module.exports = ApiError;
