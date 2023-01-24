const httpStatus = require('http-status');
const { ApiError, handleResponse } = require('../utils');

// eslint-disable-next-line no-unused-vars
const errorHandler = (err, req, res, next) => {
  let apiError = err;

  if (!(apiError instanceof ApiError)) {
    console.error(apiError);
    apiError = new ApiError(httpStatus.INTERNAL_SERVER_ERROR, 'Sorry, something went wrong');
  }

  const payload = {
    status: apiError.status,
    msg: apiError.msg,
  };

  handleResponse(res, payload);

  return next();
};

module.exports = errorHandler;
