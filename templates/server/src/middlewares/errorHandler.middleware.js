const httpStatus = require('http-status');
const { ApiError, handleResponse } = require('../utils');

// eslint-disable-next-line no-unused-vars
const errorHandler = (err, req, res, next) => {
  let apiError = err;

  if (!(apiError instanceof ApiError)) {
    console.error(apiError);
    apiError = new ApiError(httpStatus.INTERNAL_SERVER_ERROR, [
      { msg: 'Sorry, something went wrong' },
    ]);
  }

  handleResponse(res, {
    status: apiError.status,
    errors: apiError.errors,
  });

  return next();
};

module.exports = errorHandler;
