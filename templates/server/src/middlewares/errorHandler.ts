import { Request, Response, NextFunction } from 'express';
import httpStatus from 'http-status';
import ApiError from '../utils/apiError';

export const errorHandler = (
  err: any,
  _req: Request,
  res: Response,
  _next: NextFunction,
) => {
  let apiError = err;

  if (!(apiError instanceof ApiError)) {
    console.log(apiError);
    apiError = new ApiError({
      status: httpStatus.INTERNAL_SERVER_ERROR,
      code: 'internal_server_error',
      error: 'internal_server_error',
      message: 'Something went wrong',
      details: [],
    });
  }

  res.status(apiError.status).json(apiError);
};
