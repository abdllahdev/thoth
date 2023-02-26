import { NextFunction, Request, Response } from 'express';
import httpStatus from 'http-status';
import jwt from 'jsonwebtoken';
import ApiError from '../utils/apiError';
import { errorHandler } from './errorHandler';

export function isAuthenticated(
  req: Request,
  res: Response,
  next: NextFunction,
) {
  const { authorization } = req.headers;

  if (!authorization)
    throw new ApiError({
      status: httpStatus.UNAUTHORIZED,
      code: 'unauthorized',
      error: 'unauthorized',
      message: 'Unauthorized access',
      details: [],
    });

  try {
    const token = authorization.split(' ')[1];
    const payload = jwt.verify(token, process.env.JWT_ACCESS_SECRET as string);
    req.user = payload;
    next();
  } catch (err) {
    const apiError = new ApiError({
      status: httpStatus.UNAUTHORIZED,
      code: 'unauthorized',
      error: 'unauthorized',
      message: 'Invalid access token',
      details: [],
    });

    errorHandler(apiError, req, res, next);
  }
}
