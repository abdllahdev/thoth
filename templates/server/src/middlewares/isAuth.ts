import { NextFunction, Request, Response } from 'express';
import httpStatus from 'http-status';
import jwt from 'jsonwebtoken';
import ApiError from '../utils/apiError';
import { errorHandler } from './errorHandler';

export function isAuth(
  req: Request,
  res: Response,
  next: NextFunction,
) {
  const { authorization } = req.headers;
  const { accessToken } = req.query;

  if (!authorization && !accessToken)
    throw new ApiError({
      status: httpStatus.UNAUTHORIZED,
      code: 'unauthorized',
      error: 'unauthorized',
      message: 'Unauthorized access',
      details: [],
    });

  try {
    const token = authorization
      ? authorization.split(' ')[1]
      : (accessToken as string);
    const payload = jwt.verify(token, process.env.JWT_ACCESS_SECRET as string);
    req.user = {accessToken: token, ...payload as jwt.JwtPayload};
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
