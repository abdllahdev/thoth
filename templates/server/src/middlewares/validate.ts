import { Request, Response, NextFunction } from 'express';
import httpStatus from 'http-status';
import ApiError from '../utils/apiError';
import { errorHandler } from './errorHandler';
import { AnyZodObject, ZodError } from 'zod';

export const validate =
  (schema: AnyZodObject) =>
  async (req: Request, res: Response, next: NextFunction) => {
    try {
      req.validatedPayload = await schema.parseAsync({
        data: req.body,
        where: req.params,
        search: req.query,
      });
      next();
    } catch (error) {
      const details = await (error as ZodError).issues.map((item) => ({
        code: item.code,
        parameter: item.path[1] as string,
        message: item.message,
      }));

      const apiError = new ApiError({
        status: httpStatus.BAD_REQUEST,
        code: 'bad_request',
        error: 'invalid_input',
        message: 'Invalid inputs',
        details,
      });

      errorHandler(apiError, req, res, next);
    }
  };
