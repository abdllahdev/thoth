import { NextFunction, Request, Response } from 'express';

export async function ownsEntry(req: Request, _: Response, next: NextFunction) {
  try {
    const { userId } = req.user;

    req.validatedPayload = {
      ...req.validatedPayload,
      where: {
        user: { id: userId },
        ...req.validatedPayload.where,
      },
    };

    next();
  } catch (err) {
    next(err);
  }
}
