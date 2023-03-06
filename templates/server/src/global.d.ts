import { Request } from 'express';

declare global {
  namespace Express {
    export interface Request {
      validatedPayload: {
        search?: any,
        where?: any;
        data?: any;
      };
      user: JwtPayload;
    }
  }
}
