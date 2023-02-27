import { Request } from 'express';

declare global {
  namespace Express {
    export interface Request {
      validatedPayload: {
        where?: any;
        data?: any;
      };
      user: JwtPayload;
    }
  }
}
