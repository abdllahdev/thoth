import { Request } from 'express';

declare global {
  namespace Express {
    export interface Request {
      validatedPayload: {
        [x: string]: any;
      };
    }
  }
}
