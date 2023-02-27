import { ZodIssueCode } from 'zod';

type ErrorCode =
  | 'bad_request'
  | 'not_found'
  | 'internal_server_error'
  | 'forbidden'
  | 'unauthorized';

export type ErrorDetail = {
  code: ZodIssueCode;
  parameter: string;
  message: string;
};

export type ApiErrorType = {
  status: number;
  error: 'invalid_input' | ErrorCode;
  code: ErrorCode;
  message: string;
  details: ErrorDetail[];
};

export default class ApiError {
  private status;
  private code;
  private error;
  private message;
  private details;

  constructor({ status, code, error, message, details }: ApiErrorType) {
    this.status = status;
    this.code = code;
    this.error = error;
    this.message = message;
    this.details = details;
  }
}
