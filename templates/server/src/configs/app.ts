import dotenv from 'dotenv';

dotenv.config();

export const appConfig = {
  PORT: process.env.APP_PORT,
};
