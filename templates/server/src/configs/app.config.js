const dotenv = require('dotenv');

dotenv.config();

const config = {
  PORT: process.env.APP_PORT,
};

module.exports = config;
