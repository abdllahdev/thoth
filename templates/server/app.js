// Global consts
const express = require('express');
const cors = require('cors');
const helmet = require('helmet');
const xss = require('xss-clean');
const compression = require('compression');
const bodyParser = require('body-parser');
const httpStatus = require('http-status');

// App consts
const { errorHandler } = require('./middlewares');
const routes = require('./routes');
const { ApiError } = require('./utils');

// Init express app
const app = express();

// Set security HTTP headers
app.use(helmet());

// Parse application/json
app.use(bodyParser.json());

// Parse application/x-www-form-urlencoded
app.use(bodyParser.urlencoded({ extended: false }));

// Sanitize request data
app.use(xss());

// gzip compression
app.use(compression());

// Enable cors
app.use(cors());
app.options('*', cors());

// App router
app.use('/', routes);

// Send back a 404 error for any unknown api request
app.use((req, res, next) => {
  const error = new ApiError(httpStatus.NOT_FOUND, 'Not found');
  errorHandler(error, req, res, next);
});

// Global error handler
app.use(errorHandler);

module.exports = app;
