const handleResponse = (res, payload) => {
  res.status(payload.status).json(payload);
};

module.exports = handleResponse;
