const filterObject = (obj, keys) => {
  const filteredEntries = Object.entries(obj).filter(([key]) => keys.includes(key));
  return Object.fromEntries(filteredEntries);
}

module.exports = filterObject
