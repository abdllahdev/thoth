const getNum = (str) => {
    return /^\d+$/.test(str) ? parseInt(str) : NaN
}

module.exports = getNum;
