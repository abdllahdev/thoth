import React from "react";

type ButtonProps = {
  name: string;
  style?: string;
  handleOnClick?: () => {};
};

const Button = ({
  name,
  style,
  handleOnClick,
}: ButtonProps) => {
  return (
    <button className={style} onClick={handleOnClick}>
      {name}
    </button>
  );
};

export default Button;
