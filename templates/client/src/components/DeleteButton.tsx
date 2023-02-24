import React from "react";

type DeleteButtonProps = {
  url: string;
  name: string;
  style?: string;
};

const DeleteButton = ({ url, name, style }: DeleteButtonProps) => {
  const handleDelete = async () => {
    await fetch(url, {
      method: "DELETE",
    });
  };

  return <button onClick={handleDelete} className={style}>{name}</button>;
};

export default DeleteButton;
