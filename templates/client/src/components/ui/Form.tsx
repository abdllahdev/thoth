import React, { useState } from "react";

type FormInput = {
  type: "text" | "email" | "password" | "number";
  name: string;
  visibility: string;
  style?: string;
  defaultValue?: string | number;
  placeholder?: string;
};

type FormButton = {
  name: string;
  style?: string;
}

type FormProps = {
  method: "POST" | "PUT";
  url: string;
  formInputs: FormInput[];
  formButton: FormButton;
};

const Form = ({ method, url, formInputs, formButton }: FormProps) => {
  const formInitialData: { [key: string]: any } = {};

  formInputs.map((input) => {
    formInitialData[input.name] = input.defaultValue ? input.defaultValue : "";
  });

  const [formData, setFormData] = useState(formInitialData);

  const handleInputChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFormData({
      ...formData,
      [event.target.name]: event.target.value,
    });
  };

  const handleFormSubmit = async (event: React.FormEvent) => {
    event.preventDefault();
    try {
      await fetch(url, {
        method,
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(formData),
      });
    } catch (error) {
      console.log(error);
    }
  };

  return (
    <form onSubmit={handleFormSubmit}>
      {formInputs.map((input, idx) => {
        if (input.visibility === "true")
          return (
            <input
              key={idx}
              type={input.type}
              name={input.name}
              value={formData[input.name]}
              onChange={handleInputChange}
              placeholder={input.placeholder}
            />
          );
      })}
      <button type="submit" className={formButton.style}>{formButton.name}</button>
    </form>
  );
};

export default Form;
