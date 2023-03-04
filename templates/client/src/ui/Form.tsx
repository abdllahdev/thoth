import React, { useState } from "react";

type FormInputLabel = {
  name: string;
  style?: string;
};

type FormInput = {
  type: "text" | "email" | "password" | "number" | "object";
  name: string;
  visibility: string;
  style?: string;
  defaultValue?: string | number | { [x: string]: any };
  placeholder?: string;
  label?: FormInputLabel;
};

type FormButton = {
  name: string;
  style?: string;
};

type FormProps = {
  method: "POST" | "PUT";
  url: string;
  formInputs: FormInput[];
  formButton: FormButton;
  handleResponse?: (response: any) => void;
  style?: string;
  accessToken?: string | null;
};

const Form = ({
  method,
  url,
  formInputs,
  formButton,
  style,
  handleResponse,
  accessToken,
}: FormProps) => {
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
      const response = await fetch(url, {
        method,
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${accessToken}`,
        },
        body: JSON.stringify(formData),
      });

      const data = await response.json();

      if (response.ok || response.status == 201) {
        if (handleResponse) handleResponse(data);
      }
    } catch (error) {
      console.log(error);
    }
  };

  return (
    <form className={style} onSubmit={handleFormSubmit}>
      {formInputs.map((input, idx) => {
        if (input.visibility === "true" && input.type !== "object")
          return (
            <div key={idx}>
              {input.label && (
                <label className={input.label.style}>{input.label.name}</label>
              )}
              <input
                type={input.type}
                name={input.name}
                value={formData[input.name]}
                placeholder={input.placeholder}
                onChange={handleInputChange}
              />
            </div>
          );
      })}
      <button className={formButton.style}>{formButton.name}</button>
    </form>
  );
};

export default Form;
