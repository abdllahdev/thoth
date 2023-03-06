import React, { useState } from "react";

type FormInputLabel = {
  name: string;
  style?: string;
};

type FormInput = {
  type: "text" | "email" | "password" | "number" | "object";
  name: string;
  visibility: boolean;
  style?: string;
  defaultValue?: string | number | { [x: string]: any };
  placeholder?: string;
};

type FormElement = {
  style?: string;
  formInputLabel?: FormInputLabel;
  formInput: FormInput;
}

type FormButton = {
  name: string;
  style?: string;
};

type FormProps = {
  method: "POST" | "PUT";
  url: string;
  formElements: FormElement[];
  formButton: FormButton;
  handleResponse?: (response: any) => void;
  style?: string;
  accessToken?: string | null;
};

const Form = ({
  method,
  url,
  formElements,
  formButton,
  style,
  handleResponse,
  accessToken,
}: FormProps) => {
  const formInitialData: { [key: string]: any } = {};

  formElements.map((formElement) => {
    const formInput = formElement.formInput;
    formInitialData[formInput.name] = formInput.defaultValue ? formInput.defaultValue : "";
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
      {formElements.map((formElement, idx) => {
        const formInput = formElement.formInput;
        const formLabel = formElement?.formInputLabel;
        const formElementStyle = formElement?.style;
        if (formInput.visibility == true && formInput.type !== "object")
          return (
            <div key={idx} className={formElementStyle}>
              {formLabel && (
                <label className={formLabel.style}>{formLabel.name}</label>
              )}
              <input
                type={formInput.type}
                name={formInput.name}
                value={formData[formInput.name]}
                placeholder={formInput.placeholder}
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
