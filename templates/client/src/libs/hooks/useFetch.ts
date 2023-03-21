import { useEffect, useState } from 'react';

type UseFetchType = {
  findFunc: (where?: number) => string;
  where?: number;
  model?: string;
  privateStream?: boolean;
  accessToken?: string;
};

const useFetch = <T>({ findFunc, where, model, privateStream, accessToken }: UseFetchType) => {
  const url = findFunc(where);
  const [data, setData] = useState<T>();
  const [error, setError] = useState<string | undefined>();
  const [isLoading, setIsLoading] = useState<boolean>(true);

  const fetcher = async () => {
    try {
      const headers: { [x: string]: string } = {};

      if (accessToken) {
        headers['Authorization'] = `Bearer ${accessToken}`;
      }

      const response = await fetch(url, {
        headers,
      });

      if (!response.ok) {
        setError('An error occurred');
        setIsLoading(false);
        return;
      }

      const data = await response.json();

      setData(data);
      setError(undefined);
      setIsLoading(false);
    } catch (err: any) {
      setData(undefined);
      setError(err);
      setIsLoading(false);
    }
  };

  useEffect(() => {
    const eventSource = new EventSource(
      `${url}/events?accessToken=${accessToken}`,
    );

    setIsLoading(true);

    fetcher();

    let createStream = `create${model}`
    if (privateStream) createStream += accessToken
    eventSource.addEventListener(createStream, (event) => {
      setData((prevData: T) => {
        return Array.isArray(prevData)
          ? [...prevData, JSON.parse(event.data)]
          : JSON.parse(event.data);
      });
    });

    let updateStream = `update${model}`
    if (privateStream) updateStream += accessToken
    eventSource.addEventListener(updateStream, (event) => {
      setData((prevData: T) => {
        if (!Array.isArray(prevData)) return JSON.parse(event.data);
        const updatedItem = JSON.parse(event.data);
        const updatedIndex = prevData.findIndex(
          (item) => item.id === updatedItem.id,
        );
        if (updatedIndex === -1) return prevData;
        const newData = [...prevData];
        newData[updatedIndex] = updatedItem;
        return newData;
      });
    });

    let deleteStream = `delete${model}`
    if (privateStream) deleteStream += accessToken
    eventSource.addEventListener(deleteStream, (event) => {
      setData((prevData: T) => {
        if (!Array.isArray(prevData)) return undefined;
        const deletedId = JSON.parse(event.data).id;
        const newData = prevData.filter((item) => item.id !== deletedId);
        return newData;
      });
    });

    setIsLoading(false);

    return () => {
      eventSource.close();
    };
  }, [url]);

  return { data, isLoading, error };
};

export default useFetch;
