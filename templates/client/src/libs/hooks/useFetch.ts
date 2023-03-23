import { useEffect, useState } from 'react';

type FuncParameters = {
  where?: number;
  search?: { [x: string]: any };
};

type UseFetchType = {
  findFunc: (arg: FuncParameters) => string;
  eventsFunc: () => string;
  where?: number;
  search?: { [x: string]: any };
  model?: string;
  privateStream?: boolean;
  accessToken?: string;
};

const useFetch = <T>({ findFunc, eventsFunc, where, search, model, privateStream, accessToken }: UseFetchType) => {
  const url = findFunc({ where, search });
  const eventsUrl = eventsFunc();
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
    const eventSource = new EventSource(`${eventsUrl}/events?accessToken=${accessToken}`);

    setIsLoading(true);

    fetcher();

    let createStream = `create${model}`;
    if (privateStream) createStream += accessToken;
    eventSource.addEventListener(createStream, (event) => {
      setData((prevData: T) => {
        return Array.isArray(prevData) ? [...prevData, JSON.parse(event.data)] : JSON.parse(event.data);
      });
    });

    let updateStream = `update${model}`;
    if (privateStream) updateStream += accessToken;
    eventSource.addEventListener(updateStream, (event) => {
      setData((prevData: T) => {
        const eventData = JSON.parse(event.data);
        if (!Array.isArray(eventData)) {
          // Handle single object update
          if (!Array.isArray(prevData)) {
            // Handle single object initial case
            return eventData;
          } else {
            // Handle single object update in array
            const updatedItem = eventData;
            const updatedIndex = prevData.findIndex((item) => item.id === updatedItem.id);
            if (updatedIndex === -1) return prevData;
            const newData = [...prevData];
            newData[updatedIndex] = updatedItem;
            return newData;
          }
        } else {
          // Handle array update
          const newData = prevData.map((item) => {
            const updatedItem = eventData.find((e) => e.id === item.id);
            return updatedItem ? updatedItem : item;
          });
          return newData;
        }
      });
    });

    let deleteStream = `delete${model}`;
    if (privateStream) deleteStream += accessToken;
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
