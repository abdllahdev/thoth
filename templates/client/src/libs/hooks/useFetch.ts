import { useLayoutEffect, useState } from 'react';

// TODO: create a store in the app and update the cached data based on the events pushed by the server

type UseFetchType = {
  findFunc: (where?: number) => string;
  where?: number;
  accessToken?: string;
};

const useFetch = <T>({ findFunc, where, accessToken }: UseFetchType) => {
  const url = findFunc(where);
  const [data, setData] = useState<T>();
  const [error, setError] = useState<string | undefined>();
  const [isLoading, setIsLoading] = useState<boolean>(true);

  const fetcher = async () => {
    try {
      let headers: { [x: string]: string } = {};

      if (accessToken) {
        headers["Authorization"] = `Bearer ${accessToken}`;
      }

      const response = await fetch(url, {
        headers,
      });

      if (!response.ok) {
        setError('An error occurred');
        return;
      }

      const data = await response.json();

      setData(data);
      setError(undefined);
    } catch (err: any) {
      setData(undefined);
      setError(err);
    }
  };

  useLayoutEffect(() => {
    setIsLoading(true);

    const eventSource = new EventSource(
      `${url}/events?accessToken=${accessToken}`,
    );

    eventSource.addEventListener('create', (event) => {
      fetcher();
    });

    eventSource.addEventListener('update', (event) => {
      fetcher();
    });

    eventSource.addEventListener('delete', (event) => {
      fetcher();
    });

    fetcher();
    setIsLoading(false);

    return () => {
      eventSource.close();
    };
  }, []);

  return { data, isLoading, error };
};

export default useFetch;
