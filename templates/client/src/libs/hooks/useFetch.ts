import { useEffect, useState } from "react";

// TODO: create a store in the app and update the cached data based on the events pushed by the server
const useFetch = <T>(findFunc : () => Promise<Response>, eventsUrl?: string) => {
  const [data, setData] = useState<T>();
  const [error, setError] = useState<string | undefined>();
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const fetcher = async () => {
    const response = await findFunc()

    if (!response.ok) {
      setError("An error occurred");
      return;
    }

    const data = await response.json();
    setData(data);
    setError(undefined);
  };

  useEffect(() => {
    if (eventsUrl) {
      setIsLoading(true);

      const eventSource = new EventSource(eventsUrl);

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
    }
  }, []);

  return { data, isLoading, error };
};

export default useFetch;
