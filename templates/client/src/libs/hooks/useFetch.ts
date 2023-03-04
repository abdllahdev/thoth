import { useEffect, useState } from "react";

// TODO: create a store in the app and update the cached data based on the events pushed by the server
const useFetch = <T>(url: string, accessToken?: string | null) => {
  const [data, setData] = useState<T>();
  const [error, setError] = useState<string | undefined>();
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const fetcher = async () => {
    let headers: { [x: string]: string } = {};

    if (accessToken) {
      headers["Authorization"] = `Bearer ${accessToken}`;
    }

    const response = await fetch(url, {
      headers,
    });

    if (!response.ok) {
      setError("An error occurred");
      return;
    }

    const data = await response.json();
    setData(data);
    setError(undefined);
  };

  useEffect(() => {
    setIsLoading(true);
    const eventSource = new EventSource(
      `${url}/events?accessToken=${accessToken}`
    );

    eventSource.addEventListener("create", (event) => {
      fetcher();
    });

    eventSource.addEventListener("update", (event) => {
      fetcher();
    });

    eventSource.addEventListener("destroy", (event) => {
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
