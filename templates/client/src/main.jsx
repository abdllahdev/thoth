import React, { Suspense } from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter as Router, useRoutes } from "react-router-dom";
import { SWRConfig } from "swr";

import "virtual:windi.css";
import routes from "~react-pages";

const App = () => {
  return <Suspense fallback={<p>Loading...</p>}>{useRoutes(routes)}</Suspense>;
};

ReactDOM.createRoot(document.getElementById("root")).render(
  <React.StrictMode>
    <Router>
      <SWRConfig
        value={{
          refreshInterval: 3000,
          fetcher: (url) =>
            fetch(`http://localhost:4001${url}`).then((res) => res.json()),
        }}
      >
        <App />
      </SWRConfig>
    </Router>
  </React.StrictMode>
);
