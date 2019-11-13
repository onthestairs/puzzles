import React from "react";
import logo from "./logo.svg";
import "./App.css";

import { RestfulProvider } from "restful-react";

import { useGetUsers } from "./api-hooks";

const App = () => {
  const { loading, data: users } = useGetUsers();
  return (
    <div className="App">
      {loading && "loading!..."}
      {users &&
        users.map(
          user => `${user.name} (registered ${user.registration_date})`
        )}
    </div>
  );
};

const AppWithProvider = () => (
  <RestfulProvider base="http://localhost:8081">
    <App />
  </RestfulProvider>
);

export default AppWithProvider;
