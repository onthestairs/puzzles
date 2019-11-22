import React from "react";
import "./App.css";

import { RestfulProvider } from "restful-react";

import { useGetpuzzlesTrainTracks } from "./api-hooks";
import TrainTracksApp from "./components/puzzles/train-tracks/TrainTracksApp";

const App = () => {
  const { loading, data: trainTracks } = useGetpuzzlesTrainTracks();
  if (loading) {
    return "loading...";
  }
  return (
    <TrainTracksApp trainTracks={trainTracks[Object.keys(trainTracks)[0]]} />
  );
};

const AppWithProvider = () => (
  <RestfulProvider base="http://localhost:8081">
    <App />
  </RestfulProvider>
);

export default AppWithProvider;
