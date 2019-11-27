import React from "react";
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Link,
  useParams
} from "react-router-dom";
import "./App.css";

import { RestfulProvider } from "restful-react";

import { useGetpuzzlesTrainTracks } from "./api-hooks";
import { parseTrainTracks } from "./components/puzzles/train-tracks/game";
import TrainTracksApp from "./components/puzzles/train-tracks/TrainTracksApp";

const App = () => {
  const { loading, data: allTrainTracks } = useGetpuzzlesTrainTracks();
  if (loading) {
    return "loading...";
  }

  return (
    <Router>
      <Switch>
        <Route exact path="/">
          {Object.keys(allTrainTracks).map(trainTrackId => {
            return (
              <li>
                <Link
                  to={`/train-tracks/${trainTrackId}`}
                >{`Train tracks ${trainTrackId}`}</Link>
              </li>
            );
          })}
        </Route>
        <Route path="/train-tracks/:id">
          <TrainTracksAppDispatch allTrainTracks={allTrainTracks} />
        </Route>
      </Switch>
    </Router>
  );
};

const TrainTracksAppDispatch = ({ allTrainTracks }) => {
  const { id } = useParams();
  const trainTracks = allTrainTracks[id];
  const parsedTrainTracks = parseTrainTracks(trainTracks);
  return <TrainTracksApp trainTracks={parsedTrainTracks} />;
};

const AppWithProvider = () => (
  <RestfulProvider base="http://localhost:8081">
    <App />
  </RestfulProvider>
);

export default AppWithProvider;
