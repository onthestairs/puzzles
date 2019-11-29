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

import {
  useGetpuzzlesTrainTracks,
  useGetpuzzlesTrainTracksRandom
} from "./api-hooks";
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
          <li>
            <Link to={`/train-tracks/random`}>{`Train tracks random`}</Link>
          </li>
        </Route>
        <Route path="/train-tracks/random">
          <RandomTrainTracksApp />
        </Route>
        <Route path="/train-tracks/:id">
          <TrainTracksAppDispatch allTrainTracks={allTrainTracks} />
        </Route>
      </Switch>
    </Router>
  );
};

const RandomTrainTracksApp = () => {
  const { loading, data: randomTrainTracks } = useGetpuzzlesTrainTracksRandom({
    queryParams: { rows: 5, cols: 5 }
  });
  if (loading) {
    return "loading...";
  }
  const parsedTrainTracks = parseTrainTracks(randomTrainTracks);
  return <TrainTracksApp trainTracks={parsedTrainTracks} />;
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
