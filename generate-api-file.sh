#!/bin/bash
cd frontend
node ./scripts/munge-swagger.js
restful-react import --file ../api/api.json --output src/api-hooks/index.tsx