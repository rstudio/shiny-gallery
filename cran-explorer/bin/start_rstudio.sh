#!/bin/bash

docker run -d -p 8788:8787 -v $(pwd):/home/rstudio -e DISABLE_AUTH=true local/rstudio
