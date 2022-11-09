# Need for Uniqueness in the World of the Smartphones
This thesis aims to research the behavior of reviewers who have purchased a unique smartphone color. Over 100k reviews were extracted from Amazon.com with a web scraper.
Other useful variables were retrieved with a Keepa API. This repository contains the source code of this project.

## Repository overview
+---data
+---gen
¦   +---audit
¦   +---input
¦   +---output
¦   +---paper
¦   +---temp
+---src
    +---api
    +---data-preparation
    +---paper
    +---scraping
    
## Running instructions
The web scraper itself runs with Splash on docker in Python. The Keepa API also runs in Python. The library package 'beautifulsoup' was used to get the data.
The data cleaning was partially done in R and in Python. The modeling and visualization in R.

## About
This thesis was written by Jan van der Doe under the supervision of dr. Hannes Datta
