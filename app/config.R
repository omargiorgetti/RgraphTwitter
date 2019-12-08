library(stringr)
library(lubridate)
library(dplyr)
library(quanteda)
library(tidytext)
library(tm)
library(visNetwork)
library(tidyr)
library(readr)
library(shinydashboard)
library(igraph)
library(DT)
drif <- Sys.time()  # per produzione data di riferimento
drif <- '2019-09-20' # per test data di riferimento
ntophashtag <- 20 #numero di hastag più frequenti