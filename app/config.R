library(stringr)
library(lubridate)
library(dplyr)
library(quanteda)
library(tidytext)
library(tm)
library(visNetwork)
library(tidyr)
library(readr)
library(tidyselect)
library(shinydashboard)
library(igraph)
library(DT)
library(scales)
drif <- Sys.time()  # per produzione data di riferimento
drif <- '2019-10-20' # per test data di riferimento
ntophashtag <- 50 #numero di hastag più frequenti

