# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

## library(conosur)
library(taxlist)
library(readODS)
library(XML)
library(tcltk)

# Testing functions
source("R/conosur_species.R")
source("R/load_author.R")

t1 <- conosur_species("Z")
t2 <- load_author(t1[1:5, ])



# Reference list
spp <- read_ods("lab/klimnem_species.ods")

# Families
Families <- conosur_families()

# conosur_query
t1 <- conosur_query(genus = dissect_name(spp$sp_name[1], repaste = 1))




# conosur_species



# TODO: Collapse by default


x <- t2[1:3, ]
i <- 1

url <- x[i, "url"]
level <- x[i, "Level"]


x = t1
progress = TRUE


x = x[1, ]





###
letter = "Z"
home="http://www.darwin.edu.ar"
progress=TRUE
i <- "Z"


library(stringr)
## espcod <- str_match(query, "espcod=\\s*(.*?)\\s*")
espcod <- as.integer(str_match(query, "espcod=(.*?)$")[ , 2])

espcod <- as.integer(sub(".*espcod=(.+)$", "\\1", query))
head(espcod)

i <- t_ranks[4]
t3 <- sub(paste0(".*", i, "=(.+?)&.*"), "\\1", query)
head(t3)


a<-" anything goes here, STR1GET_ME& but not me&, anything goes here"

gsub(".*STR1(.+)&.*", "\\1", a)

gsub(".*STR1(.*?)&.*", "\\1", a)



