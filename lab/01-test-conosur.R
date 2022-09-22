# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

## library(conosur)
library(taxlist)
library(readODS)
library(XML)
library(tcltk)
library(conosur)
library(stringr)

# Testing functions
## source("R/conosur_species.R")
## source("R/load_author.R")
## source("R/get_synonyms.R")

library(conosur)
t1 <- conosur_species("Z")
t2 <- get_info(t1, TaxonConceptID %in% c(173379, 24062, 196608))




t2 <- get_synonyms(t1, TaxonConceptID %in% c(173379, 24062, 196608))

t3 <- get_synonyms(t1, TaxonConceptID == 24062)



x = t1
progress = TRUE
idx <- substitute(TaxonConceptID %in% c(173379, 24062, 196608))


x <- t2 <- conosur_species(letter = "K")
idx <- substitute(TaxonConceptID %in% t2$TaxonConceptID[1:5])

i <- 3
x <- x[i, ]


i <- 4
home <- "http://www.darwin.edu.ar"
x <- x[i, ]

#' splist <- get_synonyms(splist)



t2 <- get_info(t1)



m_funct <- function(x) if (x > 1) return(5) else stop ("x = 1!")

m_funct(2)
m_funct(1)



# Check this!

m_funct2 <- function(x) tryCatch({
        OUT <- NULL
        OUT <- m_funct(x)}, error = function(e) message("Error here!"),
      finally = return(OUT))

m_funct2(2)
m_funct2(1)


OUT <- list()



x = t2[1,]

x = t2
progress = FALSE

i <- 1


t10 <- conosur_species(letter = "K")[1:5, ]
t11 <- get_info(t10)
t12 <- get_synonyms(t11)

(Test <- df2taxlist(t12))

summary(Test, "all")


x = t11
progress = TRUE




browseURL(splist[5, "url"])


A <- list("A", NULL, "B")
do.call(c, A)


Test <- get_table(t1[1,])


x <- t1
progress = TRUE


splist <- conosur_species(letter = "K")[1:5, ]
splist <- get_synonyms(splist)
head(splist)






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



# Main test
library(conosur)

start_time <- Sys.time()
t10 <- conosur_species()
end_time <- Sys.time()

end_time - start_time

start_time2 <- Sys.time()
t10 <- conosur_species(progress = FALSE)
end_time2 <- Sys.time()

end_time2 - start_time2


t11 <- get_info(t10)
t12 <- get_synonyms(t11)


