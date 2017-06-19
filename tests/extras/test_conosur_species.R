# TODO:   Some tests from functionality of conosur_species
# 
# Author: Miguel Alvarez
################################################################################

# Test to encoding line
species <- conosur_species(Letter="I", collapse=TRUE)
species <- subset(species, species == "savatieri")
species$genus[1] == iconv("Isoëtes", "UTF-8", "LATIN1")
