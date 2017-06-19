# TODO:   Examples inserting lines to .Rbuildignore
# 
# Author: Miguel Alvarez
################################################################################

require(devtools)

# Example for this file
use_build_ignore(file.path("tests", c("buildignore.R","test_conosur.R")),
        pkg="M:/WorkspaceEclipse/conosur")
