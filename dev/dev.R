library(usethis)

grkmisc::use_grk_gitignore()
use_directory("dev")
use_build_ignore("dev")
use_directory("_test")
use_build_ignore("_test")
use_git_ignore("_test")

use_mit_license("Garrick Aden-Buie")

use_package("glue")
use_package("knitr")

use_package_doc()
# import glue
edit_file("R/epoxy-package.R")

use_r("epoxy")
use_r("utils")
use_r("zzz")

use_tidy_description()

use_readme_rmd()

use_github()
use_tidy_description()

use_r("engines")
use_package("rmarkdown")
