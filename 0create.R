file.edit('.gitignore')
#  ----
library(roxygen2)
roxygenise(clean = TRUE)
#-----
library(pkgdown)
usethis::use_pkgdown()
pkgdown::build_site()


#  usethis -----
library(usethis)
use_description(fields = list(Language = "es"))
edit_r_profile(scope = c("user", "project"))
use_mit_license()       # need a LICENSE file
use_roxygen_md()        # use {roxygen2} for documentation and configuration
use_package_doc()       # setup a package-level manual page
use_testthat()          # setup testing infrastructure
use_test("placeholder") # setup a placeholder test file
devtools::document()             # Let {roxygen2} create NAMESPACE entries, build manual pages (and, more later on)
devtools::check()                # looking for the three "0's" that tell us we're ready to roll!
use_git()               # put the directory under git version control
git_vaccinate()         # Prevent leaking credentials and other unnecessary filesystem cruft
