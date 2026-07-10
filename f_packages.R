#' Description: Function to install and/or update packages 
#'
#' This function checks whether a set of required packages is
#' available in the local R environment. Missing packages can be installed
#' automatically, and existing packages (including their dependencies) can
#' optionally be updated to the latest available version.
#'
#' @param packages Character vector containing package names.
#' @param install Logical. If `TRUE`, packages that are not currently
#'   installed will be installed automatically. Default is `TRUE`.
#' @param update Logical. If `TRUE`, installed packages and their
#'   dependencies will be checked for available updates and updated if
#'   necessary. Default is `FALSE`.
#'
#' @details
#' The function performs two independent tasks:
#' \itemize{
#'   \item Installation of missing packages required for the analysis.
#'   \item Updating outdated packages and their dependencies to ensure
#'         compatibility and reproducibility.
#' }
#'
#' Package dependencies are identified using
#' \code{tools::package_dependencies()} and include packages listed under
#' \code{Depends}, \code{Imports}, and \code{LinkingTo}.
#'
#' @return No returned object. The function is called for its side effects
#' (installing and/or updating packages).
#'
#' @export

f_packages <- function(
    packages, # packages to be installed / updated
    install = TRUE, # install new packages if required
    update = FALSE # update new packages if required
){
  # install packages if required
  if (install == TRUE) {
    new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]  # check for new packages
    if (length(new.packages)) {install.packages(new.packages)}  # install new packages (if needed)
  }
  
  # update packages and dependencies if required
  if (update == TRUE) {
    dependencies <- unname(unlist(tools::package_dependencies(packages, db = available.packages(), which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))
    packages_deps <- c(packages, dependencies)
    outdated.packages <- packages_deps[(packages_deps %in% old.packages()[, "Package"])]
    if (length(outdated.packages)) {install.packages(outdated.packages)}  # update packages (if needed)
  }
} # end of function