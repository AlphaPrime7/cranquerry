.pkg_globals_cache <- function() {
  .cache <- environment()
  list(
    get = function(y) .cache[[y]],
    set = function(y, v) .cache[[y]] <- v
  )
}

.globals$set("version_3_4", 'https://cloud.r-project.org/web/packages/available_packages_by_name.html')
.globals$set("version_5", 'https://cloud.r-project.org/web/packages/available_packages_by_name.html'
)
.globals$set("version_6", "http://cran.r-project.org/web/packages/packages.rds")
.globals$set("version_7", "http://crandb.r-pkg.org/-/desc")
