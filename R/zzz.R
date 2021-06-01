.onAttach <- function(libname, pkgname) {
  packageStartupMessage("carrrotte: R tools to analyse data from sediment cores")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Remo Roethlin",
    devtools.desc.author = "Remo Roethlin avelarius@gmail.com [aut, cre]",
    devtools.desc.license = "GPL-3",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}
