.onLoad <- function(libname, pkgname, where)
    message(sprintf("\n*** Deprecation warning ***:\nThe package '%s' is deprecated and will not be supported after Bioconductor release 2.11\n\n", pkgname))
