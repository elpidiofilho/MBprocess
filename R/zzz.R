.onAttach <- function(libname, pkgname) {
  packageStartupMessage("MBprocess ",
                        utils::packageDescription("MBprocess", field = "Version"),
                        " ",
                        appendLF = TRUE
  )
}
