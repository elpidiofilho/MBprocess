.onAttach <- function(libname, pkgname) {
  packageStartupMessage("MBprocess v",
                        utils::packageDescription("MBprocess", field = "Version"),
                        " ",
                        appendLF = TRUE
  )
}
