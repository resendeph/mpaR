# Load the package from source for all tests
pkgload::load_all(
  path      = system.file(package = "mpaR", mustWork = FALSE) %||%
              file.path(dirname(dirname(getwd())), ""),
  quiet     = TRUE,
  export_all = FALSE
)
