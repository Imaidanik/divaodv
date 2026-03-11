.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "divaodv ", utils::packageVersion("divaodv"),
    " \u2014 ODV-style section plots via DIVAnd interpolation\n",
    "Julia + DIVAnd.jl required. Run diva_setup_check() to verify."
  )
}

#' Check Julia + DIVAnd availability
#'
#' Verifies that Julia is reachable via JuliaCall and that the DIVAnd package
#' is installed. Prints a diagnostic summary and returns invisibly.
#'
#' @return Invisibly, a logical: \code{TRUE} if Julia + DIVAnd are ready.
#' @export
diva_setup_check <- function() {

  cat("Checking Julia + DIVAnd setup...\n")

  ok <- tryCatch({
    JuliaCall::julia_setup()
    cat("  [OK] Julia found\n")
    JuliaCall::julia_library("DIVAnd")
    cat("  [OK] DIVAnd.jl loaded\n")
    JuliaCall::julia_eval("using Statistics")
    cat("  [OK] Statistics loaded\n")
    julia_ver <- JuliaCall::julia_eval("string(VERSION)")
    cat("  Julia version:", julia_ver, "\n")
    TRUE
  }, error = function(e) {
    cat("  [FAIL]", conditionMessage(e), "\n")
    cat("\nTo install:\n")
    cat("  1. Install Julia from https://julialang.org/downloads/\n")
    cat("  2. In Julia REPL: using Pkg; Pkg.add(\"DIVAnd\")\n")
    cat("  3. In R: install.packages(\"JuliaCall\")\n")
    FALSE
  })

  invisible(ok)
}
