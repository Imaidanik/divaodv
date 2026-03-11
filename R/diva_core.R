# =============================================================================
# diva_core.R
# Internal DIVA Julia engine — NOT exported.
# =============================================================================

# Environment for session-level Julia state
.diva_env <- new.env(parent = emptyenv())
.diva_env$julia_ready <- FALSE


#' Lazy Julia / DIVAnd initialiser (internal)
#' @noRd
.diva_ensure_julia <- function() {

  if (isTRUE(.diva_env$julia_ready)) {
    return(invisible(TRUE))
  }

  message("[divaodv] Initialising Julia + DIVAnd (first call this session)...")

  tryCatch({
    JuliaCall::julia_setup()
    JuliaCall::julia_library("DIVAnd")
    JuliaCall::julia_eval("using Statistics")
    JuliaCall::julia_eval("using Dates")
    .diva_env$julia_ready <- TRUE
    message("[divaodv] Julia ready.")
  }, error = function(e) {
    stop("Failed to initialise Julia or load DIVAnd.\n",
         "  Original error: ", conditionMessage(e), "\n",
         "  Run diva_setup_check() for diagnostic help.",
         call. = FALSE)
  })

  invisible(TRUE)
}
