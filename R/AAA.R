### Called when package is loaded

# Create cache environment ------------------------------------------------

.BEMOVI_CACHE <- new.env(FALSE, parent = globalenv())

.onLoad <- function(lib, pkg) {
}

