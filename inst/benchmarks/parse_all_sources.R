# Parse-check all package R source and test files before loading the package.
# Run from the package root with:
#   source("inst/benchmarks/parse_all_sources.R")
files <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("tests", pattern = "\\.R$", full.names = TRUE,
             recursive = TRUE)
)
if (!length(files)) stop("No package R files found. Run from the project root.")
for (f in files) {
  tryCatch(parse(file = f, keep.source = TRUE), error = function(e) {
    stop(sprintf("Parse error in %s: %s", f, conditionMessage(e)),
         call. = FALSE)
  })
}
cat(sprintf("Successfully parsed %d R files.\n", length(files)))
