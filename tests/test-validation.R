# Upload validation

app <- shinyAppDir(".")
tmp <- tempfile(fileext = ".csv")
on.exit(unlink(tmp), add = TRUE)

read_with <- function(text) {
  writeLines(text, tmp)
  out <- NULL
  testServer(app, {
    session$setInputs(file1 = upload(tmp))
    out <<- df()
  })
  out
}

check_error("rejects a file with no rat_id column",
            read_with(c("animal,test1", "1,5", "2,6")),
            "The first column must be named 'rat_id'")

check_error("rejects a non-numeric measure column",
            read_with(c("rat_id,test1,sex", "1,5,M", "2,6,F")),
            "must be numeric")

check_error("rejects duplicate ids",
            read_with(c("rat_id,test1", "1,5", "1,6")),
            "rat_id values must be unique")

check_error("rejects missing values",
            read_with(c("rat_id,test1", "1,5", "2,")),
            "missing values")

check_error("rejects a file with no measures at all",
            read_with(c("rat_id", "1", "2")),
            "at least one behavioural measure")

ok <- read_with(c("rat_id,test1,test2", "1,5,10", "2,6,11", "3,7,12"))
check("accepts a well-formed file", nrow(ok) == 3 && ncol(ok) == 3)
check("column names are lower-cased", identical(names(ok), c("rat_id", "test1", "test2")))
check("rat_id becomes a factor", is.factor(ok$rat_id))
