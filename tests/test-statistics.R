# Assumption checks and test selection

app  <- shinyAppDir(".")
data <- "test_data.csv"

with_app <- function(capture, seed = 123) {
  out <- NULL
  testServer(app, {
    session$setInputs(file1 = upload(data), num_groups = 2,
                      group_names_allocation = "Lesion, Control",
                      group_sizes = "15,15", set_seed = seed)
    session$setInputs(process_data = 1)
    out <<- capture(environment())
  })
  out
}

sh <- with_app(function(e) get("shapiro", envir = e)())
lv <- with_app(function(e) get("levene",  envir = e)())

# Regression: both tests used to run on every measure pooled into one vector.
# The measures sit on different scales, so that tests a mixture distribution.
# On this very file the pooled Shapiro-Wilk gives p = 1.9e-06 while each
# measure on its own is comfortably normal (p = 0.32 to 0.82).
check("normality is tested per measure, not pooled", nrow(sh) == 5)
check("equal variance is tested per measure, not pooled", nrow(lv) == 5)
check("every measure is individually normal", all(sh$p >= 0.05))
check("the pooled test would have disagreed", {
  pooled <- shapiro.test(read.csv(data)[, -1] |> as.matrix() |> as.vector())$p.value
  pooled < 0.05
})

# With normal, homoskedastic data and two groups the choice must be Student's
choice <- with_app(function(e) get("stat_choice", envir = e)())
check("chooses Student's t-test for this data", identical(choice, "student"))

# Regression: rstatix::t_test defaults to var.equal = FALSE, so the branch
# labelled "equal variance" was silently running Welch.
sts <- with_app(function(e) get("sts", envir = e)())
check("one comparison per measure", nrow(sts) == 5)
# Student's t-test uses n1 + n2 - 2 degrees of freedom exactly; Welch's uses a
# fractional Satterthwaite approximation. The p-values happen to agree on this
# data, so the degrees of freedom are what distinguish the two.
check("Student's t-test is actually run, not Welch's",
      all(sts$df == sts$n1 + sts$n2 - 2),
      paste("df were", paste(round(sts$df, 3), collapse = ", ")))

# Anticlustered groups should not differ
check("p-values are large, as expected after anticlustering", all(sts$p > 0.05))
