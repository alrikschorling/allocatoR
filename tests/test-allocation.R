# Allocation and the figures

app  <- shinyAppDir(".")
data <- "test_data.csv"

allocate <- function(seed = 123, k = 2, names = "Lesion, Control", sizes = "15,15") {
  out <- NULL
  testServer(app, {
    session$setInputs(file1 = upload(data), num_groups = k,
                      group_names_allocation = names, group_sizes = sizes,
                      set_seed = seed)
    session$setInputs(process_data = 1)
    out <<- processed_data()
  })
  out
}

d <- allocate()

check("reads the bundled example data", nrow(d) == 30)
check("keeps every measure column", ncol(d) == 7)   # rat_id + 5 measures + group
check("splits into the requested group sizes",
      identical(unname(as.integer(table(d$group))), c(15L, 15L)))
check("uses the group names given", identical(levels(d$group), c("Lesion", "Control")))
check("assigns every animal", !anyNA(d$group))

# Reproducibility
map <- function(x) setNames(as.character(x$group), as.character(x$rat_id))[order(as.integer(as.character(x$rat_id)))]
check("same seed reproduces the same allocation",
      identical(map(allocate(seed = 123)), map(allocate(seed = 123))))
check("a different seed changes the allocation",
      !identical(map(allocate(seed = 69)), map(allocate(seed = 123))))
check("the seed accepts any integer, not just the old three choices",
      !is.null(allocate(seed = 7)))

# Anticlustering should leave the groups comparable on every measure
long <- reshape(as.data.frame(d[, setdiff(names(d), "rat_id")]),
                direction = "long", varying = list(1:5), v.names = "v",
                timevar = "measure", times = names(d)[2:6])
worst <- max(vapply(split(long, long$measure), function(m) {
  mu <- tapply(m$v, m$group, mean); abs(diff(range(mu))) / mean(mu)
}, numeric(1)))
check("group means differ by under 5% on every measure",
      worst < 0.05, sprintf("worst was %.1f%%", 100 * worst))

# Figures and downloads
testServer(app, {
  session$setInputs(file1 = upload(data), num_groups = 2,
                    group_names_allocation = "Lesion, Control",
                    group_sizes = "15,15", set_seed = 123)
  session$setInputs(process_data = 1)
  for (p in c("reactivePlot1", "reactivePlot2", "reactivePlot3"))
    check(paste(p, "builds"), inherits(ggplot2::ggplot_build(get(p)()), "ggplot_built"))
  check("all three figures download as valid PDFs",
        all(vapply(c("downloadData1","downloadData2","downloadData3"),
                   function(n) is_pdf(output[[n]]), logical(1))))
  check("allocation downloads as CSV",
        grepl("^group_allocation_", basename(output$downloadAllocation)))

  # Regression: Fig 2 used to force every facet onto one y-axis scaled to the
  # global maximum, flattening the measures with a smaller range.
  panels <- ggplot2::ggplot_build(reactivePlot2())$layout$panel_params
  ranges <- vapply(panels, function(p) diff(p$y.range), numeric(1))
  check("Fig 2 gives each measure its own y scale",
        length(unique(round(ranges, 6))) > 1)
})
