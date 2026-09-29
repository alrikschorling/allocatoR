# Set up
# Load packages
# Declared explicitly rather than via pacman::p_load() so that dependencies are
# discoverable by renv/rsconnect and nothing is installed at runtime on the
# server. ggpubr was declared previously but never used.
library(shiny)          # app framework
library(shinyFeedback)  # inline input validation
library(shinythemes)    # bootswatch themes
library(shinyjs)        # enable/disable controls
library(DT)             # interactive tables
library(dplyr)          # data manipulation
library(tidyr)          # pivot_longer()
library(ggplot2)        # plotting
library(ggridges)       # geom_density_ridges()
library(anticlust)      # balanced group allocation
library(rstatix)        # statistical tests
library(ggprism)        # add_pvalue()

#set themes and palettes
basic_theme <- theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(linewidth = 0.5),
        axis.ticks.length = unit(.2, "cm"), 
        axis.text.x = element_text(color = "black", family = "sans", size = 12),
        axis.text.y = element_text(color = "black", family = "sans", size = 12),
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12), 
        strip.background = element_rect(color = NA, fill = NA), 
        plot.title = element_text(color = "black", family = "sans", size = 12))

theme_1 <- basic_theme + theme(axis.line = element_line(linewidth = 0.5))

theme_2 <- basic_theme + 
  theme(axis.line = element_line(linewidth = 0.3), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#make custom color palette
pal <- c("#B5B5B5", "#80607E", "#64D1C5", "#D1B856", "#8F845B", "#5C7C78", "#524410", "#470C43")
pal2 <- c("#5EBF92", "#F2E205", "#F2B705", "#BF7E04", "#F23838")
expanded_pal <- colorRampPalette(pal)(50)
expanded_pal2 <- colorRampPalette(pal2)(50)


#subsample a colour ramp down to the number of factor levels actually present.
#Defaults to expanded_pal2, which is what the app used before this was
#de-duplicated; pass `palette` explicitly to draw from the other ramp.
get_subsampled_palette <- function(n, palette = expanded_pal2) {
  palette[round(seq(1, length(palette), length.out = n))]
}

#generate example data that matches the format users should upload
example_table <- data.frame(
  rat_id = c("1", "2", "3"),
  test1 = c(250, 245, 260),
  test2 = c(248, 244, 258)
)



# UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    tags$style(HTML("
      .btn-spacing {
        margin-bottom: 5px; margin-right: 5px;
      }
      .container-fluid {
        max-width: 1600px;  /* Set maximum width for the window */
      }
      .progress {
      width: 100% !important;  /* Make the progress bar 100% width */
      height: 30px !important;  /* Adjust the height of the progress bar */
    }
    .progress-bar {
      font-size: 14px !important;  /* Increase the font size */
      line-height: 30px !important;  /* Match the line height to the bar height to center text */
      text-align: center !important;  /* Ensure the text is centered horizontally */
    }
    "))
  ),
  useShinyFeedback(),
  useShinyjs(),
  
  titlePanel("allocatoR - animal group allocator"),
  
  fluidRow(column(4, div(
    #user input csv files 
    fileInput("file1", "Choose CSV file", 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

    #help text
    helpText(HTML("<p><strong>Please upload a CSV file with the following specifications:</strong></p>",
                  
                  "<ul>",
                  "<li>The first column must be 'rat_id', given as a number (see example below).</li>",
                  "<li>The file should use <strong>DOTS</strong> as decimal separators (e.g., 0.5, not 0,5).</li>",
                  "<li>The file should use <strong>COMMAS</strong> as cell separators.</li>",
                  "</ul>",
                  
                  "<p>If not, the following error message will appear:</p>",
                  "<p><code>Error in read.table(file = file, header = header, sep = sep, quote = quote, :<br>
  more columns than column names</code></p>",
                  
                  "<p><strong>Remaining columns:</strong> should contain the behavioral data (see below).</p>",
                  
                  "<p><strong>Allocation Method:</strong></p>",
                  "<ul>",
                  "<li>The allocation aims to minimize the variance and standard deviation between groups.</li>",
                  "<li>It uses the <code>anticlust</code> package with the following parameters:</li>",
                  "<ul>",
                  "<li><code>objective = kplus</code></li>",
                  "<li><code>standardize = TRUE</code></li>",
                  "<li><code>method = local-maximum</code></li>",
                  "</ul>",
                  "</ul>",
                  
                  "<p><strong>Statistical Tests:</strong> The app will perform normality and homoskedasticity tests, which will determine the downstream descriptive statistical test used.</p>",
                  
                  "<p><strong>Example Table:</strong></p>")),
    
    tableOutput("exampleTable"),
    
    #output number of rats (to simplify for group size determination)
    verbatimTextOutput("num_rats"),  # Display unique number of rat_id
             
    #user input
    numericInput("num_groups", "Number of groups for anticlustering", 2),
    textInput("group_names_allocation", "Group names (comma-separated)", 
              placeholder = "Eg. Lesion, Control"),  # Comma-separated input
    textInput("group_sizes", "Number of rats in each group (comma-separated)", 
              placeholder = "E.g. 5,5"),  # Comma-separated input for sizes
    selectInput("set_seed", "Set seed (for reproducibility)", 
                choices = c(123, 69, 111), selected = 123),
    
    #action buttons
    actionButton("process_data", "Process Data", class = "btn-primary"),
    downloadButton("downloadData1", "Download Fig. 1", class = "btn-spacing"),  
    downloadButton("downloadData2", "Download Fig. 2", class = "btn-spacing"),
    downloadButton("downloadData3", "Download Fig. 3", class = "btn-spacing"),
    downloadButton("downloadAllocation", "Download allocation", class = "btn-spacing"))
    ),
    
    column(8,
           verbatimTextOutput("shapiro_result"), #shapiro-wilk test result
           verbatimTextOutput("levene_result"), #levene's test result
           verbatimTextOutput("testUsed"),  #output the statistial test used
           plotOutput("plot1"),
           plotOutput("plot2"),
           plotOutput("plot3"),
           DTOutput("allocation_table")
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  #display the example table
  output$exampleTable <- renderTable({
    example_table
  })
  
  #Download buttons stay disabled until the data has actually been processed.
  #These must be driven by an observer -- calling enable() at the top level of
  #server() fires once at session start, immediately undoing the disable.
  download_buttons <- c("downloadData1", "downloadData2",
                        "downloadData3", "downloadAllocation")
  lapply(download_buttons, shinyjs::disable)
  observeEvent(processed_data(), lapply(download_buttons, shinyjs::enable))
  
  #function to read and process CSV files
  df <- reactive({
    req(input$file1)
    files <- input$file1
    df <- read.csv(files$datapath) |> 
      rename_with(tolower) |>
      mutate(rat_id = factor(rat_id)) 
  })
  
  #display unique number of rat_id
  output$num_rats <- renderText({
    paste("Number of unique rats IDs:", length(unique(df()$rat_id)))
  })
  
  #reactive to process anticlustering after user input
  processed_data <- eventReactive(input$process_data, {
    
    #validate user inputs
    group_names <- strsplit(input$group_names_allocation, ",\\s*")[[1]]
    group_sizes <- as.numeric(strsplit(input$group_sizes, ",\\s*")[[1]])
    
    #set the seed (for reproducibility)
    set.seed(input$set_seed)
    
    #validate that group sizes and number of names matches the input number of groups
    validate(
      need(length(group_names) == input$num_groups, "Number of group names doesn't match the number of groups."),
      need(length(group_sizes) == input$num_groups, "Group sizes don't match the number of groups."),
      need(sum(group_sizes) == nrow(df()), "The total size of all groups doesn't match the number of animals.")
    )
    
    #create a new dataframe to store the group allocation
    df_with_groups <- df()
    
    #perform anticlustering
    df_with_groups$group <- factor(anticlustering(
      df_with_groups[, -1], #exclude the rat_id column
      K = group_sizes,  # User-defined group sizes
      objective = "kplus", #anticlust algorithm (minimize variance and standard deviation between groups)
      standardize = TRUE, #scale the variables
      method = "local-maximum" #algorithm endpoint: local maximum method 
    ), labels = group_names)  #apply user-defined group names
    
    df_with_groups
  })
  
  #convert to long format
  df_long <- reactive({
    processed_data() |> pivot_longer(!c(rat_id, group), names_to = 'test', values_to = 'vals') |> 
      mutate(test = factor(test)) |> ungroup()
  })
  
  # ---- Assumption checks -------------------------------------------------
  # Run PER behavioural test. Pooling every test into one vector compares a
  # mixture of variables measured on different scales, which rejects normality
  # for reasons that have nothing to do with the data.

  #shapiro-wilk test for normality, one per behavioural test
  shapiro <- reactive({
    df_long() |> group_by(test) |> shapiro_test(vals) |> ungroup()
  })

  output$shapiro_result <- renderText({
    res <- shapiro()
    paste0(
      "Shapiro-Wilk test for normality (per behavioural test)\n",
      paste(sprintf("  %-10s W = %.3f, p = %.4f%s",
                    res$test, res$statistic, res$p,
                    ifelse(res$p < 0.05, "  *", "")), collapse = "\n"),
      "\n  * p < 0.05: departs from normality"
    )
  })

  #levene's test for homogeneity of variance, one per behavioural test
  levene <- reactive({
    df_long() |> group_by(test) |> levene_test(vals ~ group) |> ungroup()
  })

  output$levene_result <- renderText({
    res <- levene()
    paste0(
      "Levene's test for homogeneity of variance (per behavioural test)\n",
      paste(sprintf("  %-10s F(%d, %d) = %.3f, p = %.4f%s",
                    res$test, res$df1, res$df2, res$statistic, res$p,
                    ifelse(res$p < 0.05, "  *", "")), collapse = "\n"),
      "\n  * p < 0.05: unequal variance between groups"
    )
  })

  #Summarise assumptions across behavioural tests. One test family is applied
  #to every panel, so the strictest result governs: if ANY behavioural test
  #violates an assumption, the more conservative method is used.
  assumptions <- reactive({
    list(normal        = all(shapiro()$p >= 0.05),
         homoskedastic = all(levene()$p  >= 0.05))
  })

  #decide which downstream test to run
  stat_choice <- reactive({
    a <- assumptions()
    k <- length(unique(df_long()$group))

    if      (!a$normal && k > 2)  "dunn"
    else if (!a$normal)           "wilcox"
    else if (!a$homoskedastic)    "welch"
    else                          "student"
  })

  test_labels <- c(
    dunn    = "Dunn's non-parametric multiple comparison test (BH-adjusted)",
    wilcox  = "Wilcoxon rank-sum test (non-parametric)",
    welch   = "Welch's t-test (unequal variance)",
    student = "Student's t-test (equal variance)"
  )

  #conditional statistics
  sts <- reactive({
    d <- df_long() |> group_by(test)

    switch(
      stat_choice(),
      dunn    = d |> dunn_test(vals ~ group, p.adjust.method = "BH"),
      wilcox  = d |> wilcox_test(vals ~ group, paired = FALSE),
      welch   = d |> t_test(vals ~ group, var.equal = FALSE),
      #rstatix::t_test defaults to var.equal = FALSE, so state it explicitly
      student = d |> t_test(vals ~ group, var.equal = TRUE)
    ) |> add_xy_position()
  })

  output$testUsed <- renderText({
    paste("Statistical test used:", test_labels[[stat_choice()]])
  })
  
  
  #plot 1
  reactivePlot1 <- reactive({
    ggplot(df_long(), aes(vals, group)) + 
      geom_density_ridges(aes(fill = group)) +
      scale_fill_manual(values = pal) +
      labs(title = "Fig 1. Behavioral data distribution", x = "Values", y = "Group") +
      theme_1
  })
  
  #display plot1
  output$plot1 <- renderPlot({
    print(reactivePlot1())
  })
  
  
  #plot2
  #The two former branches differed only in which p-value column to label with:
  #rstatix reports an adjusted p ("p.adj") for >2 groups and a raw p otherwise.
  reactivePlot2 <- reactive({
    p_col <- if (length(unique(df_long()$group)) > 2) "p.adj" else "p"

    ggplot(df_long(), aes(group, vals)) +
      geom_violin(aes(fill = group)) +
      geom_point(position = position_jitter(width = 0.2)) +
      #free_y: the behavioural tests are on different scales, so a shared axis
      #flattens the ones with the smaller range into an unreadable strip
      facet_wrap(~test, scales = "free_y") +
      scale_fill_manual(values = pal) +
      #headroom at the top so the significance brackets are not clipped
      scale_y_continuous(expand = expansion(mult = c(0.08, 0.18))) +
      add_pvalue(sts(), label = p_col, bracket.size = 0.4, label.size = 3) +
      labs(title = "Fig 2. Behavioral data group comparison") +
      theme_2
  })

  #display plot2
  output$plot2 <- renderPlot({
    print(reactivePlot2())
  })

  
  
  #plot3
  reactivePlot3 <- reactive({
    ggplot(df_long(), aes(group, vals, col = rat_id)) + 
      geom_point() + 
      scale_color_manual(values = get_subsampled_palette(length(unique(df_long()$rat_id)))) +
      facet_wrap(~test) +
      labs(title = "Fig 3. Behavioral data per animal") +
      theme_1
  })
  
  #display plot3
  output$plot3 <- renderPlot({
    print(reactivePlot3())
  })
  
  
  #display group allocation table
  output$allocation_table <- renderDT({
    processed_data() |> relocate(rat_id, group) |> arrange(group, rat_id) |>
      datatable(editable = TRUE,
                filter = 'top',
                options = list(pageLength = 5)) 
  })
  
  #download group allocation as CSV
  output$downloadAllocation <- downloadHandler(
    filename = function() {
      paste("group_allocation_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data() %>% select(rat_id, group), file, row.names = FALSE)
    }
  )
  
  #dynamically adjust the plot download dimensions
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Fig_1_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p1 <- isolate(reactivePlot1())
      g <- ggplot_build(p1)
      
      plot_width <- 4
      plot_height <- length(unique(df_long()$group)) * 1.5
      
      pdf(file, width = plot_width, height = plot_height)  # Use dynamic size for the plot
      print(p1)
      dev.off()
    }
  )
  
  #dynamically adjust the plot download dimensions
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Fig_2_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p2 <- isolate(reactivePlot2())  # Correctly isolate plot 2
      g <- ggplot_build(p2)  # Get the plot build object
      
      plot_width <- length(unique(df_long()$test)) * 1.5
      plot_height <- length(unique(df_long()$test)) * 1.5
      
      pdf(file, width = plot_width, height = plot_height)
      print(p2)
      dev.off()
    }
  )
  
  #dynamically adjust the plot download dimensions
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("Fig_3_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      p3 <- isolate(reactivePlot3())
      g <- ggplot_build(p3)
      
      # Dynamically adjust the plot size
      plot_width <- length(unique(df_long()$test)) * 1.5
      plot_height <- length(unique(df_long()$test)) * 1.5
      
      pdf(file, width = plot_width, height = plot_height)
      print(p3)
      dev.off()
    }
  )
  
}



# Run the application
shinyApp(ui = ui, server = server)