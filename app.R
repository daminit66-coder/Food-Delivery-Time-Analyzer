# Food Delivery Time and Service Analyzer
# Sample size: 100 simulated orders
# Run with: shiny::runApp()

library(shiny)
library(ggplot2)
library(dplyr)

# -----------------------------
# Generate 100 sample orders
# -----------------------------
set.seed(42)

n_orders <- 100

df <- data.frame(
  Order_ID = 1:n_orders,
  Distance = round(runif(n_orders, min = 1, max = 12), 1),
  Area = sample(
    c("Urban", "Suburban", "Rural"),
    size = n_orders,
    replace = TRUE,
    prob = c(0.45, 0.35, 0.20)
  )
)

# Simulate delivery times based on distance and area.
# Values are fictional and intended for demonstrating statistics.
area_delay <- ifelse(df$Area == "Rural", 5,
                     ifelse(df$Area == "Suburban", 2, 0))
df$Delivery_Time <- round(
  pmax(8, 14 + 2.0 * df$Distance + area_delay +
         rnorm(n_orders, mean = 0, sd = 5)),
  1
)

# Rating is simulated from delivery experience.
df$Rating <- ifelse(
  df$Delivery_Time <= 30, "Good", "Poor"
)

# -----------------------------
# User interface
# -----------------------------
ui <- fluidPage(
  titlePanel("Food Delivery Time and Service Analyzer"),
  sidebarLayout(
    sidebarPanel(
      h4("Sample dataset"),
      p("This app uses 100 simulated food-delivery orders."),
      actionButton("refresh", "Generate new sample data"),
      hr(),
      h4("Predict delivery time"),
      sliderInput(
        "distance_input",
        "Delivery distance (km):",
        min = 1, max = 12, value = 5, step = 0.5
      ),
      uiOutput("prediction_result"),
      hr(),
      p("Significance level: 0.05")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          fluidRow(
            column(4, wellPanel(h4("Orders"), textOutput("n_orders"))),
            column(4, wellPanel(h4("Average delivery"), textOutput("avg_time"))),
            column(4, wellPanel(h4("Over 30 minutes"), textOutput("late_pct")))
          ),
          h4("Sample data (first 10 orders)"),
          tableOutput("sample_table"),
          plotOutput("delivery_hist")
        ),
        tabPanel(
          "1. Z-test (P4)",
          br(),
          h4("Does the average delivery time exceed 30 minutes?"),
          p("H0: population mean is 30 minutes; H1: population mean is greater than 30 minutes."),
          verbatimTextOutput("z_result"),
          plotOutput("z_plot")
        ),
        tabPanel(
          "2. Regression (P6)",
          br(),
          h4("Predict delivery time from distance"),
          verbatimTextOutput("reg_result"),
          h4("Prediction for selected distance"),
          textOutput("prediction_result"),
          plotOutput("reg_plot")
        ),
        tabPanel(
          "3. Sign test (P8)",
          br(),
          h4("Is the median delivery time greater than 30 minutes?"),
          p("Orders exactly at 30 minutes are excluded from the sign test."),
          verbatimTextOutput("sign_result"),
          plotOutput("sign_plot")
        ),
        tabPanel(
          "4. Rating vs Area (P7)",
          br(),
          h4("Are rating and area independent?"),
          tableOutput("rating_table"),
          verbatimTextOutput("chi_result"),
          plotOutput("rating_plot")
        ),
        tabPanel(
          "5. Sampling distribution (P2)",
          br(),
          h4("Sampling distribution of mean delivery time"),
          p("Each simulated sample contains 1,000 delivery times drawn with replacement. The histogram uses 1,000 sample means."),
          verbatimTextOutput("sampling_result"),
          plotOutput("sampling_plot")
        )
      )
    )
  )
)

# -----------------------------
# Server logic
# -----------------------------
server <- function(input, output, session) {

  # Reactive data lets the user generate another sample.
  orders <- eventReactive(input$refresh, {
    set.seed(as.integer(Sys.time()))
    new_df <- data.frame(
      Order_ID = 1:n_orders,
      Distance = round(runif(n_orders, 1, 12), 1),
      Area = sample(
        c("Urban", "Suburban", "Rural"),
        n_orders, replace = TRUE,
        prob = c(0.45, 0.35, 0.20)
      )
    )
    area_delay <- ifelse(new_df$Area == "Rural", 5,
                         ifelse(new_df$Area == "Suburban", 2, 0))
    new_df$Delivery_Time <- round(
      pmax(8, 14 + 2.0 * new_df$Distance + area_delay +
             rnorm(n_orders, 0, 5)),
      1
    )
    new_df$Rating <- ifelse(new_df$Delivery_Time <= 30, "Good", "Poor")
    new_df
  }, ignoreNULL = FALSE)

  output$n_orders <- renderText(nrow(orders()))
  output$avg_time <- renderText(
    paste0(round(mean(orders()$Delivery_Time), 1), " min")
  )
  output$late_pct <- renderText(
    paste0(round(mean(orders()$Delivery_Time > 30) * 100, 1), "%")
  )

  output$sample_table <- renderTable(head(orders(), 10))
  output$delivery_hist <- renderPlot({
    ggplot(orders(), aes(x = Delivery_Time)) +
      geom_histogram(bins = 15, color = "white") +
      geom_vline(xintercept = 30, linetype = "dashed") +
      labs(
        title = "Distribution of delivery times",
        subtitle = "Dashed line = 30-minute promise",
        x = "Delivery time (minutes)", y = "Number of orders"
      ) +
      theme_minimal()
  })

  # Task 1: Large-sample one-sided z-test approximation.
  # A classical z-test assumes known population SD. Here we use sample SD
  # as a large-sample approximation and state that limitation.
  output$z_result <- renderPrint({
    x <- orders()$Delivery_Time
    n <- length(x)
    xbar <- mean(x)
    s <- sd(x)
    z <- (xbar - 30) / (s / sqrt(n))
    p <- pnorm(z, lower.tail = FALSE)
    cat("Sample size:", n, "\n")
    cat("Sample mean:", round(xbar, 3), "minutes\n")
    cat("Sample SD:", round(s, 3), "minutes\n")
    cat("Z statistic:", round(z, 4), "\n")
    cat("One-sided p-value:", signif(p, 5), "\n\n")
    if (p < 0.05) {
      cat("Decision at alpha = 0.05: Reject H0.\n",
          "There is evidence that the population mean delivery time exceeds 30 minutes.\n")
    } else {
      cat("Decision at alpha = 0.05: Do not reject H0.\n",
          "There is insufficient evidence that the population mean exceeds 30 minutes.\n")
    }
    cat("\nNote: This is a large-sample z approximation using sample SD. ",
        "If population SD is unknown, a one-sample t-test is usually preferred.")
  })

  output$z_plot <- renderPlot({
    x <- orders()$Delivery_Time
    z <- (mean(x) - 30) / (sd(x) / sqrt(length(x)))
    p <- pnorm(z, lower.tail = FALSE)
    curve(dnorm(x), from = -4, to = 4,
          main = paste0("Standard normal curve; z = ", round(z, 2),
                        ", p = ", signif(p, 3)),
          xlab = "Z value", ylab = "Density")
    abline(v = z, lty = 2)
  })

  # Task 2: Linear regression.
  model <- reactive(lm(Delivery_Time ~ Distance, data = orders()))

  output$reg_result <- renderPrint({
    print(summary(model()))
    cat("\nRegression equation:\n")
    b <- coef(model())
    cat("Predicted time =", round(b[1], 2), "+",
        round(b[2], 2), "* distance (km)\n")
  })

  
  # Predict delivery time whenever the distance changes
  output$prediction_result <- renderUI({
    req(input$distance_input)
    
    model <- lm(Delivery_Time ~ Distance, data = orders())
    
    pred <- predict(
      model,
      newdata = data.frame(Distance = input$distance_input),
      interval = "prediction"
    )
    
    tagList(
      h3(paste0(round(pred[1, "fit"], 1), " minutes")),
      p(paste0(
        "Selected distance: ", input$distance_input, " km"
      )),
      p(paste0(
        "95% prediction interval: ",
        round(pred[1, "lwr"], 1), " to ",
        round(pred[1, "upr"], 1), " minutes"
      ))
    )
  })
  

  output$prediction_result <- renderText({
    pred <- predict(
      model(),
      newdata = data.frame(Distance = input$distance_input),
      interval = "prediction"
    )
    paste0(
      "At ", input$distance_input, " km, predicted delivery time is ",
      round(pred[1, "fit"], 1), " minutes. Approximate 95% prediction interval: ",
      round(pred[1, "lwr"], 1), " to ", round(pred[1, "upr"], 1), " minutes."
    )
  })

  output$reg_plot <- renderPlot({
    ggplot(orders(), aes(x = Distance, y = Delivery_Time)) +
      geom_point(aes(color = Area), alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      labs(
        title = "Delivery time vs distance",
        x = "Distance (km)", y = "Delivery time (minutes)",
        color = "Area"
      ) +
      theme_minimal()
  })

  # Task 3: Exact sign test for median > 30.
  output$sign_result <- renderPrint({
    x <- orders()$Delivery_Time
    above <- sum(x > 30)
    below <- sum(x < 30)
    ties <- sum(x == 30)
    result <- binom.test(
      above, above + below,
      p = 0.5, alternative = "greater"
    )
    cat("Orders above 30 minutes:", above, "\n")
    cat("Orders below 30 minutes:", below, "\n")
    cat("Ties at exactly 30 minutes (excluded):", ties, "\n")
    cat("Effective sample size:", above + below, "\n\n")
    print(result)
    if (result$p.value < 0.05) {
      cat("\nConclusion: Evidence that the population median is greater than 30 minutes.\n")
    } else {
      cat("\nConclusion: Insufficient evidence that the population median is greater than 30 minutes.\n")
    }
  })

  output$sign_plot <- renderPlot({
    sign_counts <- data.frame(
      Category = c("Below 30", "Exactly 30", "Above 30"),
      Orders = c(
        sum(orders()$Delivery_Time < 30),
        sum(orders()$Delivery_Time == 30),
        sum(orders()$Delivery_Time > 30)
      )
    )
    ggplot(sign_counts, aes(x = Category, y = Orders)) +
      geom_col() +
      labs(title = "Delivery times relative to 30 minutes",
           x = NULL, y = "Number of orders") +
      theme_minimal()
  })

  # Task 4: Chi-square independence test.
  output$rating_table <- renderTable({
    as.data.frame.matrix(table(orders()$Rating, orders()$Area))
  }, rownames = TRUE)

  output$chi_result <- renderPrint({
    tab <- table(orders()$Rating, orders()$Area)
    test <- suppressWarnings(chisq.test(tab, correct = FALSE))
    cat("Observed contingency table:\n")
    print(tab)
    cat("\nChi-square test:\n")
    print(test)
    if (any(test$expected < 5)) {
      cat("\nWarning: Some expected cell counts are below 5. ",
          "Consider Fisher's exact test or combining categories if justified.\n")
    }
    if (test$p.value < 0.05) {
      cat("\nConclusion: Evidence of an association between rating and area.\n")
    } else {
      cat("\nConclusion: Insufficient evidence of an association between rating and area.\n")
    }
  })

  output$rating_plot <- renderPlot({
    ggplot(orders(), aes(x = Area, fill = Rating)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      labs(title = "Rating proportions by area",
           x = "Area", y = "Share of orders", fill = "Rating") +
      theme_minimal()
  })

  # Task 5: Sample size n = 1000; 1000 repetitions.
  sample_means <- reactive({
    replicate(
      1000,
      mean(sample(orders()$Delivery_Time, size = 1000, replace = TRUE))
    )
  })

  output$sampling_result <- renderPrint({
    means <- sample_means()
    cat("Source dataset size:", nrow(orders()), "\n")
    cat("Sample size for each repetition: 1000\n")
    cat("Number of repetitions: 1000\n")
    cat("Mean of sample means:", round(mean(means), 3), "\n")
    cat("Standard deviation of sample means:", round(sd(means), 3), "\n")
    cat("Theoretical standard error using sample SD:",
        round(sd(orders()$Delivery_Time) / sqrt(1000), 3), "\n")
  })

  output$sampling_plot <- renderPlot({
    means <- sample_means()
    ggplot(data.frame(Sample_Mean = means), aes(x = Sample_Mean)) +
      geom_histogram(bins = 25, color = "white") +
      geom_vline(
        xintercept = mean(orders()$Delivery_Time),
        linetype = "dashed"
      ) +
      labs(
        title = "Sampling distribution of mean delivery time",
        subtitle = "Each sample contains 1,000 orders; 1,000 repetitions",
        x = "Sample mean (minutes)", y = "Frequency"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
