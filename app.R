# Load required libraries
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(lmerTest)
library(lme4)
library(broom.mixed)
library(purrr)

# Define UI
ui <- page_fillable(
  theme = bs_theme(
    primary = "#174717",
    bg = "#F2F2F2",
    fg = "#267326",
    base_font = font_google("Alegreya Sans"),
    heading_font = font_google("Alegreya Sans")
  ),

  # Add custom CSS
  tags$head(
    tags$style(HTML("
      .card-header {
        background-color: #8cd000;
        color: #174717;
        font-weight: bold;
      }
      .card {
        margin-bottom: 15px;
      }
    "))
  ),


  # Layout
  layout_column_wrap(
    width = 1/3,

    # Parameters panel
    card(
      card_header("Simulation Parameters"),
      card_body(
        sliderInput("icc", "Intraclass Correlation (ICC)",
                    min = 0, max = 0.5, value = 0.2, step = 0.05),
        sliderInput("students_per_class", "Measurements per Person",
                    min = 5, max = 50, value = 20, step = 5),
        sliderInput("num_classes", "Number of Persons (Clusters)",
                    min = 5, max = 50, value = 20, step = 5),
        selectInput(
          "randomization",
          "Randomization",
          list("Between Person" = "b", "Within Person" = "w")
        ),
        sliderInput("num_simulations", "Number of Simulations",
                    min = 100, max = 2000, value = 100, step = 20),
        actionButton("run_sim", "Run Simulation",
                     class = "btn-lg btn-primary", width = "100%",
                     style = "background-color: #174717; border-color: #174717;")
      )
    ),


    # Example Panel

    card(
      full_screen = TRUE,
      card_header("Example Dataset Visualization"),
      card_body(
        plotOutput("example_plot", height = "300px")
      )
    ),

    # Results panel
    layout_column_wrap(
      width = 1,
      card(
        full_screen = TRUE,
        card_header("Type I Error Rate Results"),
        card_body(
          plotOutput("alpha_plot", height = "300px")
        )
      ),

      card(
        full_screen = TRUE,
        card_header("Distribution of p-values"),
        card_body(
          plotOutput("pvalue_dist", height = "300px")
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {

  # Generate example dataset for visualization
  example_data <- reactive({

    # Get parameters
    icc <- input$icc
    students_per_class <- input$students_per_class
    num_classes <- input$num_classes

    total_var <- 1
    between_var <- icc * total_var
    within_var <- (1 - icc) * total_var
    n_total <- num_classes * students_per_class
    cluster_ids <- rep(1:num_classes, each = students_per_class)
    cluster_effects <- rep(rnorm(num_classes, 0, sqrt(between_var)), each = students_per_class)

    # Assign treatment (balanced design)
    # Half of the clusters get treatment, half get control

    if (input$randomization == "b") {
      # Randomize within classes
      treatment <- rep(c(0, 1), each = n_total/2)
    } else {
      # Randomize between classes
      treatment <- sample(c(0, 1), size = n_total, replace = TRUE)
    }

    # Generate individual effects
    individual_effects <- rnorm(n_total, 0, sqrt(within_var))

    # Generate outcomes (null hypothesis is true - no treatment effect)
    outcome <- cluster_effects + individual_effects

    # Create dataset
    data.frame(
      class_id = cluster_ids,
      Treatment = factor(treatment),
      outcome = outcome
    )
  })

  # Plot example dataset
  output$example_plot <- renderPlot({
    data <- example_data()

    ggplot(data, aes(y = factor(class_id), x = outcome,
                     fill = Treatment,
                     color = Treatment)) +
      geom_boxplot() +
      labs(
        title = "Example Data",
        subtitle = paste("ICC =",
                         round(psychometric::ICC1.lme(outcome,
                                                      class_id,
                                                      example_data()),
                               2),
                         "; DEFT = ",
                         round(sqrt(1 + (input$students_per_class -1) *
                                      psychometric::ICC1.lme(outcome,
                                                             class_id,
                                                               data)),
                               2)),
        y = "Class ID",
        x = "Outcome"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("0" = "#8cd000", "1" = "#267326")) +
      scale_color_manual(values = c("0" = "#8cd000", "1" = "#267326")) +
      theme(
        text = element_text(family = "Alegreya Sans"),
        plot.title = element_text(face = "bold", color = "#174717"),
        plot.subtitle = element_text(color = "#267326"),
        legend.position = "bottom",
      )
  })

  # Run simulation when button is clicked
  sim_results <- eventReactive(input$run_sim, {
    # Get parameters
    icc <- input$icc
    students_per_class <- input$students_per_class
    num_classes <- input$num_classes
    n_sims <- input$num_simulations

    # Function to run one simulation
    run_one_sim <- function() {
      # Calculate variance components based on ICC
      total_var <- 1
      between_var <- icc * total_var
      within_var <- (1 - icc) * total_var
      n_total <- num_classes * students_per_class
      cluster_ids <- rep(1:num_classes, each = students_per_class)
      cluster_effects <- rep(rnorm(num_classes, 0, sqrt(between_var)), each = students_per_class)

      # Assign treatment (balanced design)
      if (input$randomization == "b") {
        # Randomize within classes
        treatment <- rep(c(0, 1), each = n_total/2)
      } else {
        # Randomize between classes
        treatment <- sample(c(0, 1), size = n_total, replace = TRUE)
      }

      # Generate individual effects
      individual_effects <- rnorm(n_total, 0, sqrt(within_var))

      # Generate outcomes (null hypothesis is true - no treatment effect)
      outcome <- cluster_effects + individual_effects


      # Create dataset
      data <- data.frame(
        class_id = cluster_ids,
        treatment = treatment,
        outcome = outcome
      )

      # Run t-test (ignoring clustering)
      t_test_result <- t.test(outcome ~ treatment, data = data)
      t_test_p <- t_test_result$p.value

      # Run mixed model (accounting for clustering)
      mixed_model <- lmer(outcome ~ treatment + (1|class_id), data = data)
      mixed_p <- summary(mixed_model)$coefficients[2, "Pr(>|t|)"]

      # Return p-values
      return(c(t_test_p = t_test_p, mixed_p = mixed_p,
               ICC = psychometric::ICC1.lme(outcome, class_id, data),
               DEFT = sqrt(1 + (input$students_per_class -1) * psychometric::ICC1.lme(outcome, class_id, data))))
    }

    # Run simulations with progress indicator
    withProgress(message = 'Running simulations', value = 0, {
      p_values <- replicate(n_sims, {
        incProgress(1/n_sims)
        run_one_sim()
      })
    })

    # Convert results to data frame
    p_df <- data.frame(
      t_test_p = p_values["t_test_p", ],
      mixed_p = p_values["mixed_p", ]
    )

    # Calculate Type I error rates (alpha)
    t_test_type1 <- mean(p_df$t_test_p < 0.05)
    mixed_type1 <- mean(p_df$mixed_p < 0.05)

    # Return results
    list(
      p_values = p_df,
      t_test_type1 = t_test_type1,
      mixed_type1 = mixed_type1,
      inflation_factor = t_test_type1 / 0.05
    )
  })


  # Plot Type I error rates
  output$alpha_plot <- renderPlot({
    if(is.null(sim_results())) {
      return(NULL)
    }

    results <- sim_results()

    # Create data frame for plotting
    plot_data <- data.frame(
      method = c("Standard t-test", "Mixed model"),
      alpha = c(results$t_test_type1, results$mixed_type1)
    )

    ggplot(plot_data, aes(x = method, y = alpha, fill = method)) +
      geom_col() +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
      scale_fill_manual(values = c(
        "Standard t-test" = "#8cd000",
        "Mixed model" = "#267326"
      )) +
      labs(
        title = "Type I Error Rates",
        x = NULL,
        y = "Type I Error Rate"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Alegreya Sans"),
        plot.title = element_text(face = "bold", color = "#174717"),
        plot.subtitle = element_text(color = "#267326"),
        legend.position = "none"
      )
  })

  # Plot p-value distribution
  output$pvalue_dist <- renderPlot({
    if(is.null(sim_results())) {
      return(NULL)
    }

    results <- sim_results()

    # Prepare data for plotting
    p_values_long <- results$p_values %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "method",
        values_to = "p_value"
      ) %>%
      mutate(method = factor(method,
                             levels = c("t_test_p", "mixed_p"),
                             labels = c("Standard t-test", "Mixed model")))

    ggplot(p_values_long, aes(x = p_value, fill = method)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      geom_vline(xintercept = 0.05, linetype = "dashed", color = "#174717") +
      scale_fill_manual(values = c("Standard t-test" = "#8cd000", "Mixed model" = "#267326")) +
      labs(
        title = "Distribution of p-values",
        x = "p-value",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Alegreya Sans"),
        plot.title = element_text(face = "bold", color = "#174717"),
        plot.subtitle = element_text(color = "#267326"),
        legend.position = "bottom"
      )
  })
}


# Run the application
shinyApp(ui = ui, server = server)
