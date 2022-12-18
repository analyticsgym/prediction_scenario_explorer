### Future improvement areas
# TODO: reduce code pattern duplication with functions
# TODO: further tuning and feature engineering for saved xgb model
# TODO: functionality to randomly generate scenario inputs

library(shiny)
library(tidyverse)
library(tidymodels)
library(mlbench)
library(xgboost)
library(bundle)

data(PimaIndiansDiabetes2)

pct_format_1 <- scales::label_percent(accuracy = 1)

diabetes_df <- PimaIndiansDiabetes2 %>%
  ### for sake of example drop features that have high density of NAs
  dplyr::select(-triceps, -insulin) %>%
  ### drop remaining observations that have NAs for example
  ### imputation methods could be applied in practice to fill in NAs
  drop_na() %>%
  dplyr::mutate(diabetes = factor(ifelse(diabetes=="pos", 1, 0))) %>%
  as_tibble()

##############################################################################
### Build model and save for reuse in app
##############################################################################

# set.seed(321)
# diabetes_split <- initial_split(diabetes_df, strata = diabetes)
# diabetes_train <- training(diabetes_split)
# diabetes_test <- testing(diabetes_split)
# 
# set.seed(123)
# diabetes_folds <- vfold_cv(diabetes_train, v=5)
# 
# xgb_recipe <-
#   recipe(formula = diabetes ~ ., data = diabetes_df) %>%
#   step_zv(all_predictors())
# 
# xgb_spec <-
#   boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
#              loss_reduction = tune(), sample_size = tune()) %>%
#   set_mode("classification") %>%
#   set_engine("xgboost")
# 
# xgb_workflow <-
#   workflow() %>%
#   add_recipe(xgb_recipe) %>%
#   add_model(xgb_spec)
# 
# set.seed(456)
# xgb_tune <- xgb_workflow %>%
#   tune_grid(resamples = diabetes_folds,
#             grid = 3)
# 
# # xgb_tune %>% 
# #   autoplot(metric = "accuracy")
# # 
# # xgb_tune %>% 
# #   select_best("roc_auc")
# 
# best_params <- xgb_tune %>% select_best("roc_auc")
# 
# xgb_final_params <- 
#   tibble(trees = best_params$trees,
#          min_n = best_params$min_n,
#          tree_depth = best_params$tree_depth,
#          learn_rate = best_params$learn_rate,
#          loss_reduction = best_params$loss_reduction,
#          sample_size = best_params$sample_size)
# 
# final_xgb_workflow <- 
#   xgb_workflow %>%
#   finalize_workflow(parameters = xgb_final_params)
# 
# final_xgb <- 
#   final_xgb_workflow %>%
#   fit(diabetes_train)
# 
# # final_xgb %>%
# #   fit(data = diabetes_train) %>%
# #   extract_fit_parsnip() %>%
# #   vip(geom = "point")
# 
# # ~79% accuracy on test data
# predict(final_xgb, diabetes_test) %>%
#   mutate(actual = diabetes_test$diabetes) %>%
#   accuracy(truth=actual, estimate=.pred_class)
# 
# xgb_all_data <- final_xgb
#   final_xgb_workflow %>%
#   fit(diabetes_df)
# 
# mod_bundle <- bundle(xgb_all_data)
# saveRDS(mod_bundle, file = "xgb_mod_bundle.rds")

##############################################################################
### Power app with saved model
##############################################################################

saved_mod_bundle <- readRDS(file = "xgb_mod_bundle.rds")

xgb_mod_unbundled <- unbundle(saved_mod_bundle)

ui <- fluidPage(
    titlePanel("Prediction Scenario Explorer"),
    p("Author: Brian Moore (@analyticsanalyst)"),
    HTML("<p><a href='https://github.com/analyticsanalyst/prediction_scenario_explorer'>Code on Github</a></p>"),
    p("Example app comparing predicted probability of testing
       positive for diabetes based on scenario predictor inputs."),
    tags$div(
      tags$h5("Predictor variable descriptions:")
    ),
    tags$ul(
      tags$li("pregnant: number of times pregnant"), 
      tags$li("glucose: plasma glucose concentration (glucose tolerance test)"), 
      tags$li("pressure: diastolic blood pressure (mm Hg)"),
      tags$li("triceps: triceps skin fold thickness"), 
      tags$li("insulin: 2-Hour serum insulin (mu U/ml)"), 
      tags$li("pedigree: diabetes pedigree function"),
      tags$li("age: age (years)")
    ),
    HTML("<p><a href='https://cran.r-project.org/web/packages/mlbench/mlbench.pdf'>
         Data source: PimaIndiansDiabetes2 dataset from mlbench package</a></p>"),
    fluidRow(
      column(width = 4, h1("Scenario 1")),  
      column(width = 4, h1("Scenario 2")),
      column(width = 4, h1("Scenario 3"))  
    ),
    fluidRow(
      column(width = 4,
             sliderInput("pregnant_s1", "Pregnant",
                          value = round(mean(diabetes_df$pregnant)),
                          min = min(diabetes_df$pregnant),
                          max = max(diabetes_df$pregnant))),
      column(width = 4,
             sliderInput("pregnant_s2", "Pregnant",
                          value = round(mean(diabetes_df$pregnant)),
                          min = min(diabetes_df$pregnant),
                          max = max(diabetes_df$pregnant))),
      column(width = 4,
             sliderInput("pregnant_s3", "Pregnant",
                          value = round(mean(diabetes_df$pregnant)),
                          min = min(diabetes_df$pregnant),
                          max = max(diabetes_df$pregnant)))
    ),
    fluidRow(
      column(width = 4,
             sliderInput("glucose_s1", "Glucose",
                         value = round(mean(diabetes_df$glucose)),
                         min = min(diabetes_df$glucose),
                         max = max(diabetes_df$glucose))),
      column(width = 4,
             sliderInput("glucose_s2", "Glucose",
                         value = round(mean(diabetes_df$glucose)),
                         min = min(diabetes_df$glucose),
                         max = max(diabetes_df$glucose))),
      column(width = 4,
             sliderInput("glucose_s3", "Glucose",
                         value = round(mean(diabetes_df$glucose)),
                         min = min(diabetes_df$glucose),
                         max = max(diabetes_df$glucose)))
    ),
    fluidRow(
      column(width = 4,
             sliderInput("pressure_s1", "Pressure",
                         value = round(mean(diabetes_df$pressure)),
                         min = min(diabetes_df$pressure),
                         max = max(diabetes_df$pressure))),
      column(width = 4,
             sliderInput("pressure_s2", "Pressure",
                         value = round(mean(diabetes_df$pressure)),
                         min = min(diabetes_df$pressure),
                         max = max(diabetes_df$pressure))),
      column(width = 4,
             sliderInput("pressure_s3", "Pressure",
                         value = round(mean(diabetes_df$pressure)),
                         min = min(diabetes_df$pressure),
                         max = max(diabetes_df$pressure)))
    ),
    fluidRow(
      column(width = 4,
             sliderInput("mass_s1", "Mass",
                         value = round(mean(diabetes_df$mass)),
                         min = min(diabetes_df$mass),
                         max = max(diabetes_df$mass))),
      column(width = 4,
             sliderInput("mass_s2", "Mass",
                         value = round(mean(diabetes_df$mass)),
                         min = min(diabetes_df$mass),
                         max = max(diabetes_df$mass))),
      column(width = 4,
             sliderInput("mass_s3", "Mass",
                         value = round(mean(diabetes_df$mass)),
                         min = min(diabetes_df$mass),
                         max = max(diabetes_df$mass)))
    ),
    fluidRow(
      column(width = 4,
             sliderInput("pedigree_s1", "Pedigree",
                         value = mean(diabetes_df$pedigree),
                         min = min(diabetes_df$pedigree),
                         max = max(diabetes_df$pedigree))),
      column(width = 4,
             sliderInput("pedigree_s2", "Pedigree",
                         value = mean(diabetes_df$pedigree),
                         min = min(diabetes_df$pedigree),
                         max = max(diabetes_df$pedigree))),
      column(width = 4,
             sliderInput("pedigree_s3", "Pedigree",
                         value = mean(diabetes_df$pedigree),
                         min = min(diabetes_df$pedigree),
                         max = max(diabetes_df$pedigree)))
    ),
    fluidRow(
      column(width = 4,
             sliderInput("age_s1", "Age",
                         value = round(mean(diabetes_df$age)),
                         min = min(diabetes_df$age),
                         max = max(diabetes_df$age))),
      column(width = 4,
             sliderInput("age_s2", "Age",
                         value = round(mean(diabetes_df$age)),
                         min = min(diabetes_df$age),
                         max = max(diabetes_df$age))),
      column(width = 4,
             sliderInput("age_s3", "Age",
                         value = round(mean(diabetes_df$age)),
                         min = min(diabetes_df$age),
                         max = max(diabetes_df$age))),
    ),
    fluidRow(
      column(width = 4,
             h4("Scenario 1 Predicted Diabetes Probability"),
             textOutput("res1")
      ),
      column(width = 4,
             h4("Scenario 2 Predicted Diabetes Probability"),
             textOutput("res2")
      ),
      column(width = 4,
             h4("Scenario 2 Predicted Diabetes Probability"),
             textOutput("res3")
      )
    ),
    br(),
    actionButton("reset", "Reset inputs"),
    br(),
    br()
)
server <- function(input, output) {

    output$res1 <- renderText({
        pred_df <- tibble(
          pregnant = input$pregnant_s1,
          glucose = input$glucose_s1,
          pressure = input$pressure_s1,
          mass = input$mass_s1,
          pedigree = input$pedigree_s1,
          age = input$age_s1
        )
        predict(xgb_mod_unbundled, pred_df, type = "prob") %>%
          gather() %>%
          arrange(desc(key)) %>%
          head(1) %>%
          mutate(out_txt = ifelse(key==".pred_1",
                                  paste0("Predicted Probability Diabetes: ", pct_format_1(value)),
                                  "Error")) %>%
          pull(out_txt)
      })
    
    output$res2 <- renderText({
      pred_df <- tibble(
        pregnant = input$pregnant_s2,
        glucose = input$glucose_s2,
        pressure = input$pressure_s2,
        mass = input$mass_s2,
        pedigree = input$pedigree_s2,
        age = input$age_s2
      )
      predict(xgb_mod_unbundled, pred_df, type = "prob") %>%
        gather() %>%
        arrange(desc(key)) %>%
        head(1) %>%
        mutate(out_txt = ifelse(key==".pred_1",
                                paste0("Predicted Probability Diabetes: ", pct_format_1(value)),
                                "Error")) %>%
        pull(out_txt)
    })
    
    output$res3 <- renderText({
      pred_df <- tibble(
        pregnant = input$pregnant_s3,
        glucose = input$glucose_s3,
        pressure = input$pressure_s3,
        mass = input$mass_s3,
        pedigree = input$pedigree_s3,
        age = input$age_s3
      )
      predict(xgb_mod_unbundled, pred_df, type = "prob") %>%
        gather() %>%
        arrange(desc(key)) %>%
        head(1) %>%
        mutate(out_txt = ifelse(key==".pred_1",
                                paste0("Predicted Probability Diabetes: ", pct_format_1(value)),
                                "Error")) %>%
        pull(out_txt)
    })
    
    # when reset action button click update inputs back to default
    observeEvent(input$reset, {
      # TODO: add function here
      map(c("pregnant_s1", "pregnant_s2", "pregnant_s3"),
          ~updateSliderInput(inputId = ., 
                            value = round(mean(diabetes_df$pregnant))))
      map(c("glucose_s1", "glucose_s2", "glucose_s3"),
          ~updateSliderInput(inputId = ., 
                             value = round(mean(diabetes_df$glucose))))
      map(c("pressure_s1", "pressure_s2", "pressure_s3"),
          ~updateSliderInput(inputId = ., 
                             value = round(mean(diabetes_df$pressure))))
      map(c("mass_s1", "mass_s2", "mass_s3"),
          ~updateSliderInput(inputId = ., 
                             value = round(mean(diabetes_df$mass))))
      map(c("pedigree_s1", "pedigree_s2", "pedigree_s3"),
          ~updateSliderInput(inputId = ., 
                             value = mean(diabetes_df$pedigree)))
      map(c("age_s1", "age_s2", "age_s3"),
          ~updateSliderInput(inputId = ., 
                             value = round(mean(diabetes_df$age))))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

