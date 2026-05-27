# Deservingness conjoint with surveydown

This is the repository for the conjoint experiment designed for the survey "Inequality and Deservingness", in the framework of the project "Market justice and deservingness of social welfare - [JUSMER](https://jusmer.com/)". The survey is implemented using the R package [surveydown](https://surveydown.org/), which allows for creating complex surveys with custom HTML, CSS, and JavaScript.

## Basics

- the survey was built using the `conjoint_tables` template provided by surveydown, which is designed for conjoint experiments with a tabular format
- to run the survey locally, use `shiny::runApp()`
- to deploy the survey:
  - create an account on [shinyapps.io](https://www.shinyapps.io/)
  - install the `rsconnect` package
  - use `rsconnect::deployApp(appName = "my_survey") to deploy the survey to shinyapps.io
-  

Refer to the [Start with a template](https://surveydown.org/docs/getting-started#start-with-a-template) section for more details.
