#' pcas_variance_explained
#'@param id,input,output,session Internal parameters for {shiny}.
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


# min_vars_for_var_exp <-  !!sym(input$features_selected)
#
# shiny::validate(
#   shiny::need(!is.null(min_vars_for_var_exp), "Choose atleast one variable in the first tab for the chart to show up!")
# )
#
# sdev_id <- function(){
#   pca_scores() %>%
#     dplyr::select(-district) %>%
#     purrr::map_df(., purrr::possibly(var, NA_integer_)) %>%
#     tidyr::pivot_longer(everything(), names_to = "component", values_to = "value") %>%
#     # group_by(component) %>%
#     dplyr::mutate(
#       percent_var = value/sum(value),
#       cumsum = sum(percent_var))
# }
#
# sdev_id()  %>%
#   # mutate(component = forcats::fct_reorder(factor(component), percent_var)) %>%
#   ggplot2::ggplot(ggplot2::aes(component, percent_var))+
#   ggplot2::geom_col(alpha=0.5, fill = "seagreen", width = 0.4)+
#   # geom_point(aes(component,percent_var))+
#   ggplot2::labs(x="Principal Components",
#                 y="Variane Explained")+
#   ggplot2::scale_y_continuous(labels = scales::percent_format())
#
#
