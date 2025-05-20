#' @title Create Drop Down Filter Render Function
#' @description Function that returns a `reactable` filter input
#' render function that provide a drop down functionality.
#' @param reactable_element_id Text input indicating the
#' element ID of the `reactable`
#' @param style Text input indicating the javascript style parameters
#' of the filter input drop down box.
#' Default: 'width: 100\%; height: 28px;'
#' @return A render function for a `reactable` column indicating that
#' it needs a drop down filter input box.
#' @examples
#' create_dropdown_filter_render_function(
#'   reactable_element_id = "drop-down-filter-table")
#' @rdname create_dropdown_filter_render_function
#' @export
create_dropdown_filter_render_function <- function(
    reactable_element_id,
    style = "width: 100%; height: 100%;") {

  # https://glin.quarto.pub/observable-reactable/#using-reactable-to-filter-observable-charts
  # https://glin.github.io/reactable/articles/custom-filtering.html
  # Creates a data list column filter for a table with the given ID

  # Select input filter with an "All" default option
  function(values, name) {


    htmltools::tags$select(
        # Set to undefined to clear the filter
        onchange = sprintf("
          const value = event.target.value
          Reactable.setFilter('%s', '%s', value === '__ALL__' ? undefined : value)",
          reactable_element_id, name),
        # "All" has a special value to clear the filter, and is the default option
        htmltools::tags$option(value = "__ALL__", "All"),
        lapply(intersect(levels(values),unique(values)), htmltools::tags$option),
        "aria-label" = sprintf("Filter %s", name),
        style = style
      )
  }
}

#' @title Reactable With Download CSV Button
#' @description A function that converts a simple
#' tibble or dataframe into a reactable with a
#' download csv button
#' @param input_data Input simple dataframe or tibble
#' with no nested data
#' @param download_file_name Name of the
#' csv file when download csv button is clicked
#' Default: NULL
#' @return A interactive table of `input_data`
#' with a download csv button.
#' medical_data <- tibble::tribble(
#'   ~unique_id, ~medications, ~medication_fixed,
#'   "1", "Medication 1", "Fixed Medication 1",
#'   "2", "Medication 2", "Fixed Medication 2"
#' )
#'
#' medical_table <- medical_data |>
#'   reactable_with_download_csv_button(
#'     factor_columns = c("unique_id", "medications",
#'                        "medication_fixed")
#'     download_file_name = "download"
#'   )
#'
#' @rdname reactable_with_download_csv_button
#' @export
reactable_with_download_csv_button <- function(
    input_data,
    download_file_name = "download",
    ...) {

  input_data <- input_data |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.factor), ~ forcats::fct_na_value_to_level(.x, ""))
    )

  # Ensure file name is valid
  download_file_name <- download_file_name |>
    fs::path_sanitize()

  # Ensure that the element id is unique
  # Need some testing if it works
  reactable_element_id <- "reactable" |>
    paste(fs::path_file(fs::file_temp(pattern = "")),
          sep = "-")

  csv_download_file_name <- download_file_name |>
    paste0(".csv")
  reactable_api_command <- glue::glue(
    "Reactable.downloadDataCSV('{reactable_element_id}', \\
    '{csv_download_file_name}')"
  )

  filter_list <- tibble::lst()

  factor_columns <- input_data |>
    dplyr::select_if(is.factor) |>
    names()

  if (length(factor_columns) != 0) {
    for (factor_column in factor_columns) {
      new_list = tibble::lst(
        !!factor_column := reactable::colDef(
          filterInput = create_dropdown_filter_render_function(reactable_element_id),
          # Exact match filter method
          filterMethod = reactable::JS("(rows, columnId, filterValue) => {
        return rows.filter(row => row.values[columnId] === filterValue)
      }")
        )
      )
      filter_list <- filter_list |>
        append(new_list)
    }
  } else {
    filter_list <- NULL
  }

  data_table <- input_data |>
    reactable::reactable(
      columns = filter_list,
      filterable = TRUE,
      elementId = reactable_element_id,
      ...)

  results_table <- htmltools::browsable(
    htmltools::tagList(
      data_table,
      htmltools::tags$button(
        htmltools::tagList(fontawesome::fa("download"), "Download as CSV"),
        onclick = reactable_api_command
      )
    )
  )

  return(results_table)

}
