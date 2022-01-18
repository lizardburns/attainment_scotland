#' Convert subject names to lower case accounting for proper nouns
#'
#' Uses {stringr} functions str_to_lower() or str_to_sentence() according to
#' whether subject name is among defined list of subjects whose names start with
#' proper nouns.
#'
#' @param string A string or character vector
#'
#' @export
#'
#' @examples
#' c("X", "x", "English", "English Literature", "Geography") %>%
#'   subject_str_to_lower()
subject_str_to_lower <- function(string) {
  proper_subjects <- c("English language",
                       "English literature",
                       "English language & literature",
                       "English Language",
                       "English Literature",
                       "English Language & Literature",
                       "English",
                       "German",
                       "French",
                       "Spanish",
                       "Arabic",
                       "Bengali",
                       "Chinese",
                       "Dutch",
                       "Greek",
                       "Italian",
                       "Japanese",
                       "Panjabi",
                       "Persian",
                       "Polish",
                       "Portuguese",
                       "Russian",
                       "Turkish",
                       "Urdu",
                       "Latin")
  string %>% purrr::map_chr(~{
    if(.x %in% proper_subjects) {
      stringr::str_to_sentence(string = .x)
    } else {
      stringr::str_to_lower(string = .x)
    }
  })
}

filter_summary <- function(df, qlevel, subj, threshold) {
  thresh <- switch (threshold,
                    "All" = c("A", "B & above", "C & above", "D & above", "U"),
                    "A" = "A",
                    "B & above" = "B & above",
                    "C & above" = "C & above",
                    "D & above" = "D & above",
                    "U" = "U",
  )
  
  df %>% 
    dplyr::filter(Qualification == qlevel, 
                  Subject == subj, 
                  `Grade threshold` %in% thresh)
}

# dat %>% 
#   filter_summary(qlevel = "scqf7", subj = "Accounting", threshold = "U")
# dat %>% 
#   filter_summary(qlevel = "scqf7", subj = "Accounting", threshold = "All")
# dat %>% 
#   filter_summary(qlevel = "scqf7", subj = "Accounting", threshold = "C")
cols_sqa <- c("#003466", "#cb94cb", "#f27d13", "#7793bc", "#6c848c")

ts_plot <- function(data, threshold) {
  
  p <- data %>% 
    dplyr::mutate(
      `Number of entries` = prettyNum(`Number of entries`, big.mark = ",")
      ) %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = Year, 
        y = pc,
        text = paste(
          "Pecent of candidates:", 
          scales::percent(pc, accuracy = .1)
        ),
        label = `Number of entries`
      )
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = `Grade threshold`)) +
    ggplot2::geom_line(
      ggplot2::aes(colour = `Grade threshold`, 
                   group = `Grade threshold`)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_color_manual(values = cols_sqa) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = paste(
        "Attainment for", 
        subject_str_to_lower(
          data %>% dplyr::distinct(Subject) %>% dplyr::pull(Subject)
          ), 
        "at", 
        ifelse(threshold == "All", 
               paste(stringr::str_to_lower(threshold), "grade thresholds"),
               paste("the", threshold, "grade threshold")),
        "from", 
        min(data$Year), 
        "to", 
        max(data$Year)
      ),
      y = "Percent of candidates"
    ) +
    ggplot2::theme(plot.title.position = "plot")
  
  plotly::ggplotly(
    p,
    tooltip = c("colour", "x", "text", "label")
  ) %>%
    plotly::config(displaylogo = FALSE) %>% 
    plotly::config(modeBarButtonsToRemove = c("lasso2d", 
                                              "select2d",
                                              "toggleSpikelines"))
  
}

ts_table <- function(data, threshold) {
  export_name <- paste0(
    "ts_", 
    data %>% dplyr::distinct(Qualification) %>% dplyr::pull(Qualification), 
    "_outcomes_", 
    data %>% dplyr::distinct(Subject) %>% dplyr::pull(Subject), 
    "_", 
    threshold
    )
  
  DT::datatable(
    data %>% 
      dplyr::mutate(
        pc = scales::percent(pc, accuracy = .1), 
        Year = as.integer(Year)
      ) %>% 
      dplyr::arrange(Year) %>% 
      dplyr::select(Qualification, Subject, Year, `Grade threshold`, pc) %>% 
      tidyr::pivot_wider(names_from = Year, values_from = pc),
    rownames = FALSE,
    extensions = 'Buttons',
    # container = sketch,
    options = list(
      dom = 'Bt',
      pageLength = 10,
      # columnDefs = list(list(className = 'dt-right', targets = 1:8)),
      buttons = list(
        list(extend = 'csv', filename = export_name, text = 'Download CSV'),
        list(extend = 'excel', title = NULL, filename = export_name, text = 'Download XLSX')
      ) # end of button list
    ) # end of options
  )
}

calculate_grade_dist <- function(data, subject) {
  df <- data %>% dplyr::filter(Year >= 2019)
  
  if(!missing(subject)) {
    df <- df %>% dplyr::filter(Subject == subject)
  }
  
  df %>% 
    dplyr::mutate(grade = stringr::str_remove(`Grade threshold`, " & above")) %>% 
    dplyr::select(-pc, -`Grade threshold`) %>% 
    tidyr::pivot_wider(names_from = grade, values_from = n) %>% 
    dplyr::mutate(
      Year = factor(Year, levels = c("2019", "2020", "2021")),
      B = B - A,
      C = C - B - A,
      D = D - C - B - A
    ) %>% 
    tidyr::pivot_longer(
      names_to = "Grade", 
      values_to = "n", 
      cols = A:U
    ) %>% 
    dplyr::mutate(
      # total = readr::parse_number(`Number of entries`),
      total = `Number of entries`,
      pc = n / total
    )
}

dist_plot <- function(data) {
  data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = factor(Grade, 
                   levels = c("U", "D", "C", "B", "A")), 
        y = pc, 
        fill = Year
      )
    ) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::scale_fill_manual(values = cols_sqa) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title.position = "plot") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0,.05)),
      labels = ~scales::percent(., accuracy = 1)
    ) +
    ggplot2::labs(
      title = paste(
        "Distribution of attainment for", 
        subject_str_to_lower(unique(data$Subject)), 
        "from", 
        levels(data$Year) %>% head(1), 
        "to", 
        levels(data$Year) %>% tail(1)
      ),
      x = "Grade",
      y = "Percent of candidates"
    )
}

dist_table <- function(data) {
  export_name <- paste0(
    "dist_", 
    data %>% dplyr::distinct(Qualification) %>% dplyr::pull(Qualification), 
    "_outcomes_", 
    data %>% dplyr::distinct(Subject) %>% dplyr::pull(Subject)
  )
  
  DT::datatable(
    data %>% 
      dplyr::mutate(
        pc = scales::percent(pc, accuracy = .1), 
        Year = as.integer(as.character(Year))
      ) %>% 
      dplyr::select(Qualification, Subject, Year, Grade, pc) %>% 
      tidyr::pivot_wider(names_from = Year, values_from = pc, names_sort = TRUE),
    rownames = FALSE,
    extensions = 'Buttons',
    # container = sketch,
    options = list(
      dom = 'Bt',
      pageLength = 10,
      # columnDefs = list(list(className = 'dt-right', targets = 1:8)),
      buttons = list(
        list(extend = 'csv', filename = export_name, text = 'Download CSV'),
        list(extend = 'excel', title = NULL, filename = export_name, text = 'Download XLSX')
      ) # end of button list
    ) # end of options
  )
}
