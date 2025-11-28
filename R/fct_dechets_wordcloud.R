#' ---
#' title : "RIPARU - dechet wordcloud function"
#' author : Aubin Woehrel
#' date : 2025-11-28
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - DECHETS WORDCLOUD FUNCTION
#'
#' Description :
#' Function for wordcloud representation
#'
#' =============================================================================

# Libraries ----
library("magrittr")

#' Main Function ----
dechets_wordcloud <- function(
  dechet_data = NULL, # data
  to_remove = NULL, # variables to remove from categorie_specifique (vector)
  offset = 10,
  graph_size = 0.5,
  background_color = "#e7e4e4",
  text_color = "random-dark",
  rotate_ratio = 1
) {
  # In case of lack of dataset entry
  if (is.null(dechet_data)) {
    stop("No data, please enter dataset with parameter dechet_data")
  }

  macro_wordcloud <- dechet_data %>%
    filter(!categorie_specifique %in% to_remove) %>%
    dplyr::mutate(total_transfo = sqrt(total_100m + offset))

  total_objets <- sum(macro_wordcloud$total_100m)
  total_objets_transfo <- sum(macro_wordcloud$total_transfo)

  macro_wordcloud %>%
    dplyr::mutate(
      freq = total_100m / total_objets,
      freq_transfo = total_transfo / total_objets_transfo
    ) %>%
    dplyr::select(print_name, freq_transfo) %>%
    wordcloud2::wordcloud2(
      data = .,
      size = graph_size,
      shuffle = TRUE,
      backgroundColor = background_color,
      color = text_color,
      rotateRatio = rotate_ratio
    )
}
