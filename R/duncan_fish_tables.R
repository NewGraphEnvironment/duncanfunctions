#' Convert output of FSA::dunnTest to table for reporting.
#'
#' Convert output of FSA::dunnTest to table for reporting and include unadjusted and adjusted p-values
#' as well as the method ued for adjustment.

#' @param x Captured output of FSA::dunnTest function

#' @importFrom magrittr "%>%"
#' @export duncan_table_dunnTest


duncan_table_dunnTest <- function(x){
  a <- x$res
  b <- x$method
  c <- a %>%
    mutate_if(is.numeric, round,3) %>%
    purrr::set_names(nm = c('Comparison', 'Z', 'p-value unadj.', 'p-value adj.')) %>%
    dplyr::mutate(Comparison = stringr::str_replace(Comparison, ' - ', " vs."))

}


#' Convert output of stats::kruskal.test to table for reporting.
#'
#' Convert output of stats::kruskal.test to table for reporting.

#' @param x Captured output of stats::kruskal.test function

#' @importFrom magrittr "%>%"
#' @export duncan_table_kruskal
#' @examples duncan_table_kruskal(kruskal.test(Ozone ~ Month, data = airquality))

duncan_table_kruskal <- function(x){
  tibble::as_tibble(t(tibble::enframe(unlist(x), name = NULL))) %>%
    purrr::set_names(nm = names(x)) %>%
    dplyr::select(data.name, tidyselect::everything(), -method) %>%
    dplyr::mutate(p.value = as.numeric(p.value)) %>%
    dplyr::mutate(stat_below3signif =
                    dplyr::case_when(p.value < 0.001 ~ 0.001,
                                     TRUE ~ NA_real_)) %>%
    dplyr::mutate(statistic = round(as.numeric(statistic), 1),
                  p.value = as.character(round(p.value, 3)),
                  'data.name' = stringr::str_to_title(data.name)) %>%
    dplyr::mutate(p.value = case_when(stat_below3signif == 0.001 ~ "<0.001",
                                      TRUE ~ p.value)) %>%
    dplyr::mutate('data.name' = stringr::str_replace(data.name, 'By', "vs.")) %>%
    dplyr::rename(df = parameter, 'p-value' = p.value,
                  Comparison = data.name, 'chi-squared' = statistic) %>%
    dplyr::select(-stat_below3signif)
}
