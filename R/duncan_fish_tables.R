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
    purrr::set_names(nm = c('Comparison', 'Z', 'p-value unadj.', paste0('p-value adj.(', b, ')'))) %>%
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
  tibble::as_tibble(t(tibble::as_tibble(unlist(x)))) %>%
    purrr::set_names(nm = names(x)) %>%
    dplyr::select(data.name, tidyselect::everything(), -method) %>%
    dplyr::mutate(p.value = as.numeric(p.value)) %>%
    dplyr::mutate(statistic = round(as.numeric(statistic), 1),
           p.value = round(p.value, 4),
           'data.name' = stringr::str_to_title(data.name)) %>%
    dplyr::mutate('data.name' = stringr::str_replace(data.name, 'By', "vs.")) %>%
    dplyr::rename(df = parameter, 'p-value' = p.value,
           Comparison = data.name, 'chi-squared' = statistic)
}
