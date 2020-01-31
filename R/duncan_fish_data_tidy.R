#' Tidy 2016 gillnet datasets to prep for join with trawl data
#'
#'Gillnet data loaded is from original excel workbook provided by FLNRO Large Lakes team (DuncanGillnettData_2018 ages final.xlsx).
#'The names of the columns have been repaired on import with .name_repair function from readxl \url{https://readxl.tidyverse.org/} package
#'The name of the sheet in the workbook is "Duncan 2016 GN "

#' @param df  List of dataframes each representing a sheet in DuncanGillnettData_2018 ages final.xlsx. This defaults to the gillnet_raw list object.
#' @param sheet_name String name of excel sheet in list loaded from original excel file. Defaults to "Duncan 2016 GN " which is the only sheet
#' this function will work on since the column names differ in the later years....
#' @importFrom magrittr "%>%"
#' @export duncan_gillnet_tidy_2016


duncan_gillnet_tidy_2016 <- function(df = gillnet_raw, sheet_name = "Duncan 2016 GN ") {
  df %>%
    purrr::pluck(sheet_name)%>%
    dplyr::rename(Date = SET.Date, Fork_Length = Fork.Length...mm., Weight = Weight..g., Age = Approved.Ages,
         Condition_Factor = CF, Gender = Sex, ID = Sample..No,
         Scale_num = Scale..Sample..No, Otolith_Sample_Num = Otolith..Sample..No,
         Otolith_Confidence = Otolith.Confidence) %>%
    dplyr::mutate(Year = as.factor(lubridate::year(Date)), scale_confidence = NA,
         file_source = "DuncanGillnettData_2018 ages final.xlsx") %>% ##grab just the year for plots
    dplyr::select(Date, Year, depth, Species, Gender, Maturity, Fork_Length, Weight,
         Condition_Factor, Age, ID, Scale_num, Otolith_Sample_Num, scale_confidence,
         Otolith_Confidence, Comment, file_source) %>%
    tidyr::drop_na(Date, Species) ##drop rows with no date and species
  }


#' Tidy 2017 gillnet dataset to prep for join with trawl data
#'
#'Gillnet data loaded is from original excel workbook provided by FLNRO Large Lakes team (DuncanGillnettData_2018 ages final.xlsx).
#'The names of the columns have been repaired on import with .name_repair function from \url{https://readxl.tidyverse.org/}  package
#'

#' @param df  List of dataframes each representing a sheet in DuncanGillnettData_2018 ages final.xlsx. This defaults to the gillnet_raw list object.
#' @param sheet_name String name of excel sheet in list loaded from original excel file. Defaults to "2017 GN".
#' Other option is "2018 GN"
#' @importFrom magrittr "%>%"
#' @export duncan_gillnet_tidy_2017


duncan_gillnet_tidy_2017<- function(df = gillnet_raw, sheet_name = "Duncan 2016 GN ") {
  df %>%
    purrr::pluck(sheet_name)%>%
    dplyr::rename(Date = SET.Date, Fork_Length = Fork.Length...mm., Weight = Weight..g., Age = Approved.Age,
           Condition_Factor = Condition..Factor, Gender = Sex,
           depth = Net.Depth, ID = Sample..No, Scale_num = Scale..Sample..No,
           Otolith_Sample_Num = Otolith..Sample..No) %>%
    dplyr::mutate(Year = as.factor(lubridate::year(Date)), Otolith_Confidence = NA, scale_confidence = NA,
           file_source = "DuncanGillnettData_2018 ages final.xlsx") %>% ##grab just the year for plots
    dplyr::select(Date, Year, depth, Species, Gender, Maturity, Fork_Length, Weight,
           Condition_Factor, Age, ID, Scale_num, Otolith_Sample_Num, scale_confidence,
           Otolith_Confidence, Comment, file_source)%>%
    tidyr::drop_na(Date, Species)
}

#' Tidy 2018 gillnet dataset to prep for join with trawl data
#'
#'Gillnet data loaded is from original excel workbook provided by FLNRO Large Lakes team (DuncanGillnettData_2018 ages final.xlsx).
#'The names of the columns have been repaired on import with .name_repair function from \url{https://readxl.tidyverse.org/}  package
#'

#' @param df  List of dataframes each representing a sheet in DuncanGillnettData_2018 ages final.xlsx. This defaults to the gillnet_raw list object.
#' @param sheet_name String name of excel sheet in list loaded from original excel file. Defaults to "2017 GN".
#' Other option is "2018 GN"
#' @importFrom magrittr "%>%"
#' @export duncan_gillnet_tidy_2018

#'
duncan_gillnet_tidy_2018<- function(df = gillnet_raw, sheet_name = "2018 GN") {
  df %>%
  purrr::pluck(sheet_name) %>%
    dplyr::rename(Date = SET.Date, Fork_Length = Fork.Length...mm., Weight = Weight..g., Age = Approved.Ages,
         Condition_Factor = Condition..Factor, Gender = Sex, ID = Sample..No,
         Scale_num = Scale..Sample..No, Otolith_Sample_Num = Otolith..Sample..No,
         scale_confidence = SCALE.Confidence) %>%
    dplyr::mutate(Year = as.factor(lubridate::year(Date)), Otolith_Confidence = NA,
         file_source = "DuncanGillnettData_2018 ages final.xlsx") %>% ##grab just the year for plots
    dplyr::select(Date, Year, depth, Species, Gender, Maturity, Fork_Length, Weight,
         Condition_Factor, Age, ID, Scale_num, Otolith_Sample_Num, scale_confidence,
         Otolith_Confidence, Comment, file_source)%>%
    tidyr::drop_na(Date, Species)
}

#' Replace the column names of a dataframe by concatenating top two rows of each columns in an up-down direction.
#'
#' Used on the biomass data tidying "Duncan report_multi basin biomass.xlsx" file
#' but should work on any dataframe where the top two stacked rows contain details for a column
#' as can be common in excel style data presentation.

#' @param df  Dataframe object with details of the columns in the top two stacked rows as can be common in
#' excel style data presentation.  Note that the top two rows of the dataframe are removed after the column names are set.

#' @importFrom magrittr "%>%"
#' @export duncan_top2rows_to_colname
#'
duncan_top2rows_to_colname <- function(df){
  a <- df %>%
    dplyr::slice(1:2) %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(new_id = paste(V1, V2)) %>%
    dplyr::mutate(new_id = stringr::str_replace_all(new_id, 'NA ', "")) %>%
    dplyr::mutate(new_id = stringr::str_replace_all(new_id, ' NA', "")) %>%
    t() %>%
    tibble::as_tibble() %>%
    purrr::set_names(., nm = unlist(slice(.,3))) %>%
    slice(-1:-3)
  df <- purrr::set_names(df, nm = unlist(colnames(a))) %>%
    slice(-1:-2)
}
