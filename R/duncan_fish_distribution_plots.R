#' Make plots to explore distribution of fish data
#'
#'This function requires you to clean the input dataframe (df) to not include nas so
#'don't forget to filter them out!
#'
#' @importFrom magrittr "%>%"
#' @param df  Dataframe cleaned up with nas removed from variable of interest
#' @param species Character value indicating fish species of interest. Defaults to KO
#' @param year Numeric value of year of the samples. Defaults to 2018
#' @param age Numeric value of age of the fish of interest
#' @param variable Character value indicating which column you wish to look at.  Defaults to weight
#' @param ... Not used
#' @param file_source Name of source file where data came from. This is detailed in our database under the file_source column Defaults to the gillnet data only.
#' @param plot Type of plot to use. Fefaults to ggdensity. Other options include ggqqplot and others from the ggpubr package

#' @export dunc_fish_distribution_plot



dunc_fish_distribution_plot <- function(df = filter(fish_all, !is.na(Weight)),
                                species = "KO",
                                year = 2018,
                                age = 2,
                                variable = "Weight",
                                ...,
                                file_source = "DuncanGillnettData_2018 ages final.xlsx",
                                plot = ggpubr::ggdensity)
{
  df %>%
    dplyr::filter((Year == year) &
             Species == species &
             file_source == file_source &
             Age == age) %>%
    dplyr::select(variable) %>%
    unlist() %>%
    plot(title = paste0("GN KO Age ", age, " - ", year,
                             " Density Plot for ", variable, " N = ",
                             nrow(filter(df, Species == species, Year == year,
                                         Age == age, file_source == file_source))),
              xlab = paste(species, variable))

}




