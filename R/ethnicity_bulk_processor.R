#' Run ethnicity graphs and save results
#' @export
#'
#' @examples
#' cartesian_ethnicity_groups_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019)
ethnicity_bulk_processor <- function(year,pollutant){


# prawn_path <-"NOx_emissions_in_2019_v0.13.3/PRAWN.csv"
# pollutant <- "NOx"

prawn_path <-paste0(year,"_",pollutant,".csv")

graph_placeholder <- cartesian_deprivation_emissions_ethnicity(prawn_path = prawn_path,
                                                   pollutant= pollutant,
                                                   year=year)

process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant," emissions in ",
                                      year,
                                      "weighted by ethnicity.png"),
                                      file_format = "agg_png",
                                      type = 2,
                                      scaling = 0.5
                    )
##This function doesn't have a single output and is misleadingly named
graph_placeholder <- facet_sources_deprivation_emissions_ethnicity(prawn_path = prawn_path,
                                                                        pollutant= pollutant,
                                                                        year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by source, coloured by ethnicity, x deprivation.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)

graph_placeholder <- facet_sources_decile_emissions_ethnicity(prawn_path = prawn_path,
                                                         pollutant= pollutant,
                                                         year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by source, coloured by ethnicity, x decile.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)


graph_placeholder <- facet_ethnicity_bar_deprivation(prawn_path = prawn_path,
                                                    pollutant= pollutant,
                                                    year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/deprivation distribution for ethnicities.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)

graph_placeholder <- facet_RUC_decile_emissions_ethnicity(prawn_path = prawn_path,
                                                     pollutant= pollutant,
                                                     year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by RUC, coloured by ethnicity, x decile.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)

graph_placeholder <- facet_RUC_deprivation_emissions_ethnicity(prawn_path = prawn_path,
                                                          pollutant= pollutant,
                                                          year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by RUC, coloured by ethnicity, x deprivation.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)
}
