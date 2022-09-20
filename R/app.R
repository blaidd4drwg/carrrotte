#' shinycorescan
#'
#' Function that starts the interactive shinycorescan web app.
#'
#' @return Nothing
#' @export
#'
#' @import shiny
shinycorescan <- function() {
  # UI ####
  ui <- navbarPage(
    "shinyCoreScan",
    ## Import page ####
    tabPanel(
      "Data import",
      shinyjs::useShinyjs(),
      sidebarLayout(
        sidebarPanel(
          h4("Data import"),
          radioButtons("import_choosesource",
            "Choose data source",
            choices = list(
              "Sample Data" = "sampledata",
              "Longcore sample data" = "longcore_sampledata",
              "File Upload" = "fileupload"
            ),
            selected = "sampledata"
          ),
          fileInput(
            "import_xrffiles",
            "Upload bAXIL batch files (*.csv)",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            ),
            multiple = TRUE
          ),
          actionButton("import_loaddata", "Load & Parse XRF data"),
          hr(),
          checkboxInput("import_catmode", "Concatenate core sections"),
          checkboxInput("import_descorder", "Sort sections in descending order", value = TRUE),
          selectizeInput("import_catpreview_element",
            "Element for long core preview",
            choices = NULL
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("catplot"),
          DT::DTOutput("sectiontable")
        )
      )
    ),

    ## Diagnostics page ####
    tabPanel(
      "Diagnostics",
      selectizeInput("diagnostics_core_id", "Choose Core ID", NULL),
      shinycssloaders::withSpinner(
        plotOutput(
          "diagnostics_diagnosticsplot",
          height = 600,
          click = "diagnostics_diagnosticsplot_click",
          brush = brushOpts(id = "diagnostics_diagnosticsplot_brush")
        )
      ),
      hr(),
      fluidRow(
        column(
          3,
          h4("Plot options"),
          selectizeInput(
            "diagnostics_diagmode",
            "Diagnostic mode",
            choices = list(
              "Element spectra (cps)" = "cpsdiag",
              "Relative standard deviation" = "relstdevdiag",
              "Goodness of fit" = "chisqdiag"
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            numericInput(
              inputId = "diagnostics_ymin",
              label = "Y min",
              value = NA
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            numericInput(
              inputId = "diagnostics_ymax",
              label = "Y max",
              value = NA
            )
          ),
          sliderInput(
            "diagnostics_xlims",
            label = "X limits (Core length)",
            min = 0,
            max = 100,
            value = c(40, 60)
          ),
          numericInput(
            "diagnostics_medrelstdev",
            HTML("Mark |&sigma;<sub>rel</sub>| median spectrum threshold"),
            value = 99
          ),
          numericInput(
            "diagnostics_maxrelstdev",
            HTML("&sigma;<sub>rel</sub>: Draw horizontal line"),
            value = 20
          ),
          numericInput(
            "diagnostics_chi2cutoff",
            HTML("&chi;<sup>2</sup>: Draw horizontal line"),
            value = 5
          )
        ),
        column(
          3,
          h4(HTML(paste(
            "Exclude data for", textOutput("diagnostics_coreid_text", inline = TRUE)
          ))),
          shinyWidgets::pickerInput(
            inputId = "diagnostics_elements_excl",
            label = "Include/Exclude Elements",
            choices = NULL,
            options = list(`actions-box` = TRUE),
            multiple = TRUE
          ),
          hr(),
          p(strong("Exclude depths")),
          HTML(
            paste0(
              "To exclude measurements, select/brush them on the plot and choose 'Remove points'. ",
              span(
                style = "color:red",
                "Brushed measurements are removed for every element of a core! NB: Measurements with negative cps values are always automatically excluded."
              )
            )
          ),
          p(),
          actionButton("diagnostics_exclude_rmpoints", "Remove points", class = "btn-warning"),
          p(),
          actionButton("diagnostics_exclude_reset", "Reset current core", class = "btn-danger"),
          hr(),
          downloadButton("diagnostics_saveplot", "Download diagnostics plot"),
          p()
        ),
        column(
          6,
          h4(HTML(
            "Overview of changes"
          )),
          DT::DTOutput("diagnostics_excltable")
        )
      )
    ),
    ## Plotting page ####
    tabPanel(
      "Plotting",
      selectizeInput(
        inputId = "plotting_mode",
        label = "Select plot mode",
        options = list(placeholder = "Upload data first"),
        multiple = FALSE,
        choices = character(0)
      ),
      plotOutput(
        "plotting_plotout",
        height = 600,
        click = "plotting_plotout_click",
        brush = brushOpts(id = "plotting_plotout_brush")
      ),
      hr(),
      fluidRow(
        column(
          3,
          h4("Compute traces"),
          selectizeInput("plotting_choose_1cXe", "Choose Core", choices = NULL), # only shown for 1cXe mode
          shinyWidgets::pickerInput(
            inputId = "plotting_choose_Xc1e",
            label = "Choose multiple cores",
            choices = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ), # only shown for Xc1e mode
          textOutput("plotting_longcorename"), # only shown for longcore mode
          shinyWidgets::pickerInput(
            inputId = "plotting_chooseproxies",
            label = "Calculate ratios? Choose proxies",
            choices = NULL,
            options = list(`actions-box` = TRUE),
            multiple = TRUE
          ), # always shown
          fileInput("plotting_addtraces", "Upload additional traces (*.csv)", multiple = TRUE, accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )), # only shown for 1cXe mode
          shinyWidgets::pickerInput(
            inputId = "plotting_choosetraces",
            label = "Choose traces to plot",
            choices = NULL,
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE
          ), # shown for 1cXe mode and longcore mode
          shinyWidgets::pickerInput(
            inputId = "plotting_choosetrace_Xc1e",
            label = "Choose trace to plot",
            choices = NULL,
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = FALSE
          ), # shown for Xc1e mode
          p()
        ),
        column(
          3,
          h4("Plot options"),
          selectizeInput(
            inputId = "plotting_theme",
            label = "Choose plot theme",
            choices = names(gg2themes)
          ),
          actionButton("plotting_redraw", "(Re)draw plot", class = "btn-primary"), # always shown
          downloadButton("plotting_saveplot", "Download plot"),
          p()
        ),
        column(
          3,
          h4("Data export"),
          checkboxGroupInput(
            inputId = "export_includes",
            label = "Include for download:",
            choices = list("Excel workbook" = "excel", "CSV files" = "csv", "RData file" = "rda"),
            selected = c("csv", "rda")
          ),
          downloadButton("export_download", "Download data"),
          p()
        )

      )
    )
  )
  # Server ####
  server <- function(input, output, session) {

    ## Reactive functions ####
    ### parse xrf event ####
    parse_xrf_eventreactive <- eventReactive(input$import_loaddata, {
      datasource <- req(input$import_choosesource)
      switch(datasource,
        "sampledata" = {
          fpath <-
            req(system.file(
              "extdata",
              c("MI18-L8-A_10kV_raw.csv", "MI18-L8-A_30kV_raw.csv", "ZH19-35_10kV.csv", "ZH19-35_30kV.csv", "ZH19-35_50kV_onlySn.csv", "ZH19-35_50kV.csv"),
              package = "carrrotter"
            ))
        },
        "longcore_sampledata" = {
          fpath <-
            req(system.file(
              "extdata",
              c("MI18-L8-A_10kV_raw.csv", "MI18-L8-A_30kV_raw.csv", "MI18-L8-B_10kV_raw.csv", "MI18-L8-B_30kV_raw.csv"),
              package = "carrrotter"
            ))
        },
        "fileupload" = {
          fpath <- req(input$import_xrffiles)
          fpath <- fpath$datapath
        }
      )

      files <- fpath
      no_files <- length(files)

      withProgress(
        message = "Parsing XRF files",
        detail = "This may take a moment...",
        value = 0,
        {
          xrf_complete <- purrr::map2_dfr(
            files,
            seq_along(files),
            function(x, y) {
              xrf_parsed <- parse_avaatech_baxil_csv(x)
              setProgress(y / no_files)
              xrf_parsed
            }
          )
        }
      )

      xrf_complete
    })

    ### filter xrf ####
    tidy_xrf_reactive <- reactive({
      xrf_complete <- req(parse_xrf_eventreactive())

      xrf_Inc_Coh_filtered <-
        xrf_complete[xrf_complete$Scattering == "", ]
      xrf_grouped_element_voltage <-
        dplyr::group_by(xrf_Inc_Coh_filtered, .data$Element, .data$Voltage)
      xrf_chi2_summary <-
        dplyr::summarise(xrf_grouped_element_voltage, totalchi2 = sum(.data$Chi2))
      xrf_chi2_ascending <-
        dplyr::arrange(xrf_chi2_summary, .data$Element, .data$totalchi2)
      xrf_chi2_min <-
        dplyr::top_n(xrf_chi2_ascending, 1, .data$totalchi2)
      xrf_chi2_min_selected <-
        dplyr::select(xrf_chi2_min, .data$Element, .data$Voltage)
      xrf_min_joined <-
        dplyr::left_join(xrf_chi2_min_selected,
          xrf_complete,
          by = c("Element", "Voltage")
        )
      xrf_element_voltage_filtered <-
        dplyr::select(
          xrf_min_joined,
          .data$CoreID,
          .data$Depth,
          .data$Voltage,
          .data$Current,
          .data$Element,
          tidyselect::everything()
        )

      xrf_element_voltage_filtered <-
        dplyr::ungroup(xrf_element_voltage_filtered)

      xrfdata_without_repeats <-
        dplyr::distinct(
          xrf_element_voltage_filtered,
          .data$CoreID,
          .data$Element,
          .data$Voltage,
          .data$Depth,
          .data$Current,
          .data$AbsLine,
          .data$Scattering,
          .keep_all = TRUE
        )

      xrfdata_without_repeats <-
        dplyr::mutate(xrfdata_without_repeats, relstdev = .data$cpsStd / .data$cps * 100)
      xrfdata_without_repeats <-
        dplyr::ungroup(xrfdata_without_repeats)
      uniquecoreid <- unique(xrfdata_without_repeats$CoreID)
      excludeddata$exclelem <-
        stats::setNames(vector("list", length(req(uniquecoreid))), req(uniquecoreid))
      excludeddata$excldepth <-
        stats::setNames(vector("list", length(req(uniquecoreid))), req(uniquecoreid))
      xrfdata_without_repeats
    })

    ### create longcore sectdf ####
    create_xrf_longcore_sectdf_reactive <- reactive({
      # create_xrf_longcore_sectdf_reactive()
      ## This reactive function adds, if import_catmode == TRUE, a new column with the overall depth z. The function expects a letter at the end of the CoreID to denote the section number. The order can be chosen to be ascending (A section at surface, B deeper, and so on) or descending (A deepest section, B next one, so on).
      ## At this point there is no support for other Longcore names/CoreID schemes.
      if (input$import_catmode) {
        tmp_xrf_longcore_grp_coreid <- dplyr::group_by(tidy_xrf_reactive(), .data$CoreID)
        tmp_xrf_longcore_grp_coreid_added1 <- dplyr::mutate(tmp_xrf_longcore_grp_coreid, Depth = .data$Depth + 1)
        tmp_longcore_xrf_summarised <- dplyr::summarise(tmp_xrf_longcore_grp_coreid_added1, Length = max(.data$Depth))
        sectdf <- dplyr::mutate(tmp_longcore_xrf_summarised, SectionID = stringr::str_extract(.data$CoreID, "[[:alpha:]]+$"), LongcoreName = stringr::str_extract(.data$CoreID, ".+(?=\\W[[:alpha:]]+)"))

        validate(
          need(!(any(is.na(sectdf$LongcoreName))), message = "Multiple longcore names were found but you chose to use concatenate mode. Make sure to use the same name and latin letters for sections at the end. E.g: XYZ19-8-A or WXYZ-HIJ1-E")
        )

        if (input$import_descorder) {
          tmp_sectdf_desc <- dplyr::arrange(sectdf, dplyr::desc(.data$CoreID))
          sectdf <- dplyr::mutate(tmp_sectdf_desc, z = cumsum(.data$Length), z = c(0, .data$z[-length(.data$z)]))
        } else {
          tmp_sectdf_asc <- dplyr::arrange(sectdf, .data$CoreID)
          sectdf <- dplyr::mutate(tmp_sectdf_asc, z = cumsum(.data$Length), z = c(0, .data$z[-length(.data$z)]))
        }
      }
    })

    ### join longcore data ####
    join_xrf_longcore_data_reactive <- reactive({
      # join_xrf_longcore_data_reactive()
      ## This function joins the depth/length information generated before

      if (input$import_catmode) {
        tmp_xrf_longcore_joined <- dplyr::left_join(create_xrf_longcore_sectdf_reactive(), tidy_xrf_reactive(), by = "CoreID")
        xrfdata <- dplyr::mutate(tmp_xrf_longcore_joined, z = .data$z + .data$Depth)
      } else {
        tidy_xrf_reactive()
      }
    })

    ### filter current core ####
    filter_xrf_per_coreid_reactive <- reactive({
      # filter_xrf_per_coreid_reactive()
      ## This function filters the dataset per CoreID for the diagnostics page. That way we don't need to filter in plot commands.
      curcore <- req(input$diagnostics_core_id)
      xrfdata_filtered_coreid <- dplyr::filter(join_xrf_longcore_data_reactive(), .data$CoreID %in% curcore)

      xrfdata_filtered_coreid
    })

    ### clean diagnosed xrf ####
    clean_diagnosed_xrf_reactive <- reactive({
      # clean_diagnosed_xrf_reactive()
      ## This function uses the data from the reactive Value excludeddata to filter out deselected measurements and elements. In the end, negative cps (physically impossible and troublesome for log ratios) are removed as well.
      xrfdata_without_repeats <- req(join_xrf_longcore_data_reactive())

      if (!rlang::is_empty(excludeddata$exclelem)) {
        tmp_elemdf <-
          dplyr::mutate(
            purrr::map_dfr(
              excludeddata$exclelem,
              ~ tibble::enframe(as.character(.), name = NULL),
              .id = "CoreID"
            ),
            elem_removed = TRUE
          )
        elemdf <- dplyr::rename(tmp_elemdf, Element = .data$value)
      }

      if (!rlang::is_empty(excludeddata$excldepth)) {
        tmp_depthdf <-
          dplyr::mutate(
            purrr::map_dfr(
              excludeddata$excldepth,
              ~ tibble::enframe(as.character(.), name = NULL),
              .id = "CoreID"
            ),
            depth_removed = TRUE
          )
        depthdf <- dplyr::rename(tmp_depthdf, Depth = .data$value)
      }

      xrfdata_cleaned <- xrfdata_without_repeats

      if (exists("elemdf") & exists("depthdf")) {
        if (!(nrow(elemdf) == 0 | nrow(depthdf) == 0)) {
          depthdf <- dplyr::mutate(depthdf, Depth = as.numeric(.data$Depth))
          tmp_xrf_depthdf_joined <-
            dplyr::left_join(xrfdata_without_repeats, depthdf, by = c("CoreID", "Depth"))
          tmp_xrf_elemdf_joined <-
            dplyr::left_join(tmp_xrf_depthdf_joined, elemdf, by = c("CoreID", "Element"))
          tmp_xrf_na_replaced <-
            tidyr::replace_na(tmp_xrf_elemdf_joined, list(depth_removed = FALSE, elem_removed = FALSE))
          xrfdata_cleaned <-
            dplyr::filter(tmp_xrf_na_replaced, !(.data$depth_removed |
              .data$elem_removed))
        }
      }

      xrfdata_cleaned
    })

    ### compute plotting xrf ####
    compute_plotting_xrf_reactive <- reactive({
      cleaned <- req(clean_diagnosed_xrf_reactive())
      pmode <- req(input$plotting_mode)
      proxies <- plotting_proxies$proxylist

      calcproxy <- function(x) {
        dot1 <- dplyr::group_by(XRFdata, .data$Depth)
        dot2 <- dplyr::mutate(dot1, `:=`(!!paste(x, "ratio", sep = "_"), .data$cps / .data$cps[.data$Element %in% !!x]))
        dot3 <- dplyr::ungroup(dot2)
        dot4 <- dplyr::select(dot3, !!paste(x, "ratio", sep = "_"))
        dot4
      }

      calcproxy_Xc1e <- function(x) {
        dot1 <- dplyr::group_by(XRFdata_Xc1e, .data$CoreID, .data$Depth)
        dot2 <- dplyr::mutate(dot1, `:=`(!!paste(x, "ratio", sep = "_"), .data$cps / .data$cps[.data$Element %in% !!x]))
        dot3 <- dplyr::ungroup(dot2)
        dot4 <- dplyr::select(dot3, !!paste(x, "ratio", sep = "_"))
        dot4
      }

      calcproxy_longcore <- function(x) {
        dot1 <- dplyr::group_by(XRFdata_longcore, .data$z)
        dot2 <- dplyr::mutate(dot1, `:=`(!!paste(x, "ratio", sep = "_"), .data$cps / .data$cps[.data$Element %in% !!x]))
        dot3 <- dplyr::ungroup(dot2)
        dot4 <- dplyr::select(dot3, !!paste(x, "ratio", sep = "_"))
        dot4
      }

      switch(pmode,
        "1cXe" = {
          core_1c <- req(input$plotting_choose_1cXe)
          dot1 <- dplyr::filter(cleaned, .data$CoreID %in% core_1c)
          XRFdata <- dplyr::select(dot1, .data$Depth, .data$Element, .data$cps)

          if (!rlang::is_empty(input$plotting_addtraces)) {
            additionaltraces$filenames <- input$plotting_addtraces$name

            additionaltraces$tracesdata <-
              purrr::map(
                input$plotting_addtraces$datapath,
                ~ (readr::read_delim(.x, delim = ";"))
              )

            validate(need(all(
              purrr::map_lgl(
                additionaltraces$tracesdata,
                ~ (colnames(.x)[1] == "Depth")
              )
            ),
            message = "Depth not found. Please check the format of the csv file."
            ))

            traces_plotting <- dplyr::mutate(purrr::reduce(additionaltraces$tracesdata, dplyr::full_join, by = "Depth"), Depth = 10 * .data$Depth)
          }

          if (rlang::is_empty(proxies) &
            rlang::is_empty(additionaltraces$filenames)) {
            xrf_plotting <- dplyr::transmute(XRFdata, Depth = .data$Depth, Varname = paste(.data$Element, "cps"), Value = .data$cps)
          }

          if (rlang::is_empty(proxies) &
            !rlang::is_empty(additionaltraces$filenames)) {
            dot1 <- dplyr::transmute(XRFdata, Depth = .data$Depth, Varname = paste(.data$Element, "cps"), Value = .data$cps)
            dot2 <- tidyr::pivot_wider(dot1, names_from = "Varname", values_from = "Value")
            dot3 <- dplyr::full_join(dot2, traces_plotting, by = "Depth")
            dot4 <- tidyr::pivot_longer(dot3, -1, names_to = "Varname", values_to = "Value")
            xrf_plotting <- tidyr::drop_na(dot4)
          }

          if (!rlang::is_empty(proxies) &
            rlang::is_empty(additionaltraces$filenames)) {
            dot1 <- dplyr::group_modify(XRFdata, ~ purrr::map_dfc(proxies, calcproxy))
            dot2 <- dplyr::bind_cols(XRFdata, dot1)
            dot3 <- tidyr::pivot_longer(dot2, -c("Depth", "Element"), names_to = "Measure", values_to = "Value")
            dot4 <- dplyr::mutate(dot3, Varname = dplyr::if_else(stringr::str_detect(.data$Measure, "ratio"), paste0(.data$Element, "/", stringr::str_extract(.data$Measure, "[:alpha:]+")), paste(.data$Element, .data$Measure)))
            xrf_plotting <- dplyr::select(dot4, -c("Element", "Measure"))
          }

          if (!rlang::is_empty(proxies) &
            !rlang::is_empty(additionaltraces$filenames)) {
            dot1 <- dplyr::group_modify(XRFdata, ~ purrr::map_dfc(proxies, calcproxy))
            dot2 <- dplyr::bind_cols(XRFdata, dot1)
            dot3 <- tidyr::pivot_longer(dot2, -c("Depth", "Element"), names_to = "Measure", values_to = "Value")
            dot4 <- dplyr::mutate(dot3, Varname = dplyr::if_else(stringr::str_detect(.data$Measure, "ratio"), paste0(.data$Element, "/", stringr::str_extract(.data$Measure, "[:alpha:]+")), paste(.data$Element, .data$Measure)))
            dot5 <- dplyr::select(dot4, -c("Element", "Measure"))
            dot6 <- tidyr::pivot_wider(dot5, names_from = "Varname", values_from = "Value")
            dot7 <- dplyr::full_join(dot6, traces_plotting, by = "Depth")
            dot8 <- tidyr::pivot_longer(dot7, -1, names_to = "Varname", values_to = "Value", -1)

            xrf_plotting <- tidyr::drop_na(dot8)
          }
        },
        "Xc1e" = {
          cores_Xc <- req(plotting_choose_Xc1e_debounced())

          dot1 <- dplyr::select(cleaned, .data$CoreID, .data$Depth, .data$Element, .data$cps)
          XRFdata_Xc1e <- dplyr::filter(dot1, .data$CoreID %in% cores_Xc)

          if(!rlang::is_empty(proxies)) {
            dot1 <- dplyr::group_modify(XRFdata_Xc1e, ~ purrr::map_dfc(proxies, calcproxy_Xc1e))
            XRFdata_Xc1e <- dplyr::bind_cols(XRFdata_Xc1e, dot1)
          }

          dot3 <- tidyr::pivot_longer(XRFdata_Xc1e, -c("CoreID", "Depth", "Element"), names_to = "Measure", values_to = "Value")
          dot4 <- dplyr::mutate(dot3, Varname = dplyr::if_else(stringr::str_detect(.data$Measure, "ratio"), paste0(.data$Element, "/", stringr::str_extract(.data$Measure, "[:alpha:]+")), paste(.data$Element, .data$Measure)))

          xrf_plotting <- dplyr::select(dot4, -c("Element", "Measure"))
        },
        "longcore" = {
          XRFdata_longcore <- dplyr::select(cleaned, .data$z, .data$SectionID, .data$Element, .data$cps)

          if(!rlang::is_empty(proxies)) {
            dot1 <- dplyr::group_modify(XRFdata_longcore, ~ purrr::map_dfc(proxies, calcproxy_longcore))
            XRFdata_longcore <- dplyr::bind_cols(XRFdata_longcore, dot1)
          }
          dot3 <- tidyr::pivot_longer(XRFdata_longcore, -c("z", "SectionID", "Element"), names_to = "Measure", values_to = "Value")
          dot4 <- dplyr::mutate(dot3, Varname = dplyr::if_else(stringr::str_detect(dot3$Measure, "ratio"), paste0(dot3$Element, "/", stringr::str_extract(dot3$Measure, "[:alpha:]+")), paste(dot3$Element, dot3$Measure)))
          xrf_plotting <- dplyr::select(dot4, -c("Element", "Measure"))
        }
      )

      xrf_plotting
    })
    ### redraw plot event ####
    redraw_plot_eventreactive <- eventReactive(input$plotting_redraw, {
      pmode <- req(input$plotting_mode)
      plotdata <- compute_plotting_xrf_reactive()
      gg2theme <- req(plotting_gg2theme())

      switch(pmode,
        "1cXe" = {
          plotdata_1cXe <- dplyr::filter(plotdata, .data$Varname %in% req(input$plotting_choosetraces))

          p <- ggplot2::ggplot(data = plotdata_1cXe, ggplot2::aes(x = .data$Depth, y = .data$Value)) +
            ggplot2::facet_grid(. ~ .data$Varname, scales = "free_x") +
            ggplot2::scale_x_reverse("Depth [mm]") +
            ggplot2::scale_y_continuous("") +
            ggplot2::coord_flip() +
            gg2theme +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
            ggplot2::geom_line()
        },
        "Xc1e" = {
          plotdata_Xc1e <- dplyr::filter(plotdata, .data$Varname %in% req(input$plotting_choosetrace_Xc1e))
          p <- ggplot2::ggplot(data = plotdata_Xc1e, ggplot2::aes(x = .data$Depth, y = .data$Value)) +
            ggplot2::facet_grid(.data$Varname ~ .data$CoreID, scales = "free_x") +
            ggplot2::scale_x_reverse("Depth [mm]") +
            ggplot2::scale_y_continuous("") +
            ggplot2::coord_flip() +
            gg2theme +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
            ggplot2::geom_line()
        },
        "longcore" = {
          plotdata_longcore <- dplyr::filter(plotdata, .data$Varname %in% req(input$plotting_choosetraces))
          p <- ggplot2::ggplot(data = plotdata_longcore, ggplot2::aes(x = .data$z, y = .data$Value, colour = .data$SectionID)) +
            ggplot2::facet_grid(. ~ .data$Varname, scales = "free_x") +
            ggplot2::scale_x_reverse("Depth [mm]") +
            ggplot2::scale_y_continuous("") +
            ggplot2::coord_flip() +
            gg2theme +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
            ggplot2::geom_line()
        }
      )

      p
    })

    # Other reactive functions

    diagnostics_currentxlims <- reactive({
      # diagnostics_currentxlims()
      ## We can't read from reactive values inside reactiveValues(), so we use reactive() to read out the current chosen CoreID and the maximum depth to create a vector for the x limits.
      xlims <- c(min(filter_xrf_per_coreid_reactive()$Depth), max(filter_xrf_per_coreid_reactive()$Depth))
    })

    ## here we create an empty reactiveValue to hold our deselected data of all CoreIDs
    excludeddata <- reactiveValues()

    additionaltraces <- reactiveValues()

    diagnostics_elements_excl <- reactive({
      input$diagnostics_elements_excl
    })

    diagnostics_elements_excl_debounced <- debounce(diagnostics_elements_excl, 1000)

    plotting_choose_Xc1e <- reactive({
      input$plotting_choose_Xc1e
    })

    plotting_choose_Xc1e_debounced <- debounce(plotting_choose_Xc1e, 1000)

    plotting_chooseproxies <- reactive({
      input$plotting_chooseproxies
    })

    plotting_chooseproxies_debounced <- debounce(plotting_chooseproxies, 1000)

    plotting_proxies <- reactiveValues()

    plotting_gg2theme <- reactive({
      gg2themes[[input$plotting_theme]]
    })

    ## empty reactiveValue to hold filenames and data of additional traces
    # additionaltraces <- reactiveValues()

    ## Observers ####

    ### Import page ####

    observeEvent(input$import_choosesource, {
      if (input$import_choosesource != "fileupload") {
        shinyjs::disable("import_xrffiles")
      } else {
        shinyjs::enable("import_xrffiles")
      }
    })

    observeEvent(input$import_catmode, {
      ## Here we initialise selectize input on the import page for the preview element when import_catmode == TRUE.
      updateSelectizeInput(
        session,
        "import_catpreview_element",
        choices = unique(join_xrf_longcore_data_reactive()$Element),
        server = TRUE
      )
    })

    observeEvent(input$import_catmode, {
      ## When import_catmode == TRUE, we show the preview plot and overview table on the import page, otherwise we hide it.
      shinyjs::toggleState("import_descorder")
      shinyjs::toggleState("import_catpreview_element")
    })


    ### Diagnostic page ####

    observeEvent(input$diagnostics_core_id, {
      ## Here we initialise the X limits range slide on the diagnostics page
      updateSliderInput(
        session,
        "diagnostics_xlims",
        value = diagnostics_currentxlims(),
        min = diagnostics_currentxlims()[1],
        max = diagnostics_currentxlims()[2]
      )
    })

    observe({
      ## Here we initialise the CoreID selectize input on the diagnostics page
      updateSelectizeInput(
        session,
        "diagnostics_core_id",
        choices = unique(join_xrf_longcore_data_reactive()$CoreID),
        server = TRUE
      )
    })

    observe({
      ## Here we initialise and update the Element picker on the diagnostics page. The control gets reset when the CoreID is changed (invalidated dependency).
      shinyWidgets::updatePickerInput(
        session,
        "diagnostics_elements_excl",
        selected = unique(filter_xrf_per_coreid_reactive()$Element),
        choices = unique(filter_xrf_per_coreid_reactive()$Element)
      )
    })

    observeEvent(input$diagnostics_elements_excl, {
      ## Here we check which Elements are deselected and write them into the excludeddata$exclelem list per CoreID. This means that changing to a CoreID that already has an exclelem list that is not NULL will overwrite the content! NB: Avoiding to overwrite existing data with NULL from another CoreID is not smart because the input gets initialized with all Elements.
      deselected <-
        setdiff(unique(filter_xrf_per_coreid_reactive()$Element), input$diagnostics_elements_excl)
      excludeddata$exclelem[[input$diagnostics_core_id]] <- deselected
    })

    observeEvent(input$diagnostics_exclude_rmpoints, {
      ## Only executed when actionbutton "remove points" is clicked. We choose all possible fields of interest in brushedDepths to be able to brush in every of the three plot modes. Then we append the data to the list excldepth, making sure not to write duplicates.
      brushedDepths <- filter_xrf_per_coreid_reactive()
      brushedDepths <- dplyr::select(
        brushedDepths,
        .data$CoreID,
        .data$Depth,
        .data$Element,
        .data$relstdev,
        .data$Chi2,
        .data$cps
      )

      current_yvar <- switch(input$diagnostics_diagmode,
        "cpsdiag" = "cps",
        "relstdevdiag" = "relstdev",
        "chisqdiag" = "Chi2"
      )

      brushed_contents <- req(input$diagnostics_diagnosticsplot_brush)
      selpoints <-
        brushedPoints(brushedDepths,
          brushed_contents,
          xvar = "Depth", yvar = current_yvar
        )

      excludeddata$excldepth[[input$diagnostics_core_id]] <-
        c(
          selpoints$Depth,
          setdiff(excludeddata$excldepth[[input$diagnostics_core_id]], selpoints$Depth)
        )
    })

    observeEvent(input$diagnostics_exclude_reset, {
      ## Only executed when the "reset" button on the diagnostics page is pressed: Removes all excluded depths from the excldepth list.
      excludeddata$excldepth[[input$diagnostics_core_id]] <- NULL
    })


    ### Plotting page ####

    observeEvent(clean_diagnosed_xrf_reactive(), {
      cleaned <- clean_diagnosed_xrf_reactive()

      if (any(stringr::str_detect(colnames(cleaned), "LongcoreName"))) {
        updateSelectizeInput(
          session,
          "plotting_mode",
          server = TRUE,
          choices = c(
            "1 Core (Section) - X Elements" = "1cXe",
            "X Cores (Sections) - 1 Element" = "Xc1e",
            "Longcore" = "longcore"
          )
        )
      } else {
        updateSelectizeInput(
          session,
          "plotting_mode",
          server = TRUE,
          choices = c(
            "1 Core (Section) - X Elements" = "1cXe",
            "X Cores (Sections) - 1 Element" = "Xc1e"
          )
        )
      }

      updateSelectizeInput(
        session,
        "plotting_choose_1cXe",
        server = TRUE,
        choices = unique(cleaned$CoreID)
      )

      shinyWidgets::updatePickerInput(
        session,
        "plotting_choose_Xc1e",
        choices = unique(cleaned$CoreID)
      )
    })

    observeEvent(input$plotting_mode, {
      ## If the user changes the plotting mode, we show/hide the already existing input controls. This way we can initialise the input controls and be sure that they already exist (e.g. not are created on demand with renderUI)

      pmode <- req(input$plotting_mode)
      switch(pmode,
        "1cXe" = {
          shinyjs::show(id = "plotting_choose_1cXe")
          shinyjs::show(id = "plotting_addtraces")
          shinyjs::show(id = "plotting_choosetraces")

          shinyjs::hide(id = "plotting_choose_Xc1e")
          shinyjs::hide(id = "plotting_longcorename")
          shinyjs::hide(id = "plotting_choosetrace_Xc1e")
        },
        "Xc1e" = {
          shinyjs::show(id = "plotting_choose_Xc1e")
          shinyjs::show(id = "plotting_choosetrace_Xc1e")

          shinyjs::hide(id = "plotting_choose_1cXe")
          shinyjs::hide(id = "plotting_addtraces")
          shinyjs::hide(id = "plotting_longcorename")
          shinyjs::hide(id = "plotting_choosetraces")
        },
        "longcore" = {
          shinyjs::show(id = "plotting_longcorename")
          shinyjs::show(id = "plotting_choosetraces")

          shinyjs::hide(id = "plotting_choose_1cXe")
          shinyjs::hide(id = "plotting_choose_Xc1e")
          shinyjs::hide(id = "plotting_addtraces")
          shinyjs::hide(id = "plotting_choosetrace_Xc1e")
        }
      )
    })

    observeEvent(input$plotting_choose_1cXe, {
      cleaned <- clean_diagnosed_xrf_reactive()
      dot1 <- dplyr::filter(cleaned, .data$CoreID %in% req(input$plotting_choose_1cXe))
      dot2 <- dplyr::select(dot1, .data$Element)
      dot3 <- dplyr::distinct(dot2)
      clean_choices <- dot3[[1]]

      shinyWidgets::updatePickerInput(
        session,
        "plotting_chooseproxies",
        choices = unique(clean_choices)
      )
    })

    observeEvent(plotting_choose_Xc1e_debounced(), {
      cleaned <- clean_diagnosed_xrf_reactive()
      dot1 <- dplyr::filter(cleaned, .data$CoreID %in% req(plotting_choose_Xc1e_debounced()))
      dot2 <- dplyr::select(dot1, .data$Element)
      dot3 <- dplyr::distinct(dot2)
      clean_choices <- dot3[[1]]

      shinyWidgets::updatePickerInput(
        session,
        "plotting_chooseproxies",
        choices = unique(clean_choices)
      )
    })

    observeEvent(plotting_chooseproxies_debounced(), {
      plotting_proxies$proxylist <- req(plotting_chooseproxies_debounced())
    })

    observeEvent(compute_plotting_xrf_reactive(), {
      computed <- compute_plotting_xrf_reactive()

      ## filter out elements that have been excluded previously for 1 or more cores -> common elements only

      shinyWidgets::updatePickerInput(
        session,
        "plotting_choosetraces",
        choices = unique(computed$Varname)
      )

      shinyWidgets::updatePickerInput(
        session,
        "plotting_choosetrace_Xc1e",
        choices = unique(computed$Varname)
      )
    })


    ## Outputs ####

    ### Import page ####

    output$sectiontable <- DT::renderDT(
      {
        ## Creating the sections table on the import page (only viewed if import_catmode == TRUE)
        create_xrf_longcore_sectdf_reactive()
      },
      options = list(paging = FALSE, searching = FALSE)
    )

    output$catplot <- renderPlot({
      if (input$import_catmode) {
        catpreview <- dplyr::filter(join_xrf_longcore_data_reactive(), .data$Element == req(input$import_catpreview_element))
        ggplot2::ggplot(catpreview, ggplot2::aes(x = .data$z, y = .data$cps)) +
          ggplot2::geom_line(ggplot2::aes(colour = factor(.data$SectionID))) +
          ggplot2::xlab("Depth [mm]") +
          ggplot2::scale_y_continuous("counts per second (cps)", limits = c(NA, NA)) +
          ggplot2::labs(color = "Section ID")
      }
    })

    ### Diagnostic page ####

    output$diagnostics_coreid_text <- renderText({
      ## Printing the chosen CoreID on the diagnostics page in the second column at the bottom.
      input$diagnostics_core_id
    })

    output$diagnostics_excltable <- DT::renderDT(
      {
        ## Printing a table showing deselected measurements and elements on the diagnostics page.
        dot1 <- dplyr::group_by(purrr::map_dfr(req(excludeddata$exclelem), ~ tibble::enframe(as.character(.), name = NULL), .id = "CoreID"), .data$CoreID)
        df_elem <- dplyr::summarise(dot1, ExcludedElements = paste0(.data$value, collapse = ", "))

        dot1 <- dplyr::group_by(purrr::map_dfr(req(excludeddata$excldepth), ~ tibble::enframe(as.character(.), name = NULL), .id = "CoreID"), .data$CoreID)
        df_depths <- dplyr::summarise(dot1, ExcludedDepths = paste0(.data$value, collapse = ", "))
        dplyr::full_join(df_elem, df_depths)
      },
      options = list(paging = FALSE, searching = FALSE),
      rownames = FALSE
    )

    output$diagnostics_diagnosticsplot <- renderPlot({
      ## Diagnostic plots: Three different modes
      excldepths <- excludeddata$excldepth[[input$diagnostics_core_id]]

      if (input$diagnostics_diagmode == "relstdevdiag") {
        XRF_relstdev <- filter_xrf_per_coreid_reactive()

        dot1 <- dplyr::group_by(XRF_relstdev, .data$CoreID, .data$Element)
        dot2 <- dplyr::summarise(dot1, keepmedquorum = stats::median(abs(.data$relstdev)))
        dot3 <- dplyr::left_join(dot2, XRF_relstdev, by = c("CoreID", "Element"))
        dot4 <- dplyr::mutate(dot3, keepmedquorum = ifelse(.data$keepmedquorum <= req(input$diagnostics_medrelstdev), "Below", "Exceeded"), keepmedquorum = as.factor(.data$keepmedquorum))
        XRF_relstdev_cutoff <- dplyr::filter(dot4, .data$Element %in% req(diagnostics_elements_excl_debounced()), !(.data$Depth %in% !!excldepths))

        p <-
          ggplot2::ggplot(XRF_relstdev_cutoff, ggplot2::aes(x = .data$Depth, y = .data$relstdev)) +
          ggplot2::geom_rect(ggplot2::aes(
            xmin = -Inf,
            xmax = +Inf,
            ymin = -Inf,
            ymax = +Inf,
            fill = .data$keepmedquorum
          ),
          alpha = 0.5
          ) +
          ggplot2::scale_fill_manual("Threshold", values = c(NA, "red")) +
          ggplot2::geom_line() +
          ggplot2::scale_y_continuous(expression(paste(sigma[rel], " Percentage std. deviation")),
            limits = c(input$diagnostics_ymin, input$diagnostics_ymax)
          ) +
          ggplot2::geom_hline(ggplot2::aes(yintercept = input$diagnostics_maxrelstdev), colour = "red") +
          ggplot2::facet_wrap(.data$CoreID ~ .data$Element, scales = "free") +
          ggplot2::scale_x_continuous("Depth [mm]", limits = input$diagnostics_xlims)
      }

      if (input$diagnostics_diagmode == "chisqdiag") {
        XRF_chisq <- dplyr::filter(filter_xrf_per_coreid_reactive(), .data$Element %in% req(diagnostics_elements_excl_debounced()), !(.data$Depth %in% !!excldepths))
        p <-
          ggplot2::ggplot(XRF_chisq, ggplot2::aes(x = .data$Depth, y = .data$Chi2)) +
          ggplot2::geom_line() +
          ggplot2::scale_y_continuous("goodness of fit", limits = c(input$diagnostics_ymin, input$diagnostics_ymax)) +
          ggplot2::facet_wrap(~ .data$Element, scales = "free") +
          ggplot2::geom_hline(ggplot2::aes(yintercept = input$diagnostics_chi2cutoff), colour = "blue") +
          ggplot2::scale_x_continuous("Depth [mm]", limits = input$diagnostics_xlims)
      }

      if (input$diagnostics_diagmode == "cpsdiag") {
        XRF_cps <- dplyr::filter(filter_xrf_per_coreid_reactive(), .data$Element %in% req(diagnostics_elements_excl_debounced()), !(.data$Depth %in% !!excldepths))
        p <-
          ggplot2::ggplot(XRF_cps, ggplot2::aes(x = .data$Depth, y = .data$cps)) +
          ggplot2::geom_line() +
          ggplot2::scale_y_continuous("cps", limits = c(input$diagnostics_ymin, input$diagnostics_ymax)) +
          ggplot2::facet_wrap(~ .data$Element, scales = "free") +
          ggplot2::scale_x_continuous("Depth [mm]", limits = input$diagnostics_xlims)
      }
      p
    })

    output$diagnostics_saveplot <- downloadHandler(
      filename = function() {
        "XRF_diagnostics.pdf"
      },
      content = function(file) {
        ggplot2::ggsave(file, width = 12, height = 10, device = "pdf")
      },
      contentType = "application/pdf"
    )

    ### Plotting page ####
    output$plotting_saveplot <- downloadHandler(
      filename = function() {
        "XRF_plotting.pdf"
      },
      content = function(file) {
        ggplot2::ggsave(file, width = 12, height = 10, device = "pdf")
      },
      contentType = "application/pdf"
    )


    output$plotting_plotout <- renderPlot({
      ## error handling here needed
      redraw_plot_eventreactive()
    })

    output$plotting_longcorename <- renderText({
      cleaned <- clean_diagnosed_xrf_reactive()
      if (any(stringr::str_detect(colnames(cleaned), "LongcoreName"))) {
        print(paste("Longcore name:", unique(cleaned$LongcoreName)))
      } else {
        return(NULL)
      }
    })

    output$export_download <- downloadHandler(
      filename = function() {
        paste0("shinyCoreScan_export", "_", format(Sys.time(), "%Y-%m-%dT%H_%M_%S"), ".zip")
      },
      content = function(filename) {
        withProgress(
          message = "Preparing Download",
          detail = "This may take a moment...",
          value = 0,
          {
            tmpdir <- tempdir()
            olddir <- getwd()
            setwd(tmpdir)
            tmpdir_root <- "shinyCoreScan"
            tmpdir_csv_sub <- "shinyCoreScan/CSV"
            tmpdir_excel_sub <- "shinyCoreScan/Excel"

            dir.create(tmpdir_root, showWarnings = FALSE)
            dir.create(tmpdir_csv_sub, showWarnings = FALSE)
            dir.create(tmpdir_excel_sub, showWarnings = FALSE)

            export_included_filetypes <- req(input$export_includes)
            progress_length <- length(export_included_filetypes)


            original_data <- parse_xrf_eventreactive()
            longcore_data <- join_xrf_longcore_data_reactive()
            cleaned_data <- clean_diagnosed_xrf_reactive()

            diag_excl <- list("Excluded Depths" = excludeddata$excldepth, "Excluded Elements" = excludeddata$exclelem)

            download_objects <- list("Original Data" = original_data, "Filtered Data" = longcore_data, "Cleaned Data" = cleaned_data)

            if ("rda" %in% export_included_filetypes) {
              objects_rdata <- append(download_objects, list("Diagnose Exclusions" = diag_excl))
              save(objects_rdata, file = paste(tmpdir_root, "shinyCoreScan_all_data.RData", sep = "/"))
            }

            incProgress(1/progress_length)

            sink(file = paste(tmpdir_root, "shinyCoreScan_diagnose_exclusions.txt", sep = "/"))
            print(diag_excl)
            sink()

            jsonlite::write_json(diag_excl, path = paste(tmpdir_root, "shinyCoreScan_diagnose_exclusions.json", sep = "/"))


            if ("excel" %in% export_included_filetypes) {
              openxlsx::write.xlsx(download_objects, file = paste(tmpdir_excel_sub, "shinyCoreScan_all_data.xlsx", sep = "/"))
            }

            incProgress(1/progress_length)

            if ("csv" %in% export_included_filetypes) {
              listnames_filefriendly <- stringr::str_replace_all(stringr::str_to_lower(paste("shinyCoreScan", names(download_objects))), "\\s", "_")
              purrr::walk2(download_objects, listnames_filefriendly, ~ readr::write_csv(.x, paste0(tmpdir_csv_sub, "/", .y, ".csv"), na = ""))
            }

            incProgress(1/progress_length)

            zipdownload <- zip::zip(zipfile = filename, files = tmpdir_root)
            setwd(olddir)
            zipdownload
          }
        )
      },
      contentType = "application/zip"
    )

  }
  shinyApp(ui, server)
}
