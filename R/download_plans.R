# download_plans ----------------------------------------------------------
#' Download conservation plans using URL
#'
#' A helper function for downloading conservation plans and reports of all or a select number (see paramter: 'plans') appearing in data, 'plan_urls'.
#' @param dir Directory within which the downloaded reports will be saved. Defaults to "./data-raw/imports/". If it does not already exist, will create this subdirectory.
#' @param countries Defaults to all regions. Country abbreviation, one or more of c("US", "CAN"), by which the plans will be downloaded.
#' @param states Defaults to all states. State or province abbreviation, one of c("US", "CAN"), by which the plans will be downloaded.
#' @param dl.mode Argument for `r download.file()`. Defaults to "wb" (binary).
#' @param overwrite.plans Defaults to FALSE. Given the size of many of these documents and the low likelihood that they will be edited, FALSE will **not** overwrite the file if it already exists.
#' @param inverse.states Defaults to NULL. If populated and argument 'state' is not defined, will download everything EXCEPT those states/regions provided here.
#' @importFrom magrittr %>%"
#' @examples
#' \donotrun{
#' dir.create("./data-raw/imports)
#' list.files("./data-raw/imports)
#' download_plans()
#' list.files("./data-raw/imports)
#' }
#'

download_plans <-
    function (dir = "./data-raw/imports//",
              countries = NULL,
              states = NULL,
              inverse.states = NULL,
              dl.mode = "wb",
              overwrite.plans = FALSE) {
        # create the dl directory if it does not exist
        if (!dir.exists(dir)) {
            dir.create(dir)
        }#end if

        #load the package url lookup table
        data("plan_urls")


        # SUBSET the lookup table by state, inverstate, and/or region
        # states = c("FL") #for testing
        if (!is.null(states)) {
            plan_urls <- plan_urls %>%
                dplyr::filter(state %in% states)
        }
        # subset download query by state, inverstate, and/or region
        if (!is.null(inverse.states)) {
            plan_urls.subset <- plan_urls %>%
                dplyr::filter(!state %in% states)
        }

        # keep countries
        if (!is.null(countries)) {
            plan_urls <- plan_urls %>%
                dplyr::filter(country %in% countries)
        }

        # DOWNLOAD the remaining from urlsin lookup table
        fn.out <- NULL
        for (i in 1:nrow(plan_urls)) {
            fn <- NULL
            temp <- plan_urls[i, ]
            if (is.na(temp$url_dl))
                next()
            if (is.na(temp$year_start)) {
                year.ind = "NaN"
            }

            fn <-
                paste0(
                    temp$year_start,
                    "_",
                    temp$country,
                    "_",
                    temp$state,
                    "_",
                    temp$plan_type,
                    ".",
                    temp$filetype
                )

            if (!overwrite.plans &
                fn %in% list.files(dir)) {
                message(fn,
                        "already exists and overwrite.plans=FALSE. not downloading this file.")
            } else
                (download.file(
                    url = temp$url_dl,
                    destfile = paste(dir, fn),
                    mode = dl.mode,
                    quiet=TRUE
                ))
            fn.out <- c(fn.out, fn)
        }

        message(
            "for a list of downloaded files, please call `list.files(",
            paste(dir),
            ")`",
            " or browse this directory"
        )

return(fn.out)
    }# end download_plans function