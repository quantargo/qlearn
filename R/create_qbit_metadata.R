#' Create Metatdata for QBits
#'
#' Create metadata for Qbits as list objects.
#'
#' @param main_item list;
#' @param files character;
#' @param pkgLock character; Package lock information from renv.lock
#' @param qbit_env environment;
#' @param packagesLoaded character; packages currently loaded
#' @param advertiseQBit logical; Define if qbit shall be advertised.
#' @param tstamp POSIXct; Time stamp to be used for QBit
#' @return list containing metadata
#' @importFrom jsonlite unbox
#' @importFrom utils capture.output str sessionInfo
#' @importFrom yaml read_yaml
#' @export
create_qbit_metadata <- function(
    main_item,
    files,
    pkgLock = NULL,
    qbit_env = NULL,
    packagesLoaded = names(sessionInfo()$otherPkgs),
    advertiseQBit = NULL,
    tstamp = Sys.time()) {

    stopifnot(!is.null(main_item$moduleId))
    stopifnot(!is.null(main_item$contentId))

    qbitModuleId <- main_item$moduleId
    qbitContentId <- main_item$contentId

    file_main <- grep("main\\.R", files, value = TRUE)[1]
    stopifnot(!is.na(file_main))
    code <- paste(readLines(file_main), collapse = "\n")

    tsamp <- format(tstamp, "%Y-%m-%d %H:%M:%OS3Z", tz = "UTC")

    qbit_out <- list()

    ### Create QBit Main Item
    qbitOut <- main_item
    qbitOut$lastModified = unbox(tstamp)

    # Generate Qbit image
    qbit_img_path <- file.path( gsub("#", "/", qbitContentId), "image.png")
    file_qbit_img <- file.path("..", qbit_img_path)
    dir.create(dirname(file_qbit_img), recursive = TRUE, showWarnings = FALSE)
    decode_dev <- sprintf("png(\"%s\")\nprint({%s})\ndev.off()", file_qbit_img, code)
    # eval(parse(text = decode_dev), new.env())
    if (!file.exists(file_qbit_img)) {
    highlight_code_image(code, file_qbit_img)
    }
    qbitOut$image <- unbox(qbit_img_path)
    qbit_out[[length(qbit_out) + 1]]  <- qbitOut

    ### Generate Setup environment file
    currtime <- unbox(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"))
    qbitEnvironmentOut <- list(
        contentId = unbox(paste0(qbitContentId, "#environment")),
        contentType = unbox("environment"),
        createdAt = currtime,
        lastModified = currtime,
        moduleId = unbox(qbitModuleId)
    )

    if (!is.null(qbit_env)) {
        qbit_env_path = file.path("..", file.path( gsub("#", "/", qbitContentId), ".RData"))
        dir.create(dirname(qbit_env_path), recursive = TRUE, showWarnings = FALSE)
        save(list = ls(envir = qbit_env), file = qbit_env_path, envir = qbit_env)
    }

    qbitEnvironmentOut$environment <-
        lapply(names(qbit_env), function(x) capture.output(str(get(x, envir = qbit_env), max.level = 1)))
    names(qbitEnvironmentOut$environment) <- names(qbit_env)
    qbit_out[[length(qbit_out) + 1]]  <- qbitEnvironmentOut

    filemain <- basename(file_main)
    ### Generate QBit Main file
    qbitCodeOut <- list(
    contentId = unbox(paste0(qbitContentId, sprintf("#files#%s", filemain))),
    moduleId = unbox(qbitModuleId),
    content = unbox(code),
    contentType = unbox("file"),
    name = unbox(filemain)
    )
    qbit_out[[length(qbit_out) + 1]]  <- qbitCodeOut

    ## Add README
    file_readme <- grep("README\\.md", files, value = TRUE)[1]
    if(!is.na(file_readme)) {
      readme_text <- paste(readLines(file_readme), collapse = "\n")
      qbitReadmeOut <- list(
        contentId = unbox(paste0(qbitContentId, sprintf("#files#%s", file_readme))),
        moduleId = unbox(qbitModuleId),
        content = unbox(readme_text),
        contentType = unbox("file"),
        name = unbox(file_readme)
      )
      qbit_out[[length(qbit_out) + 1]]  <- qbitReadmeOut

    }

    ### Add packagesLoaded
    qbitPackagesOut <- list(
        contentId = unbox(paste0(qbitContentId, "#packages")),
        moduleId = unbox(qbitModuleId),
        contentType = unbox("content"),
        packageLock = pkgLock
    )
    if(!is.null(packagesLoaded)) {
        qbitPackagesOut$packagesLoaded <- packagesLoaded
    }

    qbit_out[[length(qbit_out) + 1]]  <- qbitPackagesOut

    ### Add qbit advertisement information
    if (!is.null(advertiseQBit)) {
        qbitAdvertiseOut <- list(
            contentId = unbox(sprintf("advertise_%s", qbitContentId)),
            moduleId = unbox(qbitModuleId),
            contentType = unbox("main"),
            createdBy = unbox("SYSTEM"),
            lastModified = unbox(tstamp)
        )
        qbit_out[[length(qbit_out) + 1]]  <- qbitAdvertiseOut
    }

    qbit_out
}

#' Create QBit Metadata
#'
#' @param qbit_path character; Source path of qbit to be deployed.
#' @param pattern_main character; Path to qbit main file to be used.
#' @param index_file character; Path to meta data index file to be used.
#' @param pkgLock character; Path to package lock file.
#' @param files_ignore character; Files to be ignored.
#' @param stage character; Stage to be used for deployment
#' @importFrom yaml read_yaml
#' @importFrom rmarkdown render
#' @export
create_qbit_metadata_path <- function(qbit_path,
                                      pattern_main = "main\\.R",
                                      index_file = file.path(qbit_path, "index.yml"),
                                      pkgLock = file.path(qbit_path, "renv.lock"),
                                      files_ignore = c("renv"),
                                      stage = Sys.getenv("STAGE", "dev")) {

    files <- list.files(qbit_path)
    file_main <- grep(pattern_main, files, value = TRUE)[1]
    stopifnot(!is.na(file_main))

    # Create qbit_env
    mode <- tools::file_ext(file_main)
    main_env <- NULL

    if (tolower(mode) ==  "r") {
        main_env <- new.env()
        content_main = paste(readLines(file_main), collapse = "\n")
        eval(parse(text = content_main), envir = main_env)
    } else if (tolower(mode) ==  "rmd") {
        main_env <- new.env()
        out_file <- render(input = file_main, output_dir = qbit_path, runtime = "static", envir = main_env)

    } else {
        stop(sprintf("File extension %s not supported for main file", mode))
    }
    files <- list.files(qbit_path)
    files <- files[!files %in% files_ignore]

    # Get packagesLoaded from all files in directory
    # TODO: Change
    packagesLoaded <- unique(renv::dependencies()$Package)

    qbit_main <- read_yaml(index_file)
    pkgLock <- jsonlite::read_json(path = pkgLock)
    qbit_main$qbitRuntime <- paste(qbit_main$moduleId, stage, sep = "-")
    qbit_main$moduleType <- "qbit"
    qbit_main$qbitName <- qbit_main$moduleId

    create_qbit_metadata(
      qbit_main,
      files,
      pkgLock,
      main_env,
      packagesLoaded
    )
}
