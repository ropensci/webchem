#' Query SMARTS Viewer
#'
#' Submit a query to the SMARTS viewer webservice at
#' \url{http://smartsview.zbh.uni-hamburg.de}, ZBH Center for Bioinformatics,
#' University of Hamburg
#'
#' @param smarts A SMARTS string to visualize
#' @param output The type of response generated (image or download). See
#'   description for details.
#' @param image_format Image file format (pdf, png, or svg)
#' @param visualization_modus 1 or 2 (1 = Complete Visualization, 2 = Element
#'   Symbols)
#' @param legend_option both, none, static, dynamic
#' @param filename Filename to use if output is to be downloaded
#'
#' @return Either a raster object (output = image) suitable for adding to an
#'   R plot using \code{rasterImage} or the image is downloaded in the specified
#'   format to the indicated filename.
#' @export
#'
#' @examples
#' \dontrun{
#' img <- smarts_viewer(
#'    "[CX3](=[OX1])[OX2][CX3](=[OX1])",
#'    "image", "png", 1, "both"
#'    )
#' plot(0:1,0:1, 'n')
#' rasterImage(img[[1]],0,0,1,1)
#'
#' smarts <- c("[CX3](=[OX1])[OX2][CX3](=[OX1])",
#' "[$([nr5]:[nr5,or5,sr5]),$([nr5]:[cr5]:[nr5,or5,sr5])]",
#' "[#6][$([NX2]=O),$(N=C=O),$(OC#N),$(SC#N)]"
#' )
#' img <- smarts_viewer(smarts, "image", "png", 1, "both")
#' par(mfcol = c(1,length(smarts)))
#' sapply(img, function(i){
#'  plot(0:1,0:1, 'n')
#'  rasterImage(i,0,0,1,1)
#' })
#'
#' smarts_viewer(
#'     smarts, output = "download",
#'     image_format = "pdf",
#'     visualization_modus = 1,
#'     legend_option = "both", filename = "test.pdf")
#' }
#'
smarts_viewer <-
  function(smarts,
           output = c('image', 'download'),
           image_format = c('pdf', 'png', 'svg'),
           visualization_modus = c(1, 2),
           legend_option = c('both', 'none', 'static', 'dynamic'),
           filename = NULL) {
    entity_url <- "https://smartsview.zbh.uni-hamburg.de/auto"
    if(output == 'download' & is.null(filename)){
      stop("Must provide a filename for output type 'download'.")
    }
    if (length(smarts) > 1 &
        output == 'download') {
      message(
        paste(
          "Warning:",
          "Multiple SMARTS strings entered with output option = download.",
          "One file per SMARTS string will be downloaded automatically.",
          sep = '\n'
        )
      )
      resp <- NA
      while (!resp %in% c("Y", "N", "y", "n")) {
        resp <- readline(prompt = "Do you wish to continue? (Y/N)")
      }
      stopifnot(tolower(resp) == 'y')
    }

    lapply(seq(along = smarts), function(x) {
      entity_query <-
        paste(entity_url,
              image_format,
              visualization_modus,
              legend_option,
              utils::URLencode(smarts[x], T),
              sep = '/')
      response <- httr::GET(entity_query)
       if (response$status_code == 200) {
          if (output == 'image') {
            httr::content(response, as = 'parsed', type = 'Image/png')
          } else {
            utils::download.file(url = response$url,
                                 destfile = gsub(
                                   pattern = paste0('.', image_format),
                                   paste0('_', x, '.', image_format),
                                   filename
                                 ))
          }
        } else {
          stop(httr::http_status(response)$message)
        }
    })
  }