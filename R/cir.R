#' Query Chemical Identifier Resolver
#'
#' A interface to the Chemical Identifier Resolver (CIR).
#'  (\url{https://cactus.nci.nih.gov/chemical/structure_documentation}).
#'
#' @import xml2
#' @importFrom utils URLencode
#'
#' @param identifier character; chemical identifier.
#' @param representation character; what representation of the identifier should
#'  be returned. See details for possible representations.
#' @param resolver character; what resolver should be used? If NULL (default)
#'  the identifier type is detected and the different resolvers are used in turn.
#'  See details for possible resolvers.
#' @param match character; How should multiple hits be handled? \code{"all"}
#' returns all matches, \code{"first"} returns only the first result,
#' \code{"ask"} enters an interactive mode and the user is asked for input,
#' \code{"na"} returns \code{NA} if multiple hits are found.
#' @param choices deprecated.  Use the \code{match} argument instead.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return A list of character vectors.
#' @details
#'  CIR can resolve can be of the following \code{identifier}: Chemical Names,
#'  IUPAC names,
#'  CAS Numbers, SMILES strings, IUPAC InChI/InChIKeys, NCI/CADD Identifiers,
#'  CACTVS HASHISY, NSC number, PubChem SID, ZINC Code, ChemSpider ID,
#'  ChemNavigator SID, eMolecule VID.
#'
#'  \code{cir_query()} can handle only a part of all possible conversions of CIR.
#'  Possible \code{representations} are:
#'  \itemize{
#'      \item \code{'smiles'}(SMILES strings),
#'      \item \code{'names'} (Names),
#'      \item \code{'cas'} (CAS numbers),
#'      \item \code{'stdinchikey'} (Standard InChIKey),
#'      \item \code{'stdinchi'} (Standard InChI),
#'      \item \code{'ficts'} (FICTS Identifier),
#'      \item \code{'ficus'} (FICuS Indetifier),
#'      \item \code{'uuuuu'} (uuuuu Identifier),
#'      \item \code{'mw'} (Molecular weight),
#'      \item \code{'monoisotopic_mass'} (Monoisotopic Mass),
#'      \item \code{'formula'} (Chemical Formula),
#'      \item \code{'chemspider_id'} (ChemSpider ID),
#'      \item \code{'pubchem_sid'} (PubChem SID),
#'      \item \code{'chemnavigator_sid'} (ChemNavigator SID),
#'      \item \code{'h_bond_donor_count'} (Number of Hydrogen Bond Donors),
#'      \item \code{'h_bond_acceptor_count'} (Number of Hydrogen Bond Acceptors),
#'      \item \code{'h_bond_center_count'} (Number of Hydrogen Bond Centers),
#'      \item \code{'rule_of_5_violation_count'} (Number of Rule of 5 Violations),
#'      \item \code{'rotor_count'} (Number of Freely Rotatable Bonds),
#'      \item \code{'effective_rotor_count'} (Number of Effectively Rotatable Bonds),
#'      \item \code{'ring_count'} (Number of Rings),
#'      \item \code{'ringsys_count'} (Number of Ring Systems),
#'      \item \code{'xlogp2'} (octanol-water partition coefficient),
#'      \item \code{'aromatic'} (is the compound aromatic),
#'      \item \code{'macrocyclic'} (is the compound macrocyclic),
#'      \item \code{'heteroatom_count'} (heteroatom count),
#'      \item \code{'hydrogen_atom_count'} (H atom count),
#'      \item \code{'heavy_atom_count'} ( Heavy atom count),
#'      \item \code{'deprotonable_group_count'} (Number of deprotonable groups),
#'      \item \code{'protonable_group_count'} (Number of protonable groups).
#'  }
#'
#'  CIR first tries to determine the identifier type submitted and then
#'  uses 'resolvers' to look up the data.
#'  If no \code{resolver} is supplied, CIR tries different resolvers in
#'  turn till a hit is found.
#'  E.g. for names CIR tries first to look up in OPSIN and if this fails
#'  the local name index of CIR.
#'  However, it can be also specified which resolvers to use
#'  (if you know e.g. know your identifier type)
#'  Possible \code{resolvers} are:
#'  \itemize{
#'    \item \code{'name_by_cir'} (Lookup in name index of CIR),
#'    \item \code{'name_by_opsin'} (Lookup in OPSIN),
#'    \item \code{'name_by_chemspider'} (Lookup in ChemSpider,
#'    \url{https://cactus.nci.nih.gov/blog/?p=1386}),
#'    \item \code{'smiles'} (Lookup SMILES),
#'    \item \code{'stdinchikey'}, \code{'stdinchi'} (InChI),
#'    \item \code{'cas_number'} (CAS Number),
#'    \item \code{'name_pattern'} (Google-like pattern search
#'    (\url{https://cactus.nci.nih.gov/blog/?p=1456})
#'    Note, that the pattern search can be combined with other resolvers,
#'    e.g. \code{resolver = 'name_by_chemspider,name_pattern'}.
#'
#'  }
#'
#' @note You can only make 1 request per second (this is a hard-coded feature).
#'
#' @references
#' \code{cir} relies on the great CIR web service created by the CADD
#' Group at NCI/NIH! \cr
#' \url{https://cactus.nci.nih.gov/chemical/structure_documentation}, \cr
#' \url{https://cactus.nci.nih.gov/blog/?cat=10}, \cr
#' \url{https://cactus.nci.nih.gov/blog/?p=1386}, \cr
#' \url{https://cactus.nci.nih.gov/blog/?p=1456}, \cr
#'
#'
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cir_query("Triclosan", "cas")
#' cir_query("3380-34-5", "cas", match = "first")
#' cir_query("3380-34-5", "cas", resolver = "cas_number")
#' cir_query("3380-34-5", "smiles")
#' cir_query("Triclosan", "mw")
#'
#' # multiple inputs
#' comp <- c("Triclosan", "Aspirin")
#' cir_query(comp, "cas", match = "first")
#'
#'}
#' @export
cir_query <- function(identifier, representation = "smiles",
                      resolver = NULL,
                      match = c("all", "first", "ask", "na"),
                      verbose = getOption("verbose"),
                      choices = NULL,
                      ...){

  if (!ping_service("cir")) stop(webchem_message("service_down"))

  if (!missing("choices")) {
    stop("`choices` is deprecated.  Use `match` instead.")
  }
  match <- match.arg(match)
  foo <- function(identifier, representation, resolver, first, verbose) {
    if (is.na(identifier)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    if (verbose) webchem_message("query", identifier, appendLF = FALSE)
    identifier <- URLencode(identifier, reserved = TRUE)
    baseurl <- "https://cactus.nci.nih.gov/chemical/structure"
    qurl <- paste(baseurl, identifier, representation, 'xml', sep = '/')
    if (!is.null(resolver)) {
      qurl <- paste0(qurl, '?resolver=', resolver)
    }
    webchem_sleep(type = 'API')
    h <- try(httr::RETRY("GET",
                         qurl,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
    if (inherits(h, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(h))
    if (h$status_code == 200){
      tt <- read_xml(content(h, as = 'raw'))
      out <- xml_text(xml_find_all(tt, '//item'))
      if (length(out) == 0) {
        if (verbose) webchem_message("not_found")
        return(NA)
      }
      out <- matcher(out, query = identifier, match = match, verbose = verbose)
      if (representation %in% c('mw', 'monoisotopic_mass', 'h_bond_donor_count',
                                'h_bond_acceptor_count', 'h_bond_center_count',
                                'rule_of_5_violation_count', 'rotor_count',
                                'effective_rotor_count', 'ring_count', 'ringsys_count',
                                'xlogp2', 'heteroatom_count', 'hydrogen_atom_count',
                                'heavy_atom_count', 'deprotonable_group_count',
                                'protonable_group_count') )
        out <- as.numeric(out)
      return(out)
    }
    else {
      return(NA)
    }
  }
  out <- lapply(identifier, foo, representation = representation,
                resolver = resolver, first = first, verbose = verbose)
  names(out) <- identifier
  return(out)
}

#' Query Chemical Identifier Resolver Images
#'
#' A interface to the Chemical Identifier Resolver (CIR).
#'  (\url{https://cactus.nci.nih.gov/chemical/structure_documentation}).
#'
#' @param query character; Search term. Can be any common chemical identifier
#' (e.g. CAS, INCHI(KEY), SMILES etc.)
#' @param dir character; Directory to save the image.
#' @param format character; Format of the stored image. Can be on of TODO
#' @param format character; Output format of the image. Can be one of "png",
#' "gif".
#' @param width integer; Width of the image.
#' @param height integer; Height of the image.
#' @param linewidth integer; Width of lines.
#' @param symbolfontsize integer; Fontsize of atoms in the image.
#' @param bgcolor character; E.g. transparent, white, \%23AADDEE
#' @param antialiasing logical; Should antialiasing be used?
#' @param atomcolor character; Color of the atoms in the image.
#' @param bondcolor character; Color of the atom bond lines.
#' @param csymbol character; Can be one of "special" (default - i.e. only
#' hydrogen atoms in functional groups or defining stereochemistry) or "all".
#' @param hsymbol character; Can be one of "special" (default - i.e. none are
#' shown) or "all" (all are printed).
#' @param hcolor character; Color of the hydrogen atoms.
#' @param header character; Should a header text be added to the image? Can be
#' any string.
#' @param footer character; Should a footer text be added to the image? Can be
#' any string.
#' @param verbose logical; Should a verbose output be printed on the console?
#' @param frame integer; Should a frame be plotted? Can be on of NULL (default)
#' or 1.
#' @param ... currently not used.
#'
#' @return image written to disk
#' @details
#'  CIR can resolve can be of the following \code{identifier}: Chemical Names,
#'  IUPAC names,
#'  CAS Numbers, SMILES strings, IUPAC InChI/InChIKeys, NCI/CADD Identifiers,
#'  CACTVS HASHISY, NSC number, PubChem SID, ZINC Code, ChemSpider ID,
#'  ChemNavigator SID, eMolecule VID.
#'
#'  For an image with transparent background use ‘transparent’ as color name and
#'  switch off antialiasing (i.e. antialiasing = 0).
#'
# followed this blog post
# https://cactus.nci.nih.gov/blog/?p=136
#'
#' @note You can only make 1 request per second (this is a hard-coded feature).
#'
#' @references
#' \code{cir} relies on the great CIR web service created by the CADD
#' Group at NCI/NIH! \cr
#' \url{https://cactus.nci.nih.gov/chemical/structure_documentation}, \cr
#' \url{https://cactus.nci.nih.gov/blog/?cat=10}, \cr
#' \url{https://cactus.nci.nih.gov/blog/?p=1386}, \cr
#' \url{https://cactus.nci.nih.gov/blog/?p=1456}, \cr
#'
#'
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cir_img("CCO", dir = tempdir()) # SMILES
#'
#' # multiple query strings and different formats
#' query = c("Glyphosate", "Isoproturon", "BSYNRYMUTXBXSQ-UHFFFAOYSA-N")
#' cir_img(query, dir = tempdir(), bgcolor = "transparent", antialising = 0)
#'
#' # all parameters
#' query  = "Triclosan"
#' cir_img(query,
#'         dir = tempdir(),
#'         format = "png",
#'         width = 600,
#'         height = 600,
#'         linewidth = 5,
#'         symbolfontsize = 30,
#'         bgcolor = "red",
#'         antialiasing = FALSE,
#'         atomcolor = "green",
#'         bondcolor = "yellow",
#'         csymbol = "all",
#'         hsymbol = "all",
#'         hcolor = "purple",
#'         header = "My funky chemical structure..",
#'         footer = "..is just so awesome!",
#'         frame = 1,
#'         verbose = getOption("verbose"))
#'}
#' @export
#'
cir_img <- function(query,
                    dir,
                    format = c("png", "gif"),
                    width = 500,
                    height = 500,
                    linewidth = 2,
                    symbolfontsize = 16,
                    bgcolor = NULL,
                    antialiasing = TRUE,
                    atomcolor = NULL,
                    bondcolor = NULL,
                    csymbol = c("special", "all"),
                    hsymbol = c("special", "all"),
                    hcolor = NULL,
                    header = NULL,
                    footer = NULL,
                    frame = NULL,
                    verbose = getOption("verbose"),
                    ...) {

  if (!ping_service("cir")) stop(webchem_message("service_down"))

  if (is.na(dir) || !dir.exists(dir)) {
    stop('Directory does not exist.')
  }
  format <- match.arg(format)
  csymbol <- match.arg(csymbol, c("special", "all"))
  hsymbol <- match.arg(hsymbol, c("special", "all"))
  foo <- function(query,
                  dir,
                  format,
                  width,
                  height,
                  linewidth,
                  symbolfontsize,
                  bgcolor,
                  antialiasing,
                  atomcolor,
                  bondcolor,
                  csymbol,
                  hsymbol,
                  hcolor,
                  header,
                  footer,
                  frame,
                  verbose,
                  ...) {
    # check
    if (is.na(query) || query == '') {
      message('NA or empty string provided. Query skipped.')
      return(NULL)
    }
    # prolog
    baseurl <- "https://cactus.nci.nih.gov/chemical/structure"
    qurl <- paste(baseurl, query, "image", sep = "/")
    # options
    if (!is.null(format))
      format <- paste0("format=", format)
    if (!is.null(width))
      width <- paste0("width=", width)
    if (!is.null(height))
      height <- paste0("height=", height)
    if (!is.null(linewidth))
      linewidth <- paste0("linewidth=", linewidth)
    if (!is.null(symbolfontsize))
      symbolfontsize <- paste0("symbolfontsize=", symbolfontsize)
    if (!is.null(bgcolor))
      bgcolor <- paste0("bgcolor=", bgcolor)
    if (!is.null(antialiasing))
      antialiasing <- paste0("antialiasing=", as.numeric(antialiasing))
    if (!is.null(atomcolor))
      atomcolor <- paste0("atomcolor=", atomcolor)
    if (!is.null(bondcolor))
      bondcolor <- paste0("bondcolor=", bondcolor)
    if (!is.null(csymbol))
      csymbol <- paste0("csymbol=", csymbol)
    if (!is.null(hsymbol))
      hsymbol <- paste0("hsymbol=", hsymbol)
    if (!is.null(hcolor))
      hcolor <- paste0("hcolor=", hcolor)
    if (!is.null(header))
      header <- paste0("header=\"", header, "\"")
    if (!is.null(footer))
      footer <- paste0("footer=\"", footer, "\"")
    if (!is.null(frame))
      frame <- paste0("frame=", frame)
    opts <- c(format,
              width,
              height,
              linewidth,
              symbolfontsize,
              bgcolor,
              antialiasing,
              atomcolor,
              bondcolor,
              csymbol,
              hsymbol,
              hcolor,
              header,
              footer,
              frame)
    opts <- paste0(opts, collapse = "&")
    opts <- paste0("?", opts)
    # url
    qurl <- URLencode(paste0(qurl, opts))
    path <- file.path(dir, paste0(query, ".", sub("format=", "", format)))
    # query
    webchem_sleep(type = 'API')
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    res <- try(httr::RETRY("GET",
                           qurl,
                           quiet = TRUE,
                           terminate_on = 404,
                           httr::write_disk(path, overwrite = TRUE),
                           httr::user_agent(webchem_url())), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    if (httr::http_error(res) && file.exists(path)) {
      file.remove(path)
    } else {
      if (verbose) message(" Image saved under: ", path)
    }
  }
  for (i in query) {
    foo(query = i,
        dir = dir,
        format = format,
        width = width,
        height = height,
        linewidth = linewidth,
        symbolfontsize = symbolfontsize,
        bgcolor = bgcolor,
        antialiasing = antialiasing,
        atomcolor = atomcolor,
        bondcolor = bondcolor,
        csymbol = csymbol,
        hsymbol = hsymbol,
        hcolor = hcolor,
        header = header,
        footer = footer,
        frame = frame,
        verbose = verbose)
  }
}
