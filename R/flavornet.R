# Retrieve flavor percepts from www.flavornet.org
# 
# Retreive flavor percepts from www.flavornet.org.  Flavornet is a database of 738 compounds with odors 
# perceptible to humans detected using gas chromatography ofactometry (GCO).

fn_percept <- function(CAS, verbose = TRUE, ...)
{
  foo <- function (CAS, verbose){
    qurl = paste0("http://www.flavornet.org/info/",CAS,".html")
    if (verbose)
      message(qurl)
    Sys.sleep(rgamma(1, shape = 10, scale = 1/10))
    h <- try(read_html(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning("CAS not found... Returning NA.")
      return(NA)
    }
    doc.text = xml_text(xml_find_all(h, "/html/body/p[3]"))
    pattern = "Percepts: "
    percept <- gsub(pattern, "", doc.text)
    return(percept)
  }
  percepts <- sapply(CAS, foo, verbose = verbose)
  percepts <- setNames(percepts, CAS)
  return(percepts)
}