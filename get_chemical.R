library(rentrez)
library(xml2)

get_pubmed_substances <- function(pmid) {
  # Fetch the PubMed record
  record <- entrez_fetch(db = "pubmed", id = pmid, rettype = "xml", parsed = FALSE)
  record_xml <- read_xml(record)
  # Extract substance information
  substance_nodes <- xml_find_all(record_xml, "//Chemical")

  if (length(substance_nodes) > 0) {
    substances <- lapply(substance_nodes, function(node) {
      list(
        name = xml_text(xml_find_first(node, "./NameOfSubstance")),
        registry_number = xml_text(xml_find_first(node, "./RegistryNumber"))
      )
    })

    # Convert to a data frame
    result <- do.call(rbind, lapply(substances, data.frame, stringsAsFactors = FALSE))
    colnames(result) <- c("Substance", "RegistryNumber")

    return(result)
  } else {
    return("No substances found for this PMID")
  }
}
