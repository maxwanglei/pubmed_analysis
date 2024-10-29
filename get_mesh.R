# Obtain MeSH terms using PMID
# Input: PMID
# Output: MeSH terms
library(tidyverse)
library(xml2)
library(rentrez)
get_mesh_terms <- function(pmid) {
  article <- entrez_fetch(db = "pubmed", id = pmid, rettype = "xml", parsed = FALSE)
  xml_doc <- read_xml(article)
  mesh_terms <- xml2::xml_find_all(xml_doc, ".//MeshHeading/DescriptorName")
  return(xml2::xml_text(mesh_terms))
}

get_mesh_terms_detailed <- function(pmid) {
  article <- entrez_fetch(db = "pubmed", id = pmid, rettype = "xml", parsed = FALSE)
  xml_doc <- read_xml(article)

  mesh_headings <- xml_find_all(xml_doc, ".//MeshHeading")

  result <- lapply(mesh_headings, function(heading) {
    descriptor <- xml_text(xml_find_first(heading, "./DescriptorName"))
    descriptor_ui <- xml_attr(xml_find_first(heading, "./DescriptorName"), "UI")
    descriptor_major <- xml_attr(xml_find_first(heading, "./DescriptorName"), "MajorTopicYN")

    subheadings <- xml_find_all(heading, "./QualifierName")
    subheading_list <- lapply(subheadings, function(subheading) {
      list(
        name = xml_text(subheading),
        ui = xml_attr(subheading, "UI"),
        major = xml_attr(subheading, "MajorTopicYN")
      )
    })

    list(
      descriptor = list(
        name = descriptor,
        ui = descriptor_ui,
        major = descriptor_major
      ),
      subheadings = subheading_list
    )
  })

  return(result)
}
