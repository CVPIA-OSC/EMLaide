#' @title Create Abstract Element
#' @description Creates the abstract of a dataset according to EML standards.
#' @param abstract path to file (.docx or .md) containing project abstract, requires a minimum of 20 words. 
#' @details 
#' For	a	dataset,	the	abstract	element	can	appear	at	the	resource	level	or	the	project level.		
#' The	\code{abstract}	element	will	be	used	for	full-text	searches,	and	it	should	be	rich	with	
#' descriptive	text.	In	particular,	descriptions	should	include	information	that	does	not	fit	into	
#' structured	metadata,	and	focus	on	the	"what",	"when",	and	"where"	information,	general	
#' taxonomic	information,	as	well	as	whether	the	dataset	is	ongoing	or	completed.	Some	
#' general	methods	description	is	appropriate,	and	broad	classes	of	measured	parameters	
#' should	also	be	included.		For	a	large	number	of	parameters,	use	categories	instead	of	listing	
#' all	parameters	(e.g.	use	the	term	"nutrients"	instead	of	nitrate,	phosphate,	calcium,	etc.),	in	
#' combination	with	the	parameters	that	seem	most	relevant	for	searches.
#' @return An abstract element formatted according to EML standards
#' @examples
#' create_abstract(abstract = word_example("abstract_template.docx"))
#'           
#' @export 
create_abstract <- function(abstract) {
  
  abstract <- EML::set_TextType(abstract)
  
  return(abstract)
}

#' Add Abstract
#' @description Adds the abstract metadata element to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param abstract_file file path to abstract (word or markdown): see \code{\link{create_abstract}} 
#' @return The dataset or project with abstract appended 
#' @examples
#' abstract_docx <- system.file("extdata", "Banet-Example", "metadata","abstract.docx", package = "EMLaide", mustWork = TRUE)  
#' 
#' dataset <- list() %>%
#'     add_abstract(abstract_docx)
#' dataset
#' @export
add_abstract <- function(parent_element, abstract_file) {
  
  parent_element$abstract <- create_abstract(abstract_file)
  
  return(parent_element)
}