#' @title Add Title Elements
#' @param parament_element dataset or project 
#' @param abstract Abstract paragraphs, minimum of 20 words.
#' @details 
#' For	a	dataset,	the	abstract	element	can	appear	at	the	resource	level	or	the	project level.		
#' The	<abstract>	element	will	be	used	for	full-text	searches,	and	it	should	be	rich	with	
#' descriptive	text.	In	particular,	descriptions	should	include	information	that	does	not	fit	into	
#' structured	metadata,	and	focus	on	the	“what”,	“when”,	and	“where”	information,	general	
#' taxonomic	information,	as	well	as	whether	the	dataset	is	ongoing	or	completed.	Some	
#' general	methods	description	is	appropriate,	and	broad	classes	of	measured	parameters	
#' should	also	be	included.		For	a	large	number	of	parameters,	use	categories	instead	of	listing	
#' all	parameters	(e.g.	use	the	term	“nutrients”	instead	of	nitrate,	phosphate,	calcium,	etc.),	in	
#' combination	with	the	parameters	that	seem	most	relevant	for	searches.
#' @export 
add_abstract <- function(parent_element, abstract) {
  
  if (length(unlist(strsplit(abstract, " "))) < 20) {
    warning("Abstract should be a minimum of 20 words")
  }
  
  parent_element$abstract$para <- abstract
  
  return(parent_element)
}

