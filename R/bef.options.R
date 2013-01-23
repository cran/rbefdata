#' Set or query BEFdata portal URL address
#' 
#' This function is used to query and set the BEFdata portal URL address. It is not supposed to 
#' be changed, unless you are using Beijing portal or other portals using the code from the BEFdata 
#' project.
#' 
#' @param \dots similar to \code{\link{options}}. see examples below.
#' @examples 
#' # query BEFdata portal's URL
#' bef.options('url')
#' # set URL
#' bef.options(url='http://www.example.com')
#' @export

bef.options = function(...) {
		lst = list(...)
		.bef.opts = .bef.env$.bef.opts
		if (length(lst)) {
			if (is.null(names(lst)) && !is.list(lst[[1]])) {
					lst = unlist(lst)
					if (length(lst) == 1) .bef.opts[[lst]] else .bef.opts[lst]
			}
			else {
				omf = .bef.opts
				if (is.list(lst[[1]]))
						lst = lst[[1]]
				if (length(lst) > 0) {
					.bef.opts[names(lst)] = lst
					.bef.opts["url"] = sub(.bef.opts["url"], pattern = "(/)?$", replacement = "")
					.bef.opts["url"] = sub(.bef.opts["url"], pattern = "^(http://)?", replacement = "http://")
					.bef.env$.bef.opts = .bef.opts
				}
				invisible(omf)
			}
		} 
		else {
			.bef.opts
		}
}

.bef.env = new.env()
