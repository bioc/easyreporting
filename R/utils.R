
.parseAuthors <- function(author)
{
    # gets a list of person and returns a formatted string 
    authors.list <- lapply(author, function(a)
    {
        if(all(is.null(a$given), is.null(a$family))) stop("Please provide the Authors given and family name!")
        a.str <- "\n      - name: \""
        a.str <- paste(a.str, a$given, sep=" ")
        a.str <- paste(a.str, a$family,  sep=" ")
        ifelse(!is.null(a$email), a.str <- paste(a.str, a$email, "\"\n", sep=" " ), a.str <- paste0(a.str, "\"\n"))
        fields <- c(ORCID="orcid_id", affiliation="affiliation",
                    affiliation_url="affiliation_url", url="url")
        for (field in names(fields)) {
            value <- a$comment[field]
            if (length(value) == 1L && !is.na(value) && nzchar(value)) {
                a.str <- paste0(a.str, "        ", fields[[field]], ": ",
                                value, "\n")
            }
        }
        return(a.str)
    })
    return(paste(authors.list, collapse=""))
}

#' @importFrom methods is
.parseCode <- function(code)
{
    if ( is(code, "list") )
    {
        strlist <- lapply(code, function(c) deparse(c))
        str <- paste(unlist(strlist), collapse="\n")
    } else if ( is(code, "<-") ) { # not working?
        str <- deparse(code)
    } else {
        str <- code
    }
    return(str)
}

.parseResources <- function( df )
{
    str <- paste0("|Source|Reference|Description|\n|:--|:--|:--|\n",
    paste("|",apply(X=df, MARGIN=1, function(row) 
    {
        paste(row, collapse="|")
    }), collapse="|\n"),"|\n")
    return(str)
}
