library(easyreporting)

# Exercise parsing even when distill is unavailable on the test machine.
parseAuthors <- getFromNamespace(".parseAuthors", "easyreporting")
plain <- utils::person("Test", "Author")
plainText <- parseAuthors(plain)
stopifnot(
    grepl("Test Author", plainText, fixed = TRUE),
    !grepl("orcid_id:", plainText, fixed = TRUE)
)

author <- utils::person("Test", "Author", comment = c(
    ORCID = "0000-0002-1825-0097",
    affiliation = "Example University",
    affiliation_url = "https://example.org",
    url = "https://example.org/author"
))
text <- parseAuthors(author)
stopifnot(
    grepl("orcid_id: 0000-0002-1825-0097", text, fixed = TRUE),
    grepl("affiliation: Example University", text, fixed = TRUE),
    grepl("affiliation_url: https://example.org", text, fixed = TRUE),
    grepl("url: https://example.org/author", text, fixed = TRUE)
)

author <- utils::person("Test", "Author", comment = c(
    ORCID = NA_character_, affiliation = ""
))
stopifnot(identical(parseAuthors(author), plainText))
stopifnot(identical(parseAuthors(c(plain, plain)), paste0(plainText, plainText)))
