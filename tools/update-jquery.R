version <- "3.6.0"

# Download jQuery from source
jq_cdn_download <- function(version) {
    Map(
        filetype = ".min.js",
        f = function(filetype) {
            download.file(
                file.path("https://code.jquery.com", paste0("jquery-", version, filetype)),
                file.path("inst", "includes", "javascripts",  paste0("jquery", filetype))
            )
        }
    )
}

jq_cdn_download(version)

# Download AUTHORS.txt
download.file(
    "https://raw.githubusercontent.com/jquery/jquery/master/AUTHORS.txt",
    "inst/includes/javascripts/jquery-AUTHORS.txt"
)
