# ============================================================
# utils_utf8.R — UTF-8 cleanup helper functions
# ============================================================

clean_utf8 <- function(x){
  x <- iconv(x, from = "", to = "UTF-8", sub = "")      # clean invalid bytes
  x <- str_replace_all(x, "<", "&lt;")                  # protect HTML
  x <- str_replace_all(x, ">", "&gt;")
  return(x)
}
