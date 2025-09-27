library(dplyr)
library(ggplot2)
library(xml2)
library(httr2)
library(chromote)
library(stringi)
library(stringr)


df <- read.csv("br_keyword_ipps.csv")

# install.packages(c("rvest", "xml2", "httr"))  # if needed

# install.packages(c("httr2", "xml2", "rvest"))  # if needed

# install.packages(c("httr2", "xml2", "rvest"))  # if needed

# install.packages(c("chromote", "xml2", "rvest"))
# install.packages(c("chromote", "xml2", "rvest"))  # if needed

render_then_scrape <- function(url, collapse = TRUE, timeout_sec = 12) {
  stopifnot(is.character(url), length(url) == 1)
  
  library(chromote)
  library(xml2)
  library(rvest)
  
  # Create a browser session
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  
  # Optional: look more like Safari (some sites care about UA)
  ua_safari <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15"
  b$Network$enable()
  b$Network$setUserAgentOverride(userAgent = ua_safari)
  b$Page$enable()
  
  # Navigate and wait for load event
  b$Page$navigate(url)
  b$Page$loadEventFired()
  
  # Helper: wait until the target selector appears (or timeout)
  wait_for_selector <- function(css, timeout_sec = 10, poll = 0.25) {
    t0 <- Sys.time()
    repeat {
      doc <- b$DOM$getDocument()
      q <- b$DOM$querySelectorAll(nodeId = doc$root$nodeId, selector = css)
      if (length(q$nodeIds) > 0) return(invisible(TRUE))
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_sec) break
      Sys.sleep(poll)
    }
    invisible(FALSE)
  }
  
  container_sel <- "div.story__content.overflow-hidden.pt-2.mt-2"
  paras_sel <- paste0(container_sel, " p")
  wait_for_selector(paras_sel, timeout_sec = timeout_sec)
  
  # Grab the full outer HTML of the document
  doc <- b$DOM$getDocument()
  html <- b$DOM$getOuterHTML(nodeId = doc$root$nodeId)$outerHTML
  if (!nzchar(html)) {
    warning("Got empty HTML from the rendered page.")
    return(NA_character_)
  }
  
  # Parse and extract paragraphs, excluding link text within <a> tags
  h <- read_html(html)
  paragraphs <- html_elements(h, paras_sel)
  if (length(paragraphs) == 0) {
    warning("No matching paragraphs found under the specified container.")
    return(character())
  }
  
  p_texts <- vapply(paragraphs, function(p) {
    nodes <- xml_find_all(p, ".//text()[not(ancestor::a)]")
    txt <- paste0(xml_text(nodes), collapse = "")
    txt <- gsub("[\\h\\v]+", " ", txt, perl = TRUE)  # \h = horizontal ws (incl. NBSP), \v = vertical ws
    txt <- trimws(txt)
    trimws(txt)
  }, FUN.VALUE = character(1))
  
  p_texts <- p_texts[nzchar(p_texts)]
  if (!length(p_texts)) return(character())
  
  if (isTRUE(collapse)) paste(p_texts, collapse = "\n\n") else p_texts
}


# Helper function for extracting IPPs/Groups and surrounding sentence.
# ipps - vector of IPPs
# groups - vector of groups
detect_ipps_groups_all <- function(text, ipps, groups, url = NA_character_) {
  require(dplyr); require(stringr); require(stringi)
  
  # --- helpers ---
  trim_nz <- function(x) {
    x <- trimws(as.character(x)); x[nzchar(x)]
  }
  escape_lit <- function(x) stringr::str_replace_all(x, "([\\.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1")
  make_pattern <- function(x) {
    lit <- escape_lit(x)
    paste0("(?i)(?<![A-Za-z0-9])", lit, "(?![A-Za-z0-9])")  # case-insensitive, token-safe
  }
  
  # --- normalize IPPs input (vector OR data frame; optional aliases col) ---
  if (is.data.frame(ipps)) {
    ipp_col <- intersect(c("IPP","Name","name","Company","company"), names(ipps))
    if (length(ipp_col) == 0) ipp_col <- names(ipps)[1]
    ipp_names <- trim_nz(ipps[[ipp_col[1]]])
    
    alias_tbl <- NULL
    if ("aliases" %in% names(ipps)) {
      alias_vec <- ipps$aliases; alias_vec[is.na(alias_vec)] <- ""
      alias_list <- strsplit(alias_vec, "\\s*;\\s*")
      alias_tbl <- do.call(rbind, lapply(seq_along(ipp_names), function(i) {
        al <- alias_list[[i]]; al <- al[nzchar(al)]
        if (!length(al)) return(NULL)
        data.frame(entity = ipp_names[i], alias = al, stringsAsFactors = FALSE)
      }))
    }
  } else {
    ipp_names <- trim_nz(ipps)
    alias_tbl <- NULL
  }
  
  ipp_tbl <- data.frame(
    type = "IPP",
    name = ipp_names,
    pattern = vapply(ipp_names, make_pattern, FUN.VALUE = character(1)),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(alias_tbl) && nrow(alias_tbl)) {
    ipp_alias_tbl <- data.frame(
      type = "IPP",
      name = alias_tbl$entity,
      pattern = vapply(alias_tbl$alias, make_pattern, FUN.VALUE = character(1)),
      stringsAsFactors = FALSE
    )
    ipp_tbl <- bind_rows(ipp_tbl, ipp_alias_tbl)
  }
  
  # --- normalize Groups input (vector OR data frame) ---
  if (is.data.frame(groups)) {
    grp_col <- intersect(c("Group","group","Name","name"), names(groups))
    if (length(grp_col) == 0) grp_col <- names(groups)[1]
    group_names <- trim_nz(groups[[grp_col[1]]])
  } else {
    group_names <- trim_nz(groups)
  }
  
  grp_tbl <- data.frame(
    type = "Group",
    name = group_names,
    pattern = vapply(group_names, make_pattern, FUN.VALUE = character(1)),
    stringsAsFactors = FALSE
  )
  
  # --- combined dictionary (longest names first) ---
  dict_tbl <- bind_rows(ipp_tbl, grp_tbl) %>%
    distinct(type, name, pattern) %>%
    arrange(desc(nchar(name)))
  
  # --- split text into sentences ---
  sentences <- stringi::stri_split_boundaries(text, type = "sentence")[[1]]
  sentences <- trim_nz(sentences)
  if (!length(sentences)) {
    return(data.frame(url = character(), type = character(), entity = character(),
                      sentence = character(), stringsAsFactors = FALSE))
  }
  
  # --- scan every sentence; keep every hit (no deduping) ---
  hits_list <- vector("list", length(sentences))
  for (i in seq_along(sentences)) {
    s <- sentences[i]
    m <- dict_tbl %>%
      filter(stringr::str_detect(s, pattern)) %>%
      transmute(url = url, type, entity = name, sentence = s)
    if (nrow(m)) hits_list[[i]] <- m
  }
  
  out <- bind_rows(hits_list)
  if (!nrow(out)) {
    return(data.frame(url = character(), type = character(), entity = character(),
                      sentence = character(), stringsAsFactors = FALSE))
  }
  
  as.data.frame(out, stringsAsFactors = FALSE)
}

####### How to check various versions of IPP/Group names? Maybe ask ChatGPT.ß
ipps <- read.csv("ipplist.csv")
groups <- read.csv("pakistan_ipp_groups.csv")

