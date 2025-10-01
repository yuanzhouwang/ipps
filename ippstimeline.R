library(dplyr)
library(ggplot2)
library(xml2)
library(rvest)
library(httr2)
library(chromote)
library(stringi)
library(stringr)
library(readr)
library(purrr)

df <- read.csv("br_keyword_ipps.csv")

ua_safari_mac <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15"
ua_chrome_mac <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.5938.132 Safari/537.36"
ua_opera_mac <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.5938.132 Safari/537.36 OPR/103.0.0.0"

ua_chrome_win <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.5938.132 Safari/537.36"
ua_opera_win <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.5938.132 Safari/537.36 OPR/103.0.0.0"

# ---------------------------
# 0) Setup Chromote with browser of choice
# ---------------------------

# Set filepath to Opera
# Sys.setenv(CHROMOTE_CHROME = "C:\\Users\\Andrew\\AppData\\Local\\Programs\\Opera\\opera.exe")
# Set filepath to Safari
Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")


# ---------------------------
# 1) Scrape URL list for article bodies
# ---------------------------

library(chromote)
library(xml2)
library(rvest)

scrape_brecorder_mac_safari <- function(urls, timeout_sec = 15) {
  stopifnot(is.character(urls), length(urls) >= 1)
  if (length(urls) == 1) urls <- as.character(urls)
  
  required_pkgs <- c("RSelenium", "xml2", "rvest")
  to_install <- setdiff(required_pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  
  # Start/ensure safaridriver on port 4444
  # If it's already running, this is a no-op
  sf_proc <- NULL
  try({
    # Try to connect first; if fails, launch safaridriver
    con_test <- try(socketConnection("127.0.0.1", 4444, open = "r+", blocking = TRUE, timeout = 1), silent = TRUE)
    if (inherits(con_test, "try-error")) {
      sf_proc <- sys::exec_background("safaridriver", c("-p", "4444"))
      Sys.sleep(0.6)  # give it a moment
    } else close(con_test)
  }, silent = TRUE)
  
  # Connect RSelenium to local Safari
  rD <- RSelenium::remoteDriver(
    remoteServerAddr = "127.0.0.1",
    port = 4444,
    browserName = "safari"
  )
  rD$open(silent = TRUE)
  on.exit({
    try(rD$close(), silent = TRUE)
    if (!is.null(sf_proc)) try(tools::pskill(sf_proc), silent = TRUE)
  }, add = TRUE)
  
  # Helper: wait until the target selector appears (or timeout)
  wait_for_selector <- function(css, timeout = timeout_sec, poll = 0.25) {
    t0 <- Sys.time()
    repeat {
      present <- try({
        elems <- rD$findElements("css selector", css)
        length(elems) > 0
      }, silent = TRUE)
      if (!inherits(present, "try-error") && isTRUE(present)) return(TRUE)
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) return(FALSE)
      Sys.sleep(poll)
    }
  }
  
  scrape_one <- function(u) {
    # Navigate
    rD$navigate(u)
    ok <- wait_for_selector("div.story__content.overflow-hidden.pt-2.mt-2 p", timeout_sec)
    if (!ok) return(NA_character_)
    
    # Get page source and parse
    html <- rD$getPageSource()[[1]]
    h <- xml2::read_html(html)
    
    paras_sel <- "div.story__content.overflow-hidden.pt-2.mt-2 p"
    paragraphs <- rvest::html_elements(h, paras_sel)
    if (length(paragraphs) == 0) return(character())
    
    p_texts <- vapply(paragraphs, function(p) {
      nodes <- xml2::xml_find_all(p, ".//text()[not(ancestor::a)]")
      txt <- paste0(xml2::xml_text(nodes), collapse = "")
      txt <- gsub("[\\h\\v]+", " ", txt, perl = TRUE)
      trimws(txt)
    }, FUN.VALUE = character(1))
    
    p_texts <- p_texts[nzchar(p_texts)]
    if (!length(p_texts)) return(character())
    paste(p_texts, collapse = "\n\n")
  }
  
  vapply(urls, scrape_one, FUN.VALUE = character(1))
}


# ---------------------------
# 3) Core detection
# ---------------------------
# Works with vectors OR data frames. For data frames, it will look for canonical name
# columns and an "aliases" column. In our CSVs, aliases are pipe-separated ("|").
detect_ipps_groups_all <- function(text, ipps, groups, url = NA_character_) {
  # --- helpers ---
  trim_nz <- function(x) { x <- trimws(as.character(x)); x[nzchar(x)] }
  escape_lit <- function(x) stringr::str_replace_all(x, "([\\.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1")
  make_pattern <- function(x) {
    # Case-insensitive, token-safe (no letters/digits directly adjacent)
    lit <- escape_lit(x)
    paste0("(?i)(?<![A-Za-z0-9])", lit, "(?![A-Za-z0-9])")
  }
  
  # --- normalize IPPs input (vector OR data frame; optional aliases col) ---
  if (is.data.frame(ipps)) {
    # Accept common name column variants; default to first column if none match
    ipp_col <- intersect(c("IPP","ipp","Name","name","Company","company","ipp_name"), names(ipps))
    if (length(ipp_col) == 0) ipp_col <- names(ipps)[1]
    ipp_names <- trim_nz(ipps[[ipp_col[1]]])
    
    alias_tbl <- NULL
    if ("aliases" %in% names(ipps)) {
      alias_vec <- ipps$aliases; alias_vec[is.na(alias_vec)] <- ""
      # Our helper CSVs use pipe ("|") separators
      alias_list <- strsplit(alias_vec, "\\s*\\|\\s*")
      alias_tbl <- do.call(rbind, lapply(seq_along(ipp_names), function(i) {
        al <- trim_nz(alias_list[[i]])
        if (!length(al)) return(NULL)
        data.frame(entity = ipp_names[i], alias = al, stringsAsFactors = FALSE)
      }))
    }
  } else {
    ipp_names <- trim_nz(ipps)
    alias_tbl <- NULL
  }
  
  ipp_tbl <- data.frame(
    type    = "IPP",
    name    = ipp_names,
    pattern = vapply(ipp_names, make_pattern, FUN.VALUE = character(1)),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(alias_tbl) && nrow(alias_tbl)) {
    ipp_alias_tbl <- data.frame(
      type    = "IPP",
      name    = alias_tbl$entity,   # canonical entity
      pattern = vapply(alias_tbl$alias, make_pattern, FUN.VALUE = character(1)),
      stringsAsFactors = FALSE
    )
    ipp_tbl <- dplyr::bind_rows(ipp_tbl, ipp_alias_tbl)
  }
  
  # --- normalize Groups input (vector OR data frame) ---
  if (is.data.frame(groups)) {
    grp_col <- intersect(c("Group","group","Name","name"), names(groups))
    if (length(grp_col) == 0) grp_col <- names(groups)[1]
    group_names <- trim_nz(groups[[grp_col[1]]])
    
    grp_alias_tbl <- NULL
    if ("aliases" %in% names(groups)) {
      alias_vec <- groups$aliases; alias_vec[is.na(alias_vec)] <- ""
      alias_list <- strsplit(alias_vec, "\\s*\\|\\s*")  # pipe-separated
      grp_alias_tbl <- do.call(rbind, lapply(seq_along(group_names), function(i) {
        al <- trim_nz(alias_list[[i]])
        if (!length(al)) return(NULL)
        data.frame(entity = group_names[i], alias = al, stringsAsFactors = FALSE)
      }))
    }
  } else {
    group_names  <- trim_nz(groups)
    grp_alias_tbl <- NULL
  }
  
  grp_tbl <- data.frame(
    type    = "Group",
    name    = group_names,
    pattern = vapply(group_names, make_pattern, FUN.VALUE = character(1)),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(grp_alias_tbl) && nrow(grp_alias_tbl)) {
    grp_aliases <- data.frame(
      type    = "Group",
      name    = grp_alias_tbl$entity,  # canonical entity
      pattern = vapply(grp_alias_tbl$alias, make_pattern, FUN.VALUE = character(1)),
      stringsAsFactors = FALSE
    )
    grp_tbl <- dplyr::bind_rows(grp_tbl, grp_aliases)
  }
  
  # --- combined dictionary (longest names first) ---
  dict_tbl <- dplyr::bind_rows(ipp_tbl, grp_tbl) %>%
    distinct(type, name, pattern) %>%
    arrange(desc(nchar(name)))
  
  # --- split text into sentences ---
  sentences <- stringi::stri_split_boundaries(text, type = "sentence")[[1]]
  sentences <- trim_nz(sentences)
  if (!length(sentences)) {
    return(data.frame(
      url = character(), type = character(), entity = character(),
      sentence = character(), stringsAsFactors = FALSE
    ))
  }
  
  # --- scan every sentence; keep every hit (no deduping = exhaustive) ---
  hits_list <- vector("list", length(sentences))
  for (i in seq_along(sentences)) {
    s <- sentences[i]
    m <- dict_tbl %>%
      filter(stringr::str_detect(s, pattern)) %>%
      transmute(url = url, type, entity = name, sentence = s)
    if (nrow(m)) hits_list[[i]] <- m
  }
  
  out <- dplyr::bind_rows(hits_list)
  if (!nrow(out)) {
    return(data.frame(
      url = character(), type = character(), entity = character(),
      sentence = character(), stringsAsFactors = FALSE
    ))
  }
  
  as.data.frame(out, stringsAsFactors = FALSE)
}

# -----------------------------------------
# 4) Convenience: scan a whole articles table
# -----------------------------------------
# articles_df: a data frame of articles with at least a text column (text_col)
#              and optionally a URL/id column (url_col). If url_col is missing,
#              the row number will be used.
# ipps_csv, groups_csv: paths to your helper CSVs from earlier steps.
scan_articles_for_ipps_groups <- function(articles_df,
                                          text_col = "text",
                                          url_col  = NULL,
                                          ipps_csv,
                                          groups_csv) {
  helpers <- load_ipps_groups_helpers(ipps_csv, groups_csv)
  ipps_df <- helpers$ipps
  grps_df <- helpers$groups
  
  stopifnot(text_col %in% names(articles_df))
  use_url <- if (is.null(url_col) || !(url_col %in% names(articles_df))) FALSE else TRUE
  
  results <- purrr::imap_dfr(articles_df[[text_col]], function(txt, idx) {
    if (!is.character(txt) || !nzchar(txt)) return(NULL)
    this_url <- if (use_url) articles_df[[url_col]][idx] else paste0("article_", idx)
    detect_ipps_groups_all(text = txt, ipps = ipps_df, groups = grps_df, url = this_url)
  })
  
  # Order nicely
  if (nrow(results)) {
    results <- results %>% select(url, type, entity, sentence)
  }
  results
}



# ---------------------------
# Test: Article Processing
# ---------------------------

ipps_csv   <- "ipplist_with_aliases_newsbacked_full.csv"
groups_csv <- "pakistan_ipp_groups_with_aliases.csv"

article_text <- lapply(df$link, render_then_scrape, ua_chosen = ua_safari_mac, collapse = TRUE, timeout_sec = 15)

article_text <- c()
for (i in 1:length(df$link)) {
  # article_text <- c(article_text, render_then_scrape(df$link[i], ua_chosen = ua_safari_mac, collapse = TRUE, timeout_sec = 12))
  print(i)
}

df$article_text <- scrape_brecorder_mac_safari(urls = df$link, timeout_sec = runif(1,1.2,7.2))

scan_articles_for_ipps_groups(articles_df = data.frame(url = "https://www.brecorder.com/news/40372487/circular-debt-debacle-financial-fixes-cannot-solve-the-problem
", text = ), text_col = "text", url_col = "url", ipps_csv = ipps_csv, groups_csv = groups_csv)

articles <- data.frame(
  url  = c("https://example.com/a1", "https://example.com/a2"),
  text = c(
    "HUBCO’s Thar Energy Limited (TEL) synchronized Unit 3 under CPEC. Meanwhile, Port Qasim Electric Power Company faced maintenance.",
    "Wind additions in the Gharo–Jhimpir Wind Corridor continue, including projects like FWEL I and FWEL II by Fauji Foundation."
  ),
  stringsAsFactors = FALSE
)

hits <- scan_articles_for_ipps_groups(
  articles_df = articles,
  text_col    = "text",
  url_col     = "url",
  ipps_csv    = ipps_csv,
  groups_csv  = groups_csv
)

print(hits)
#> Each row = (article URL, whether IPP or Group, canonical entity, surrounding sentence)
