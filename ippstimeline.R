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

render_then_scrape <- function(url, ua_chosen, collapse = TRUE, timeout_sec = 12) {
  stopifnot(is.character(url), length(url) == 1)
  
  # Create a browser session
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  options(chromot.headless = "new")
  
  # UA: Select MacOS, Windows ; Safari, Chrome, Opera
  
  b$Network$enable()
  b$Network$setUserAgentOverride(userAgent = ua_chosen)
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


# ---------------------------
# 2) Load helper CSVs
# ---------------------------
# ipps_csv: path to ipplist_with_aliases_newsbacked_full.csv  (has columns: ipp_code, ipp_name, aliases, ...)
# groups_csv: path to pakistan_ipp_groups_with_aliases.csv    (has columns: Group, aliases, ...)
load_ipps_groups_helpers <- function(ipps_csv, groups_csv) {
  ipps  <- readr::read_csv(ipps_csv, show_col_types = FALSE)
  grps  <- readr::read_csv(groups_csv, show_col_types = FALSE)
  
  # Normalize column names we need
  stopifnot("ipp_name" %in% names(ipps))
  stopifnot("aliases"  %in% names(ipps))
  stopifnot("Group"    %in% names(grps))
  stopifnot("aliases"  %in% names(grps))
  
  # Return only what we need for matching
  ipps_out <- ipps %>%
    transmute(IPP = ipp_name, aliases = coalesce(aliases, ""))
  
  grps_out <- grps %>%
    transmute(Group = Group, aliases = coalesce(aliases, ""))
  
  list(ipps = ipps_out, groups = grps_out)
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
  df$article_text[i] <- render_then_scrape(df$link[i], ua_chosen = ua_safari_mac, collapse = TRUE, timeout_sec = runif(1,1,11))
  print(i)
}

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
