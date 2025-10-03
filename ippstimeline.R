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

# install.packages(c("httr2","rvest","stringr","purrr","tibble"))
library(httr2)
library(rvest)
library(stringr)
library(purrr)
library(tibble)

# --- Helpers ---
brec_safari_ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Safari/605.1.15"

to_brec_amp <- function(url) {
  # Accept either .../news/<id>/<slug> or .../news/<id>
  m <- regexec("^https?://www\\.brecorder\\.com/news/([0-9]+)/?", url)
  g <- regmatches(url, m)
  if (length(g) && length(g[[1]]) >= 2) {
    id <- g[[1]][2]
    paste0("https://www.brecorder.com/news/amp/", id)
  } else {
    # If it already looks like an AMP link, keep it; otherwise return NA
    if (grepl("^https?://www\\.brecorder\\.com/news/amp/[0-9]+/?$", url)) url else NA_character_
  }
}

fetch_amp_html <- function(amp_url) {
  req <- request(amp_url) |>
    req_user_agent(brec_safari_ua) |>
    req_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.9",
      "Upgrade-Insecure-Requests" = "1",
      "DNT" = "1",
      "Referer" = "https://www.brecorder.com/"
    ) |>
    # Avoid brotli: some R setups don't have it in libcurl
    req_options(accept_encoding = "gzip, deflate") |>
    req_timeout(20) |>
    req_retry(
      max_tries = 3,
      backoff = ~ min(30, 1.6 ^ .attempt * runif(1, 0.8, 1.25)),
      is_transient = function(resp) {
        if (inherits(resp, "error")) return(TRUE)
        s <- resp_status(resp); s >= 500 || s == 429
      }
    )
  
  resp <- req_perform(req)
  list(status = resp_status(resp), html = resp_body_html(resp))
}

extract_brec_body <- function(html,
                              selectors = c(
                                "div.story__content.overflow-hidden.pt-2.mt-2 p",
                                "div.story__content p",
                                "article p"
                              )) {
  for (sel in selectors) {
    nodes <- html |> html_elements(sel)
    if (length(nodes)) {
      txt <- nodes |> html_text2() |> str_squish()
      body <- paste(txt, collapse = "\n\n")
      if (nzchar(body)) return(list(ok = TRUE, body = body, note = NA_character_))
    }
  }
  list(ok = FALSE, body = NA_character_, note = "Selector found no nodes")
}

# --- Public: single URL ---
brec_get_article_amp <- function(url,
                                 selector = "div.story__content.overflow-hidden.pt-2.mt-2 p",
                                 verbose = FALSE) {
  stopifnot(is.character(url), length(url) == 1L, !is.na(url))
  amp <- to_brec_amp(url)
  if (is.na(amp)) {
    return(list(url = url, ok = FALSE, body = NA_character_,
                note = "Not a recognized brecorder article URL"))
  }
  if (verbose) message("Fetching AMP: ", amp)
  res <- try(fetch_amp_html(amp), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(list(url = url, ok = FALSE, body = NA_character_,
                note = paste0("HTTP error: ", as.character(res))))
  }
  if (res$status == 403L) {
    return(list(url = url, ok = FALSE, body = NA_character_,
                note = "HTTP 403 on AMP (blocked by site)"))
  }
  parsed <- extract_brec_body(res$html, selectors = c(selector, "div.story__content p", "article p"))
  if (parsed$ok) return(list(url = url, ok = TRUE, body = parsed$body, note = "Parsed from AMP"))
  list(url = url, ok = FALSE, body = NA_character_, note = parsed$note)
}

# --- Public: many URLs ---
brec_scrape_articles_amp <- function(urls,
                                     selector = "div.story__content.overflow-hidden.pt-2.mt-2 p",
                                     min_pause = 0.5, max_pause = 1.5,
                                     verbose = FALSE) {
  stopifnot(is.character(urls), length(urls) >= 1L)
  urls <- unique(urls)
  results <- map(urls, function(u) {
    res <- brec_get_article_amp(u, selector = selector, verbose = verbose)
    Sys.sleep(runif(1, min_pause, max_pause))
    res
  })
  tibble(
    url   = map_chr(results, "url"),
    ok    = map_lgl(results, "ok"),
    body  = map_chr(results, "body"),
    note  = map_chr(results, "note")
  )
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

df$article_text <- scrape_brecorder_mac_safari(urls = df$link[1], timeout_sec = runif(1,1.2,7.2))

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
