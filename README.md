# Scalable and Modular News Article Scraping Pipeline in R

## 1. Project Overview

This repository contains a comprehensive, multi-phase web scraping pipeline developed in **R**.  
Its primary objective is to systematically identify, collect, and parse news articles from a wide range of **German-language media domains**.  

The entire workflow is designed with **modularity**, **reproducibility**, and **scalability** in mind, making it suitable for **academic research** in the social sciences and other data-intensive fields.
Note: A cleaned version of this repo with a new file structure and better documentation will follow soon. 



### Pipeline Phases

1. **Sitemap Discovery:** Automatically crawling domains to find `robots.txt` files and sitemap URLs.  
2. **URL Collection:** Recursively parsing sitemaps to gather all individual article URLs within a specified date range, enriched with data from the Wayback Machine.  
3. **Modular Article Scraping:** A resilient, state-aware system for scraping full article content while handling anti-scraping measures.

---

## 2. Phase 1: Sitemap Discovery

**Goal:** Build a clean, comprehensive list of sitemap URLs from a predefined set of news media domains.  
**Key Script:** `sitemap_scraper.Rmd`

### Workflow

- **Domain Input:** Fetches a curated list of German news domains from the *paperboy* GitHub repository.  
- **robots.txt Retrieval:** Attempts to locate each domain’s `robots.txt` file by testing protocol permutations (`http://`, `https://`, `https://www.`).  
- **Sitemap Extraction:** Parses the `robots.txt` content with regex to extract all URLs declared with the `Sitemap:` directive.  
- **Heuristic Search:** For domains without declared sitemaps, tests common paths (e.g. `/sitemap.xml`, `/sitemap_index.xml`).  
- **Filtering and Cleaning:** Filters out irrelevant sitemaps (e.g. image or video sitemaps) through keyword and manual exclusion.  

**Output:**  
`/data/sitemap_scraper_clean_sitemaps.csv` containing valid, article-relevant sitemap URLs for **Phase 2**.

---

## 3. Phase 2: Sitemap Parsing & URL Collection

**Goal:** Traverse all discovered sitemaps, parse their contents, and extract article URLs within a given date range.  
**Key Scripts:** `urlset_scraper.Rmd`, `urlset_scraper_post_processing.Rmd`

### Workflow

- **Recursive Parsing:**  
  - Reads the sitemap list and processes each URL recursively.  
  - `<sitemapindex>` files → Extract nested sitemap URLs.  
  - `<urlset>` files → Extract `<loc>` tags and metadata.  

- **Temporal Filtering:**  
  - **URL-based Heuristics:** Detects date patterns in URLs (e.g. `/2025/01/...`).  
  - **Metadata Parsing:** Extracts `<lastmod>` or `<publication_date>` for filtering.  

- **Wayback Machine Integration:**  
  - Uses the Internet Archive CDX API to recover missing or out-of-range sitemaps.  

- **Stateful Logging:**  
  - Logs every request, response, and error.  
  - Enables pausing/resuming and full traceability.  

**Output:**  
`input.rds` containing a **unique list of all article URLs** to be scraped in **Phase 3**.

---

## 4. Phase 3: Modular Article Scraping

**Goal:** Scrape the full HTML content and parse structured data (headline, author, date, text) for each article URL.  
**Orchestrator Script:** `run_scraper.Rmd`

### Workflow

1. **Chunking:**  
   - `02_chunk_manager.R` splits `input.rds` into balanced chunks.  
   - Uses stratified sampling and randomization to reduce request clustering per domain.  

2. **State-Driven Orchestration:**  
   - `04_request_orchestrator.R` manages request logic based on domain health.  
   - Consults `08_domain_state_manager.R` for decision-making.  

3. **Execution & Analysis:**  
   - `05_request_executor.R` sends HTTP requests.  
   - `07_response_analyzer.R` classifies responses (valid, blocked, or error).  

4. **Parsing:**  
   - `06_html_parser.R` applies domain-specific parsing rules (from the *paperboy* package).  

5. **Storage & Logging:**  
   - Managed by `09_storage_manager.R` and `10_log_manager.R`.

---

## Core Modules Explained

### 01_init.R – Initialization & Configuration
**Purpose:** Sets up the scraping environment.  
**Tasks:**
- Creates necessary data structures (`final_data.rds`, `retry.rds`, `parse_error.rds`).  
- Initializes log files and validates schemas against a predefined codebook.

---

### 02_chunk_manager.R – Chunk Manager
**Purpose:** Divide workload into balanced, randomized chunks.  
**Tasks:**
- Implements stratified sampling to balance domains per chunk.  
- Randomizes order to increase delay between requests to the same domain.

---

### 03_identity_manager.R – Identity Manager
**Purpose:** Manage scraper identity for realistic browser behavior.  
**Tasks:**
- Manages sessions (unique user-agent, headers, cookie jar).  
- Rotates through ~100 realistic user agents.  
- Logs VPN IP per run for traceability.

---

### 04_request_orchestrator.R – Request Orchestrator
**Purpose:** Central decision-making engine.  
**Tasks:**
- Checks domain state from `08_domain_state_manager.R`.  
- Applies logic:
  - **PROCEED:** Healthy → send request.  
  - **ROTATE:** Error threshold met → new session.  
  - **BLOCK:** All sessions failed → domain blocked for current run.

---

### 05_request_executor.R – Request Executor
**Purpose:** Execute HTTP requests.  
**Tasks:**
- Uses `httr2` to send requests with headers, cookies, and timeouts.  
- Captures full response (status, headers, body) and errors.

---

### 06_html_parser.R – HTML Parser & Rule Manager
**Purpose:** Extract structured data from raw HTML and detect non-article pages.  
**Tasks:**
- **Parser Rule Setup:** Uses *paperboy* repo to build CSS/XPath selectors per domain.  
- **Paywall Detection:** Identifies paywalled vs. free articles.  
- **Bot Detection:** Recognizes Cloudflare, reCAPTCHA, and similar pages.  
- **Core Function:** `func_06_parse_html()` extracts headline, author, date, text, and flags anomalies.

---

### 07_response_analyzer.R – Response Analyzer
**Purpose:** Classify HTTP responses and determine retry logic.  
**Tasks:**
- **200 OK:** Parse HTML; detect bot-checks.  
- **4xx/5xx Errors:** Send to retry queue, increment domain error count.  
- **Network Errors:** Log and retry.  
- Updates the domain state manager with final outcomes.

---

## System Characteristics

This **modular, state-aware architecture** allows the pipeline to adapt dynamically to server behavior, increasing success rates and ensuring high-quality structured data suitable for **large-scale content analysis and academic research**.