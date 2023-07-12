library('httr')
library('rvest')
library('parallel')

verify_response <- function(response){
  if (status_code(response) == 200){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Send request and handle retries.
#'
#' @param url 
#'
#' @return response. Response we received after sending request to the URL.
#'
#' @examples
#' response <- send_request("https://www.example.com")
send_request <- function(url) {
  headers <- c(
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36",
    "Accept-Language" = "en-US,en;q=0.5"
  )
  max_retry <- 3
  while (max_retry >= 1){
    response <- httr::GET(url, add_headers(headers))
    if (verify_response(response)) {
      return(response)
    } else {
      max_retry <- max_retry - 1
    }
  } 
print("Invalid response received even after retrying. URL with the issue is:")
  print(url)
  stop("Stopping the code execution as invalid response received.")
}

#' Collect pagination URL.
#'
#' @param response
#'
#' @return url. Next listing page URL will be returned.
#'
#' @examples
#' next_page_url <- get_next_page_url(response)
get_next_page_url <- function(response){
  parser <- read_html(response)
  next_page_url <- html_nodes(parser, xpath='(//a[@class="next page-numbers"])[1]') %>% html_attr('href')
  return(next_page_url)
}

#' Collects all product URL from a listing page response.
#'
#' @param response
#'
#' @return list of urls. List of product page urls returned.
#'
#' @examples
#' product_urls <- get_product_urls(response)
get_product_urls <- function(response) {
  parser <- read_html(response)
  product_urls <- html_nodes(
    parser, xpath='//li/a[contains(@class, "product__link")]') %>% html_attr(
      'href')
  return(product_urls)
}

#' Clean the data stock.
#'
#' @param stock
#'
#' @return Stock data. Stock number will be returned by removing extra string.
#'
#' @examples
#' cleaned_stock <- clean_stock(stock)
clean_stock <- function(stock) {
  if (!is.null(stock)) {
    stock <- sub(" in stock", "", stock)
    return(stock)
  } else {
    return(NULL)
  }
}

#' Collect all details of a product.
#'
#' @param response
#'
#' @return All data of a product.
#'
#' @examples
#' product_data <- get_product_data(response)
get_product_data <- function(url) {
  response <- send_request(url)
  parser <- read_html(response)
  title <- html_nodes(parser, xpath='//h1[contains(@class, "product_title")]') %>% html_text2()
  price <- html_nodes(parser, xpath='//p[@class="price"]') %>% html_text2()
  stock <- html_nodes(parser, xpath='//p[contains(@class, "in-stock")]') %>% html_text2()
  description <- html_nodes(parser, xpath='//div[contains(@class,"product-details__short-description")]') %>% html_text2()
  image_url <- html_nodes(parser, xpath='//div[contains(@class, "woocommerce-product-gallery__image")]/a') %>% html_attr('href')
  stock <- clean_stock(stock)
  product_data <- data.frame(Title=title, Price=price, Stock=stock, Description=description, Image_URL=image_url, Product_URL=url)
  return(product_data)
}

#' Starting function.
#'
#' @examples
#' start_scraping()
start_scraping <- function(){
  listing_page_number = 1
  listing_page_url = 'https://scrapeme.live/shop/'
  product_urls = list()
  for (listing_page_number in 1:5) {
    response <- send_request(listing_page_url)
    listing_page_url <- get_next_page_url(response)
    products_from_current_page <- get_product_urls(response)
    product_urls < append(product_urls, products_from_current_page)
  }
  product_urls <- unlist(product_urls)
  results <- mclapply(product_urls, get_product_data, mc.cores = 8)
  results <- do.call(rbind, results)
  write.csv(results, file = "scrapeme_live_R_data.csv", row.names = FALSE)
  print('Data saved as csv')
}
start_scraping()
