
library(httr)
library(jsonlite)
library(shiny)
library(data.table)
library(fst)
library(plyr)
library(tidyverse)

img_base_url <- "https://www.dominos.co.uk/Content/images/Products/GB/Pizza/256x256/"


if(!exists("prod_listing")) {
  if(!file.exists("product_info/prod_listing.fst")) {
    prod_listing <- fread("product_info/Product_Data.csv") %>%
      arrange(`Product SKU ID`) %>%
      rowwise() %>%
      mutate(variant = gsub(`Product Name`, "", `Product SKU Description`)) %>%
      ungroup() %>%
      mutate(variant_adj = ifelse(nchar(variant) > 0, paste0(" (", variant, ")"), ""),
             shown = paste0(`Product SKU ID`, "-", `Product Name`, variant_adj),
             ImageURL = paste0(img_base_url, ImageURL)) 
    write.fst(prod_listing, "product_info/prod_listing.fst")
  } else {
    prod_listing <- read.fst("product_info/prod_listing.fst")
  }
}

recommender_api <- function(StoreId_val = 27536, 
                            version_val = 1,
                            productId_val = "250", 
                            productQuantity_val = 1) {
  
  json_body <- toJSON(list(StoreId = StoreId_val,
                           Version = version_val,
                           productId = as.list(productId_val),
                           productQuantity = as.list(productQuantity_val)), 
                      auto_unbox = TRUE)
  
  #"https://europe-west2-clear-destiny-741.cloudfunctions.net/prod-recommend"
  base_url <- "https://europe-west2-clear-destiny-741.cloudfunctions.net/prod-recommend-v3" 
  
  resp <- POST(base_url,
               body = json_body, 
               #encode = "raw",
               content_type_json())
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- content(resp, "text") %>% 
    #gsub(pattern = ',\\"\\d+\\":NaN', replacement = "") %>%
    gsub(pattern = 'NaN', replacement = '"NA"') %>%
    fromJSON(simplifyVector = FALSE)
  
  return(parsed)
}

# a function to fix NULL in parsed response got ignored after unlist:
replace_null2na <- function(list2fix) {
  list2fix[sapply(list2fix[[1]], is.null)] <- NA
  return(list2fix)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$sort_selection <- renderUI({
    
    radioGroupButtons(
      inputId = "sort_selected",
      label = "Sort by",
      choices = c("Most recommended", 
                  "Cheapest"),
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))#,
      #justified = TRUE
    )
  })
  
  output$store_picker <- renderUI({
    
    store_list <- prod_listing %>%
      pull(StoreID) %>%
      unique() %>%
      sort()
    
    pickerInput(
      inputId = "store_selected",
      label = "Store ID:", 
      choices = store_list,
      multiple = FALSE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE)
    )
  })
  
  output$prod_picker <- renderUI({
    
    shiny::validate(
      need(length(input$store_selected) > 0, "")
    )
    
    product_list <- prod_listing %>%
      filter(StoreID == input$store_selected) %>%
      group_by(`Product Category`, shown) %>%
      summarise(count = n())
    
    prod_category_list <- product_list$`Product Category` %>% unique()
    
    get_prod_list_per_category <- function(cat_name) {
      listing <- product_list %>% 
        filter(`Product Category` == cat_name) %>% 
        pull(shown) %>% 
        unique()
      
      if(length(listing) == 1) {
        listing <- c(listing, "")
      }
      return(listing)
    }
    
    pickerInput(
      inputId = "prod_selected",
      label = "Products in basket:", 
      choices = list(
        Starters = c(get_prod_list_per_category("Starters")),
        Pizza = c(get_prod_list_per_category("Pizza")),
        `Pizza Platters` = c(get_prod_list_per_category("Pizza Platters")),
        `Sides Platters` = c(get_prod_list_per_category("Sides Platters")),
        Subs = c(get_prod_list_per_category("Subs")),
        `Sub Platters` = c(get_prod_list_per_category("Sub Platters")),
        Desserts = c(get_prod_list_per_category("Desserts")),
        `Dessert Platters` = c(get_prod_list_per_category("Dessert Platters")),
        Drinks = c(get_prod_list_per_category("Drinks")),
        `Price Trigger Items` = c(get_prod_list_per_category("Price Trigger Items")),
        `HIDDEN PRODUCTS` = c(get_prod_list_per_category("HIDDEN PRODUCTS"))
      ),
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE)
    )
  })
  
  output$selected_store_text <- renderUI({
    
    store_selected <- input$store_selected %>%
      paste(collapse = '<br/>') %>%
      HTML()
    
    store_selected
  })
  
  output$selected_product_text <- renderUI({
    
    shiny::validate(
      need(length(input$prod_selected) > 0 & length(input$prod_selected) == lengths(strsplit(input$prod_quant, "\\W+")),
           "Please click on 'Send request' button after selecting the store, products, and filling in product quantities.")
    )
    
    selected_shown <- data.frame(prod = input$prod_selected,
                                 quantity = strsplit(input$prod_quant, "\\s+")[[1]]) %>%
      mutate(shown = paste0(prod, "  --  quantity: ", quantity))
    
    selected_shown$shown %>%
      paste(collapse = '<br/>') %>%
      HTML()
  })
  
  recommemded_prod_func <- eventReactive(input$post, {
    
    shiny::validate(
      need(length(input$prod_selected) > 0, "")
    )
    
    prod_regx <- paste0(input$prod_selected, collapse = "|")
    
    #filtered <- prod_listing %>%
    #  dplyr::filter(StoreID == input$store_selected,
    #                grepl(prod_regx, shown))
    
    #shiny::validate(need(nrow(filtered) > 0, "One or more products selected are not offered in the selected store."))
    
    storeID <<- as.numeric(input$store_selected)
    prodID <<- gsub("\\-.+$", "", input$prod_selected)
    prod_quantity <<- strsplit(input$prod_quant, "\\s+")[[1]] %>%
      as.numeric()
    model_picker <<- ifelse(input$appetisers_toggle, 2, 1)
    
    parsed <<- recommender_api(StoreId_val = storeID,
                              version_val = model_picker,
                              productId_val = c(prodID),
                              productQuantity_val = c(prod_quantity))
    
    #names(parsed)
    output_df <<- data.frame(score = unlist(replace_null2na(parsed[8])),
                            FullImageURL = unlist(replace_null2na(parsed[1])),
                            Price = unlist(replace_null2na(parsed[2])),
                            `Product Description` = unlist(replace_null2na(parsed[3])),
                            `Product Name` = unlist(replace_null2na(parsed[4])),
                            `Product SKU Description` = unlist(replace_null2na(parsed[5])),
                            product_variant_ID = unlist(replace_null2na(parsed[6]))) %>%
      mutate_all(list(~gsub("NA", "", .))) %>%
      mutate_at(c("Price", "score"), ~as.numeric(.)) %>%
      mutate(cheap_sort = -Price)
    
    return(output_df)
  })
  
  return_recommendation <- reactive({
    
    shiny::validate(
      need(length(input$prod_selected) > 0, "")
    )
    
    sort_metirc <<- switch(input$sort_selected,
                          "Most recommended" = "score", 
                          "Cheapest" = "cheap_sort")
    
    result <<- recommemded_prod_func() %>%
      dplyr::arrange(desc(get(sort_metirc))) %>%
      rowwise() %>%
      mutate(`Product.SKU.Description` = gsub(`Product.Name`, "", `Product.SKU.Description`)) %>%
      ungroup()
    
    return(result)
  })
  
  output$recommemded_prod1_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[1] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod2_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[2] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod3_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[3] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod4_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[4] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod5_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[5] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod6_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[6] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod7_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[7] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod8_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[8] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod9_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[9] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  output$recommemded_prod10_img <- renderText({
    res <- return_recommendation()
    img_link <- res$FullImageURL[10] %>% as.character()
    
    {c('<img src="',img_link,'">')}
  })
  
  
  get_prod_txt <- function(res, ind) {
    res[ind,] %>%
      mutate(shown = paste(`Product.Name`,
                           `Product.SKU.Description`,
                           `Product.Description`,
                           paste0("£", Price),
                           paste0("Product variant ID: ", product_variant_ID),
                           paste0("Recommender score: ", score),
                           sep = "<br/>"),
             shown = gsub("NA", "", shown)) %>%
      pull(shown) %>%
      HTML()
  }
  
  output$recommemded_prod1 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 1)
  })
  
  output$recommemded_prod2 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 2)
  })
  
  output$recommemded_prod3 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 3)
  })
  
  output$recommemded_prod4 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 4)
  })
  
  output$recommemded_prod5 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 5)
  })
  
  output$recommemded_prod6 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 6)
  })
  
  output$recommemded_prod7 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 7)
  })
  
  output$recommemded_prod8 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 8)
  })
  
  output$recommemded_prod9 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 9)
  })
  
  output$recommemded_prod10 <- renderText({
    res <- return_recommendation()
    get_prod_txt(res, 10)
  })
  
  #src = "https://upload.wikimedia.org/wikipedia/en/7/7d/Minions_characters.png"
  #output$picture<-renderText({c('<img src="',src,'">')})
  
})
