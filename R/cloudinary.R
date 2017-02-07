

#' @export
loadCloudinaryConfig <- function(){
  yaml.load_file("cloudinary-keys.yaml")
}

#' @export
cloudinary_getAllImages <- function(prefix = NULL, cfg = NULL, nmax = 100){
  prefix <- prefix %||% ""
  if(is.null(cfg))
    cfg <- loadCloudinaryConfig()
  cfg$action <- "resources/image"
  cfg$next_cursor <- ""
  cfg$max_results <- 500
  cfg$prefix <- prefix
  nms <- c("public_id","format","version","resource_type","type","created_at","bytes","width","height","url","secure_url")
  dfs <- createEmptyDf(nms)

  while(!is.null(cfg$next_cursor)){
    baseUrl <- "https://api.cloudinary.com/v1_1/{cloud_name}/{action}/?next_cursor={next_cursor}&max_results={max_results}&prefix={prefix}"
    url <-pystr_format(baseUrl,cfg)
    message(url)
    r <- GET(url, authenticate(cfg$apiKey, cfg$apiSecret))
    r <- fromJSON(rawToChar(r$content))
    dfs <- rbind.data.frame(dfs,r$resources)
    cfg$next_cursor <- r$next_cursor
    if(nrow(dfs) > nmax) break
  }
  dfs
}

#' @export
cloudinary_getImagesInFolder <- function(prefix = NULL){
  prefix <- prefix %||% ""
  cfg <- loadCloudinaryConfig()
  cfg$action <- "resources/image/upload"
  cfg$next_cursor <- ""
  cfg$max_results <- 500
  cfg$prefix <- prefix
  nms <- c("public_id","format","version","resource_type","type","created_at","bytes","width","height","url","secure_url")
  dfs <- createEmptyDf(nms)

  while(!is.null(cfg$next_cursor)){
    baseUrl <- "https://api.cloudinary.com/v1_1/{cloud_name}/{action}/?next_cursor={next_cursor}&max_results={max_results}&prefix={prefix}"
    url <-pystr_format(baseUrl,cfg)
    message(url)
    r <- GET(url, authenticate(cfg$apiKey, cfg$apiSecret))
    r <- fromJSON(rawToChar(r$content))
    dfs <- rbind.data.frame(dfs,r$resources)
    cfg$next_cursor <- r$next_cursor
  }
  dfs
}


#' @export
cloudinary_createUploadPreset <- function(presetName = NULL, folder = NULL){
  presetName <- presetName %||% random_name(6)
  cfg <- loadCloudinaryConfig()
  baseUrl <- "https://api.cloudinary.com/v1_1/{cloud_name}/upload_presets?name={presetName}&unsigned=true&tags=remote&allowed_formats=jpg,png&folder={folder}"
  cfg$presetName <- presetName
  cfg$folder <- folder %||% presetName
  url <-pystr_format(baseUrl,cfg)
  r <- POST(url, authenticate(cfg$apiKey, cfg$apiSecret))
  message(rawToChar(r$content))
}

#' @export
cloudinary_uploadUnsignedImage <- function(file = NULL,preset = NULL){

  cfg <- loadCloudinaryConfig()
  cfg$action <- "resources/image"
  cfg$next_cursor <- ""
  cfg$max_results <- 500
  baseUrl <- "https://api.cloudinary.com/v1_1/{cloud_name}/image/upload"
  url <- pystr_format(baseUrl,cfg)
  public_id <- file_path_sans_ext(basename(file))
  if(!isUrl(file)){
    file <- image_uri(file)
  }
  timestamp <- as.integer(as.numeric(as.POSIXct(Sys.time())))
  r2 <- POST(url,authenticate(cfg$apiKey, cfg$apiSecret),
             body = list(file = file,
                         upload_preset = preset,
                         public_id = public_id)
  )
  if(r2$status_code != 200)
    return(paste("ERROR: ", content(r2)$error$message))
  fromJSON(content(r2,"text"))
}

#' @export
cloudinary_uploadUnsignedImages <- function(files, preset){
  i <- 1
  lapply(files,function(file){
    if(i %% 100 == 0){
      message("processed... ",i)
    }
    i <<- i+1
    cloudinary_uploadUnsignedImage(file,preset)
  })
}

# uploadImage <- function(file = NULL){
#   file <- file %||% system.file("imgs/lenna.png", package = "cloudinaR", mustWork=TRUE)
#   cfg <- loadConfig()
#   cfg$action <- "resources/image"
#   cfg$next_cursor <- ""
#   cfg$max_results <- 500
#   baseUrl <- "https://api.cloudinary.com/v1_1/{cloud_name}/image/upload"
#   url <- pystr_format(baseUrl,cfg)
#   fileBase64 <- image_uri(file)
#   timestamp <- as.integer(as.numeric(as.POSIXct(Sys.time())))
#   public_id <- path_sans_ext(basename(file))
#   # create signature for upload: http://cloudinary.com/documentation/upload_images#creating_api_authentication_signatures
#   # serialized params
#   serializedString <- "public_id={public_id}&timestamp={timestamp}{apiSecret}"
#   serializedString <- pystr_format(serializedString,
#                                    list(public_id=public_id,
#                                         timestamp = timestamp,
#                                         apiSecret = cfg$apiSecret)
#   )
#   serializedString
#   signature <- openssl::sha1(serializedString)
#   body <- list(file = fileBase64, api_key = cfg$apiKey, timestamp = timestamp, signature = signature)
#   r <- POST(url,authenticate(cfg$apiKey, cfg$apiSecret),body = body)
#   r <- POST(url,body = body)
#   rawToChar(r$content)
#
#
#   r2 <- POST(url,authenticate(cfg$apiKey, cfg$apiSecret),
#              body = list(file = fileBase64,
#                          upload_preset = "uezz6j02")
#   )
#   rawToChar(r2$content)
#
# }
