#' Set connection with Amazon Redshift database.
#'
#' @param host_ip Ip adress of database server.
#' @param port Port number.
#' @param db_user A name of the user account. By default get from .Renviron file.
#' @param db_pass A password to the user account. By default get from .Renviron file.
#' @param db_name A name of the database.
#' @return Connection string.
#' @example
#' set_db_conn()
set_db_conn <- function(host_ip,
                        port,
                        db_user = Sys.getenv("REVEL_DB_USER"),
                        db_pass  = Sys.getenv("REVEL_DB_PASS"),
                        db_name ){
    if ((db_user != "") & (db_pass != "")) {
        con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                              host = host_ip,
                              port = port,
                              user = db_user,
                              password = db_pass,
                              dbname = db_name)
    } else {

    }
}

#' Disconnect with database
#'
#' @param con Connection string.
disconnect_db_conn <- function(con) {
    DBI::dbDisconnect(con)
}

create_unload_query <- function( sql_query,
                                 bucket_name,
                                 file_prefix = "redshift_data",
                                 aws_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                 aws_secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY")) {
    if ( substr(file_prefix, nchar(file_prefix), nchar(file_prefix)) == "_") {
        file_prefix <- substr(file_prefix,1, nchar(file_prefix) - 1)
    }
    ## in the following string apostroph (') will show in query
    unload_query <-  paste("UNLOAD ('", sql_query , "')
                      TO 's3://", bucket_name, "/",file_prefix, "_'
                      ACCESS_KEY_ID '", aws_key,
                      "' SECRET_ACCESS_KEY '", aws_secret_key,
                      "' MANIFEST ALLOWOVERWRITE;", sep ="")
    # cleaning white spaces and \n
    unload_query <- gsub("  ", "", unload_query)
    unload_query <- gsub("\n", " ", unload_query)
}

# czy to powinno byc tak, czy lepiej przekazywac do funkcji jako query wynik funkcji create_unload_query
execute_query_redshift <- function(sql_query, con) {
    ## send table to aws.s3
    ## in the following string: ' will show in query
    rs <- DBI::dbSendQuery(con, sql_query )
}

#' getting column definition
#'
#' @param query
#' @param con Database connection string.
#' @return data frame with column names and types.
get_col_def <- function(query, con) {
    rs <- execute_query_redshift(paste(query, " limit 0", sep = ""), con)
    col_def <- DBI::dbColumnInfo(rs)
    DBI::dbClearResult(rs)
    return(col_def)
}

get_file2obj <- function(file_url,aws_key, aws_secret_key) {
    obj <- aws.s3::get_object( file_url,
                               key = aws_key,
                               secret = aws_secret_key)
}

obj2df <- function(obj, col_def) {
    df <- data.table::fread(input = rawToChar(obj), sep ="|"
                   , header = F
                   , col.names = col_def$name
                   , colClasses = rep("character", dim(col_def)[1])
                   , na.strings = ''
                   , data.table = FALSE
    )
}


file2df <- function(file_url, col_def, aws_key, aws_secret_key) {
    t0 <- Sys.time()
    data <- obj2df(get_file2obj(file_url,aws_key, aws_secret_key), col_def)
    t1 <- Sys.time()
   # print(paste ("getting data", Sys.time()  - t0, sep = " " ))
    # data <- change_col_type(data, col_def$Sclass)
    # print(paste ("getting data", t1 - t0, "changing types", Sys.time() - t1, sep = " " ))
    return(data)
}


get_data_from_s3 <- function(manifest_url, aws_key, aws_secret_key, col_def){
    obj <- get_file2obj( manifest_url, aws_key, aws_secret_key)
    manifest <- jsonlite::fromJSON(rawToChar(obj), flatten=TRUE)[[1]]
    list_df <- apply(manifest, 1, file2df, col_def = col_def, aws_key = aws_key, aws_secret_key = aws_secret_key)
    t0 <- Sys.time()
    df_all <- dplyr::bind_rows(list_df)
   #  print(paste ("joining data ", Sys.time() - t0, sep = "" ))
    return(df_all)
}


change_col_type <- function(df, vec_type){
    for (i in 1:length(vec_type)) {
       # t0 <- Sys.time()
        if (vec_type[i] == "POSIXct") {
            df[i] <- fasttime::fastPOSIXct(df[, i], tz = "GMT")
        } else {
            df[i] <- eval(call(paste("as.", vec_type[i], sep = ""), df[, i]))
        }
      #  print(paste(i, vec_type[i], Sys.time() - t0))

    }
    return(df)
}


