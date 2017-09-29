main <- function(table_name ,
                 bucket_name,
                 file_prefix = "redshift_data",
                 aws_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                 aws_secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY")) {
    t0 <- Sys.time()
    sql_query = paste("select * from ", table_name, sep = "")
    con <- set_db_conn()

    query_r2s3 <- create_unload_query(sql_query, file_prefix, bucket_name, aws_key, aws_secret_key)

    rs <- execute_query_redshift(query_r2s3, con )

    col_def <- get_col_def(sql_query, con)
    disconnect_db_conn(con)

    manifest_url <- paste("s3://", bucket_name, "/",file_prefix, "_manifest", sep = "")
    data <- get_data_from_s3(manifest_url, aws_key, aws_secret_key, col_def)

    data <- change_col_type(data, col_def$Sclass)
    time_spent <- difftime(Sys.time(), t0, units = "secs" )

    return(list(data = data, time = time_spent))
}

