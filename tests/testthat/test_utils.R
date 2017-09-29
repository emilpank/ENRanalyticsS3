test_that("create_unload_query generate valid query", {
    test_result <- "UNLOAD ('select * from table') TO 's3://efficacy-analytics-prd/redshift_data_' ACCESS_KEY_ID 'AKIAIE5EOVC3VUYA2VVA' SECRET_ACCESS_KEY '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' MANIFEST;"
    expect_equal(create_unload_query("select * from table"), test_result)

    expect_equal(create_unload_query("select * from table", file_prefix = "redshift_data_"), test_result)
    expect_equal(create_unload_query("select * from table", file_prefix = "redshift_data_", bucket_name = "bucket"),
                 "UNLOAD ('select * from table') TO 's3://bucket/redshift_data_' ACCESS_KEY_ID 'AKIAIE5EOVC3VUYA2VVA' SECRET_ACCESS_KEY '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' MANIFEST;")
} )
