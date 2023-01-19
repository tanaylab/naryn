test_that("string_to_var works", {
    expect_equal(string_to_var("a & b"), "a_.26_b")
    expect_equal(string_to_var("saba and savta"), "saba_and_savta")
    expect_equal(string_to_var("/home/mydir"), "X.2Fhome.2Fmydir")
    expect_equal(string_to_var("www.google.com"), "www.2Egoogle.2Ecom")
    expect_equal(string_to_var("my_variable + 3"), "my.5Fvariable_.2B_3")
    expect_equal(string_to_var(".hidden variable"), "X.2Ehidden_variable")
    expect_equal(string_to_var("NULL"), "NULL.")
})

test_that("var_to_string works", {
    expect_equal(var_to_string(string_to_var("a & b")), "a & b")
    expect_equal(var_to_string(string_to_var("saba and savta")), "saba and savta")
    expect_equal(var_to_string(string_to_var("/home/mydir")), "/home/mydir")
    expect_equal(var_to_string(string_to_var("www.google.com")), "www.google.com")
    expect_equal(var_to_string(string_to_var("my_variable + 3")), "my_variable + 3")
    expect_equal(var_to_string(string_to_var(".hidden variable")), ".hidden variable")
    expect_equal(var_to_string(string_to_var("NULL")), "NULL")
})
