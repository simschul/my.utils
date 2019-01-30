# internal function
.create_testsuite_template <- function(package_name,
                                       package_path,
                                       testFileRegexp = "^test.+\\\\.R",
                                       testFuncRegexp = "^test.+"
                                       ){

  template_path <- system.file(file.path("extdata", "template.R"),
                               package = "filecovrunit",
                               mustWork = T)
  package_path <- normalizePath(package_path,
                                winslash = "/")

  template_out <- whisker::whisker.render(template = readLines(template_path),
                                          data = list("package_name" = package_name,
                                                      "package_path" = package_path,
                                                      "testFileRegexp" = testFileRegexp,
                                                      "testFuncRegexp" = testFuncRegexp))
  writeLines(template_out, (file.path(package_path, "tests", "test_runit.R")))
  return(NULL)

}


