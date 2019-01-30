library({{package_name}})
library(RUnit, quietly = T)


testsuite <- RUnit::defineTestSuite("{{package_name}}",
                                    dirs = file.path("{{package_path}}","inst", "unitTests"),
                                    testFileRegexp =  "{{testFileRegexp}}",#  "^test.+\\.R",
                                    testFuncRegexp = "{{testFuncRegexp}}") #"^test.+")
testResult <- RUnit::runTestSuite(testsuite)


if(testResult${{package_name}}$nFail > 0 | testResult${{package_name}}$nErr > 0){
  # if at least one test function failed or raised an error, save text protocol
  sink(file.path("{{package_path}}", "output.txt"))
  RUnit::printTextProtocol(testResult)
  sink()
}

