## Test Environment
* local Ubuntu 14.04.02 LTS install
* Windows using devtools::build_win()

## R CMD check results - run using devtools::check
There were no ERRORs or WARNINGs

There was 1 NOTE:

* Found the following calls to attach():
File ‘mockR/R/Mock.R’:
  attach(NULL, pos = 2L, name = "mock_methods")
  attach(mock.methods.env, pos = 2L, name = "mock_methods")
  
  Explanation: Part of the core functionality of this package is to intercept function
  calls and record the behaviour. In order to do this I've used attach to ensure that 
  the 'mock_methods' are at the top of the search path.