#'
#'

context("__ Mocks __")

context("Class imitation")

	test_that("Mock mimicks specified class", {
				
				list.mock <- Mock("list")
				test.fun <- function() NULL
				fun.mock <- Mock(test.fun)
				
				expect_that(class(list.mock), matchesObject("Mock"))
				expect_that(is(list.mock, "list"), is_true())
				expect_that(fun.mock, is_a("function"))
			})
	
	test_that("Mock can be assigned to S4 slots", {
				
				s4 <- new("testS4")
				list.mock <- Mock("list")
				s4@container <- list.mock
			})


context("Method creation")

	test_that("Mock replicates method", {

				mock <- Mock()
				mockMethod(mock, "TestMethod")
				expect_that(exists("TestMethod"), is_true())
			})
	
	test_that("Mock replaces existing function", {
				
				TestMethod <- function(mock) {
					stop("Should not be called")
				}
				
				mock <- Mock()
				mockMethod(mock, "TestMethod")
				
				result <- TestMethod(mock)
				
				expect_that(result, equals(NULL))
				expect_that(mock, called_once("TestMethod"))
			})
	
	test_that("Mock method created when not existing", {
				
				mock <- Mock()
				mockMethod(mock, "TestMethod_2")
				TestMethod_2(mock)
				
				expect_that(mock, called_once("TestMethod_2"))
			})
	
	test_that("Mock method created when mock has spec", {
				
				mock <- Mock("list")
				mockMethod(mock, "TestMethod_3", return.value = 3)
				TestMethod_3(mock)
				
				expect_that(mock, called_once("TestMethod_3"))
			})
	
	test_that("Mock method created when existing S3 generic", {
				
				mock <- Mock()
				mockMethod(mock, "plot", return.value = NULL)
				plot(mock)
				expect_that(mock, called_once("plot"))
			})
	
	test_that("Mock method created when existing S4 generic", {
				
				mock <- Mock()
				mockMethod(mock, "TestS4generic")
				TestS4generic(mock)
				
				expect_that(mock, called_once("TestS4generic"))
			})
	
	test_that("Mock method overrides existing S4 method", {
				
				mock <- Mock("testS4")
				
				result.before.mock <- TestS4method(mock)
				mockMethod(mock, "TestS4method", return.value = "PASS")
				result.after.mock <- TestS4method(mock)
				
				expect_that(result.before.mock, equals("S4 method"))
				expect_that(mock, called_once("TestS4method"))
				expect_that(result.after.mock, equals("PASS"))
			})
	
context("Mock method reporting")	

	test_that("Mock reports on method call", {
				
				mock <- Mock()
				mockMethod(mock, "TestMethod")
				TestMethod(mock)
				expect_that(mock, called_once("TestMethod"))
			})
	
	test_that("Multiple calls fails called once test", {
				
				mock <- Mock()
				mockMethod(mock, "TestMethod")
				TestMethod(mock)
				TestMethod(mock)
				expect_that(called_once("TestMethod")(mock)$passed, is_false())
			})
	
	test_that("Mock reports on method with one argument", {
				
				mock <- Mock()
				mockMethod(mock, "TestMethod")
				TestMethod(mock, 1)
				expect_that(mock, called_once_with("TestMethod", 1))
				expect_that(called_once_with("TestMethod", 2)(mock)$passed, is_false())
				expect_that(called_once_with("Testing", 1)(mock)$passed, is_false())
			})
	
	test_that("Mock reports on method with multiple arguments", {
				
				mock <- Mock()
				mockMethod(mock, "TestMethod")
				TestMethod(mock, 1, 2)
				expect_that(mock, called_once_with("TestMethod", 1, 2))
				expect_that(called_once_with("Testing", 1)(mock)$passed, is_false())
			})
	
	test_that("Mock method returns value", {
				
				mock <- Mock()
				return.value <- "Yes"
				mockMethod(mock, "TestMethod", return.value = return.value)
				returned <- TestMethod(mock)
				
				expect_that(mock, called_once("TestMethod"))
				expect_that(returned, equals(return.value))
			})

context("Assigning mock methods")
	
	test_that("Mock registers method calls", {
				
				mock <- Mock()
				mockMethod(mock, "TestMethod")
				mock2 <- Mock()
				
				expect_that(TestMethod(mock2), throws_error("Unexpected method call"))
			})
	
	test_that("Mocks can have separate return values from same method", {
				
				mock1 <- Mock()
				mock2 <- Mock()
				
				mockMethod(mock1, "TestMethod", return.value = 1)
				mockMethod(mock2, "TestMethod", return.value = 2)
				
				return1 <- TestMethod(mock1)
				return2 <- TestMethod(mock2)
				
				expect_that(return1, equals(1))
				expect_that(return2, equals(2))
				expect_that(mock1, called_once("TestMethod"))
				expect_that(mock2, called_once("TestMethod"))
			})
	
	test_that("Mock with spec and mock method", {
				
				mock <- Mock("list")
				mockMethod(mock, "TestMethod")
				TestMethod(mock)
				expect_that(mock, called_once("TestMethod"))
				
			})
	
	test_that("Multiple Mocks can have same method assigned with one call", {
				
				mock1 <- Mock("list")
				mock2 <- Mock("list")
				
				mockMethod(list(mock1, mock2), "Testing", return.value = 1)
				
				return1 <- Testing(mock1)
				return2 <- Testing(mock2)
				
				expect_that(return1, equals(1))
				expect_that(return2, equals(1))
				expect_that(mock1, called_once("Testing"))
				expect_that(mock2, called_once("Testing"))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	

