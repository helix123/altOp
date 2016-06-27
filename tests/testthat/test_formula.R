# HOWTO: http://r-pkgs.had.co.nz/tests.html

## TODO: many more tests

## TODO: more formulas with special characters, e.g. "*", see ?formula
##       => package should be able to handle those

# TODO:
formB  <- formula(compvar ~ roa + log(emp) + beta + age + tenure)
formB2 <- sub_vars(formB, "emp", "marketcap")      # inside functions: works => log(marketcap)
formB3 <- sub_vars(formB, "log(emp)", "marketcap") # substitute function: does not (yet?) work; should it?


# TODO: Tests for generating all formulae

context("Formula manipulation")

data("CEOcomp")

test_that("simple variables are substituted", {
  form  <- formula(compvar ~ roa + emp + beta + age + tenure)
  res_form2 <- sub_vars(form, "roa", "roe")
  res_form3 <- sub_vars(form, "roa", "tsr")

  expect_equal(res_form2,
               formula(compvar ~ roe + emp + beta + age + tenure))

  expect_equal(res_form3,
               formula(compvar ~ tsr + emp + beta + age + tenure))
})

test_that("variables substitution in interaction terms,
          all occurences of tenure are substituted, also in the interaction term", {
  form_int <- formula(compvar ~ roa + emp + beta + tenure + tenure:listed)
  res_form_int2 <- sub_vars(form_int, "tenure", "age")

  expect_equal(res_form_int2,
               formula(compvar ~ roa + emp + beta + age + age:listed))
})

test_that("warning if no change is detectd (output identical to input)", {
  form  <- formula(compvar ~ roa + emp + beta + age + tenure)
  expect_warning(
    sub_vars(form, "emp", "emp"),
    "Resulting formula is the same as input formula.")
})

test_that("LHS shall be preserved when variable is substituted (on RHS)", {
  form  <- formula(log(compvar) ~ roa + emp + beta + age + tenure)
  res_form2 <- sub_vars(form, "emp", "marketcap")

  expect_equal(res_form2,
               formula(log(compvar) ~ roa + marketcap + beta + age + tenure))
})



# form <- formula(compvar ~ 0 + roa + emp + beta + age + tenure)
#
# all.vars(form) # does not contain constant
# attr(terms(form), "intercept") # indicates whether intercept is present in a formula

test_that("Respect removal of intercept", {
  form1 <- formula(compvar ~ roa + emp + beta + age + tenure - 1)
  form2 <- formula(compvar ~ roa + emp + beta + age + tenure + 0)
  form3 <- formula(compvar ~ 0 + roa + emp + beta + age + tenure)

  res_form1 <- sub_vars(form1, "emp", "marketcap")
  res_form2 <- sub_vars(form2, "emp", "marketcap")
  res_form3 <- sub_vars(form3, "emp", "marketcap")

  expect_equal(attr(terms(res_form1), "intercept"), 0)
  expect_equal(attr(terms(res_form2), "intercept"), 0)
  expect_equal(attr(terms(res_form3), "intercept"), 0)

  ## maybe, the tests should not require preserving the exact position of removal of constant?
  expect_equal(res_form1,
               formula(compvar ~ roa + marketcap + beta + age + tenure - 1))
  expect_equal(res_form2,
               formula(compvar ~ roa + marketcap + beta + age + tenure + 0))
  expect_equal(res_form3,
               formula(compvar ~ 0 + roa + marketcap + beta + age + tenure))
})
