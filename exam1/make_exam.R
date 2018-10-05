library(rmarkdown)

# student versions
render("exam1.Rmd", params = list(
  seed = 1,
  key = FALSE
), output_file = "exam1_v1.pdf")
render("exam1.Rmd", params = list(
  seed = 2,
  key = FALSE
), output_file = "exam1_v2.pdf")
render("exam1.Rmd", params = list(
  seed = 3,
  key = FALSE
), output_file = "exam1_v3.pdf")

# key versions
render("exam1.Rmd", params = list(
  seed = 1,
  key = TRUE
), output_file = "exam1_v1_soln.pdf")
render("exam1.Rmd", params = list(
  seed = 2,
  key = TRUE
), output_file = "exam1_v2_soln.pdf")
render("exam1.Rmd", params = list(
  seed = 3,
  key = TRUE
), output_file = "exam1_v3_soln.pdf")
