
#ref
# https://ropenscilabs.github.io/r-docker-tutorial/

# https://rud.is/b/2017/02/09/diving-into-dynamic-website-content-with-splashr/

# https://docs.docker.com/get-started/


dir <- tempfile()
dir.create(dir)
htmlFile <- file.path(dir, "index.html")
writeLines(
  '<h1 style="text-align: center;">Thank you for using <span style="color: #ff0000;">Tidycells</span></h1>
<p style="text-align: center;">It is an <span style="color: #ff6600;"><strong>assistant</strong> </span>for you</p>
<p style="text-align: center;">It is <span style="text-decoration: underline;">yet to <span style="color: #0000ff; text-decoration: underline;">evolve</span></span></p>
<iframe src="http://localhost:6833/" title="Test Shiny">
  ',
  htmlFile
)
suppressWarnings(rstudioapi::viewer(htmlFile))
