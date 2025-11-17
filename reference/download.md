# Download file from the Internet

Download file from the Internet

## Usage

``` r
download(
  url,
  destfile,
  methods = c("auto", "wget", "libcurl", "curl", "wininet", "internal"),
  quiet = FALSE,
  ...,
  max_tries = 2
)
```

## Arguments

- url:

  a [`character`](https://rdrr.io/r/base/character.html) string (or
  longer vector for the `"libcurl"` method) naming the URL of a resource
  to be downloaded.

- destfile:

  a character string (or vector, see the `url` argument) with the file
  path where the downloaded file is to be saved. Tilde-expansion is
  performed.

- methods:

  Methods to be used for downloading files. Can be `"auto"`, `"wget"`,
  `"libcurl"`, `"curl"`, `"wininet"`, `"internal"`. Default is `"auto"`,
  which means to try different download methods.

- quiet:

  If `TRUE`, suppress status messages (if any), and the progress bar.

- ...:

  Other arguments passed to
  [utils::download.file](https://rdrr.io/r/utils/download.file.html).

- max_tries:

  Number of tries for each download method. Default is `2`.
