# Define choice identifiers

The `choice_identifiers` object defines identifiers for the deciders and
choice occasions.

- `generate_choice_identifiers()` generates identifiers.

- `extract_choice_identifiers()` extracts choice identifiers.

## Usage

``` r
choice_identifiers(
  data_frame,
  format = "wide",
  column_decider = "deciderID",
  column_occasion = "occasionID",
  cross_section = is.null(column_occasion)
)

generate_choice_identifiers(
  N = length(Tp),
  Tp = 1,
  column_decider = "deciderID",
  column_occasion = "occasionID"
)

extract_choice_identifiers(
  x,
  format = attr(x, "format"),
  column_decider = attr(x, "column_decider"),
  column_occasion = attr(x, "column_occasion"),
  cross_section = attr(x, "cross_section")
)
```

## Arguments

- data_frame:

  \[`data.frame`\]  
  Contains the choice identifiers.

- format:

  \[`character(1)`\]  
  Either `"wide"` or `"long"`.

  In the long case, unique combinations of `column_decider` and
  `column_occasion` (if present) are used.

- column_decider:

  \[`character(1)`\]  
  The name of the identifier column for deciders.

- column_occasion:

  \[`character(1)` \| `NULL`\]  
  The name of the identifier column for choice occasions (panel data).
  Can be `NULL` for the cross-sectional case.

- cross_section:

  \[`logical(1)`\]  
  Treat choice data as cross-sectional?

- N:

  \[`integer(1)`\]  
  The number of deciders.

- Tp:

  \[`integer(1)` \| `integer(N)`\]  
  The number of choice occasions per decider.

  Can also be of length `N` for a variable number of choice occasions
  per decider.

- x:

  A
  [`choice_data`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  or
  [`choice_covariates`](https://loelschlaeger.de/choicedata/reference/choice_covariates.md)
  object.

## Value

An object of class `choice_identifiers`, which is a `tibble` with
columns:

1.  `column_decider` contains the decider identifiers,

2.  `column_occasion` contains the choice occasion identifiers (only if
    `column_occasion` is not `NULL` and `cross_section = FALSE`).

## Examples

``` r
### panel case
generate_choice_identifiers(N = 2, Tp = 2)
#> # A tibble: 4 × 2
#>   deciderID occasionID
#> * <chr>     <chr>     
#> 1 1         1         
#> 2 1         2         
#> 3 2         1         
#> 4 2         2         

### cross-sectional case
generate_choice_identifiers(N = 5, column_occasion = NULL)
#> # A tibble: 5 × 1
#>   deciderID
#> * <chr>    
#> 1 1        
#> 2 2        
#> 3 3        
#> 4 4        
#> 5 5        

### read choice identifiers
choice_identifiers(
  data_frame = travel_mode_choice,
  format = "long",
  column_decider = "individual",
  column_occasion = NULL,
  cross_section = TRUE
)
#> # A tibble: 210 × 1
#>    individual
#>  * <chr>     
#>  1 1         
#>  2 2         
#>  3 3         
#>  4 4         
#>  5 5         
#>  6 6         
#>  7 7         
#>  8 8         
#>  9 9         
#> 10 10        
#> # ℹ 200 more rows
```
