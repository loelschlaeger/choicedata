# Split choice data into a train and a test subset

This function splits choice data by deciders or by choice occasions, for
example to fit a model on the train subset and to evaluate its
predictions on the test subset.

## Usage

``` r
train_test(
  data_frame,
  test_proportion = NULL,
  test_number = NULL,
  by = "decider",
  random = FALSE,
  column_decider = "deciderID",
  column_occasion = NULL
)
```

## Arguments

- data_frame:

  \[`data.frame`\]  
  Contains the choice data.

- test_proportion:

  \[`numeric(1)` \| `NULL`\]  
  The proportion of deciders or occasions in the test subset.

- test_number:

  \[`integer(1)` \| `NULL`\]  
  The number of deciders, or of occasions per decider, in the test
  subset.

- by:

  \[`character(1)`\]  
  Split by `"decider"` or by `"occasion"`.

- random:

  \[`logical(1)`\]  
  Draw the test subset at random? Else, it is the last deciders or
  occasions.

- column_decider:

  \[`character(1)`\]  
  Column name with decider identifiers.

- column_occasion:

  \[`character(1)` \| `NULL`\]  
  Column name with occasion identifiers. Set to `NULL` in
  cross-sectional data.

## Value

A `list` of two subsets of `data_frame`, named `train` and `test`.

## Details

Exactly one of `test_proportion` and `test_number` sets the size of the
test subset. Splitting by occasions keeps every decider in both subsets
and applies the size per decider, which requires panel data.

## Examples

``` r
data("Train", package = "mlogit")

### 20% of the deciders in the test subset
parts <- train_test(Train, test_proportion = 0.2, column_decider = "id")
lengths(lapply(parts, function(part) unique(part$id)))
#> train  test 
#>   188    47 

### the last choice occasion of every decider in the test subset
parts <- train_test(
  Train, test_number = 1, by = "occasion",
  column_decider = "id", column_occasion = "choiceid"
)
nrow(parts$test)
#> [1] 235
```
