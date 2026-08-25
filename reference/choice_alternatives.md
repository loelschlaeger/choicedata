# Define choice alternatives

The `choice_alternatives` object defines the set of choice alternatives.

## Usage

``` r
choice_alternatives(
  J = 2,
  alternatives = LETTERS[1:J],
  base = NULL,
  ordered = FALSE
)

# S3 method for class 'choice_alternatives'
print(x, ...)
```

## Arguments

- J:

  \[`integer(1)`\]  
  The number \\\geq 2\\ of choice alternatives.

- alternatives:

  \[`character(J)`\]  
  Unique labels for the choice alternatives.

- base:

  \[`character(1)`\]  
  The name of the base alternative for alternative-constant covariates,
  see details.

  If `NULL` (default), it is set to the first retained alternative. This
  is the first supplied alternative when `ordered = TRUE` and the first
  alphabetically sorted alternative otherwise.

- ordered:

  \[`logical(1)`\]  
  Should the supplied order of `alternatives` be preserved and treated
  as an intrinsic ranking (for ordered response models)?

  When `TRUE`, the alternatives are kept in the given order.

  Otherwise, they are sorted alphabetically.

- x:

  \[`choice_alternatives`\]  
  A `choice_alternatives` object.

- ...:

  Currently not used.

## Value

An object of class `choice_alternatives`, i.e. a `character` vector of
the choice alternatives with attributes:

- `J`:

  The number of choice alternatives.

- `base`:

  The name of the base alternative.

- `ordered`:

  Do the alternatives encode an inherent ordering?

## Base alternative

The full set of coefficients for covariates that are constant across
alternatives (including alternative-specific constants) is not
identified. To achieve identifiability, the coefficient of alternative
`base` is fixed to zero. The other coefficients then have to be
interpreted with respect to `base`. The base alternative is marked with
a `*` when printing a `choice_alternatives` object.

## Examples

``` r
### unordered choice alternatives: heating
choice_alternatives(
  J = 3,
  alternatives = c("gas", "electricity", "oil"),
  base = "gas"
)
#> 
#> ── Choice alternatives 
#> • electricity
#> • gas*
#> • oil

### ordered choice alternatives: opinion
choice_alternatives(
  J = 4,
  alternatives = c("very good", "good", "neither good nor bad", "bad"),
  ordered = TRUE
)
#> 
#> ── Choice alternatives (ordered) 
#> • very good*
#> • good
#> • neither good nor bad
#> • bad
```
