# Ordered Smoking Responses of Students

Ordered, cross-sectional smoking responses from 237 Statistics I
students at the University of Adelaide. The response levels are never,
occasional, regular, and heavy.

This data can be used to model the choice of smoking based on age and
exercise frequency.

## Usage

``` r
student_smoking_choice
```

## Format

A `tibble` with 237 rows and 4 columns:

- student \[`integer`\]:

  The student identifier.

- choice \[`ordered`\]:

  The smoking level.

- age \[`numeric`\]:

  The student's age in years.

- exercise \[`factor`\]:

  Whether the student exercises frequently, sometimes, or not at all.

## Source

Adapted from `survey` in the [MASS
package](https://CRAN.R-project.org/package=MASS). The original student
survey is documented by Venables and Ripley (2002).

## References

Venables WN, Ripley BD (2002). *Modern Applied Statistics with S*, 4
edition. Springer, New York. ISBN 978-0-387-21706-2.
