# Study Design and Management

In our proposed workflow, we put a lot of emphasis on the plate design
and management. The plate will carry all information needed not only for
bioanalytical purposes, but also the pharmacokinetic information needed
for the study.

``` r

library(PKbioanalysis)
```

``` r

newstudy <- data.frame(
  type = "SD",
  pkstudy = TRUE,
  title = "New Study",
  description = "Description of the new study",
  subject_type = "Human"
)
new_study <- create_new_study(newstudy)
```
