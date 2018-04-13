## Pull Request

Before you submit a pull request, please do the following:

* Add an entry to NEWS concisely describing what you changed.
* If appropriate, add unit tests in the tests/testthat directory.
* Run Build->Check Package in the RStudio IDE, or `devtools::check()`, to make sure your change did not add any messages, warnings, or errors.

Doing these things will make it easier to evaluate your pull request.

We will try to be responsive and provide feedback.

## Minimal reproducible example

Finally, please include a minimal reproducible example (reprex). 
The goal of a reprex is to make it as easy as possible for me to recreate your problem so that we can fix it. 
If you've never heard of a reprex before, start by reading <https://github.com/jennybc/reprex#what-is-a-reprex> or <http://tinyurl.com/reproducible-000>. 

## Testing

You can test the package using `devtools::test()`. 
Tests are disable in the `master` branch (commented out in `~tests\testthat.R`).
For local testing you can uncomment, or run `test_check("webchem")`.
Please do not include changes to `~tests\testthat.R` in your PR.


Delete these instructions once you have read them.

---

Brief description of the PR

```r
# insert reprex here
```

PR task list:
- [ ] Update NEWS
- [ ] Add tests (if appropriate)
- [ ] Update documentation with `devtools::document()`
- [ ] Check package passed