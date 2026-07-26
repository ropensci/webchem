## Pull Request

**Please do not include API keys in your pull request**. You should store any API keys or tokens in `.Renviron` or `.Rprofile` and be sure to have these files added to `.gitignore`. Do not include API keys in function examples or tests. See `cs_check_key()` for an example.

If this pull request adds a new database to the list of databases `webchem` can access, please provide some evidence that this database is OK with being accessed by a third-party---for example, a link to an About or FAQ page.

Before you submit a pull request, please do the following:

* Add an entry to NEWS concisely describing what you changed.
* If appropriate, add unit tests in the tests/testthat directory.
* Run Build->Check Package in the RStudio IDE, or `devtools::check()`, to make sure your change did not add any messages, warnings, or errors.

Doing these things will make it easier to evaluate your pull request.

We will try to be responsive and provide feedback.


## Testing

You can test the package using `devtools::test()`. Remember to set the environment variable `RUN_ONLINE_TESTS` to "true" if you want to run tests that interact with online resources.
Tests are disabled in the `master` branch (commented out in `~tests\testthat.R`).
For local testing you can uncomment, or run `test_check("webchem")`.
Please do not include changes to `~tests\testthat.R` in your PR.


Delete these instructions once you have read them.

---

Brief description of the PR


PR task list:
- [ ] Update NEWS
- [ ] Add tests (if appropriate)
- [ ] Update documentation with `devtools::document()`
- [ ] Check package passed

Extra tasks (more for maintainers, but feel free to do them if you want):

- [ ] Set the environment variable `RUN_ONLINE_TESTS` to "true"
- [ ] Test package locally (requires ~40 GB of local databases)
- [ ] Update README with `devtools::build_readme()` to update test coverage badge

