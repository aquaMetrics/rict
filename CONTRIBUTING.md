## How to contribute code (DRAFT)

:+1::tada: Hello and a warm welcome to guidelines for contributing code! :tada::+1:

  Thanks for taking the time to read these guidelines. This helps keep this code repository easy to maintain and the code easy to follow. These are mostly guidelines, not rules. Use your best judgment, and feel free to propose changes to this document in a pull request.

### Most changes to code should follow this route:

#### Open issue

- Open an [issue](https://github.com/aquaMetrics/rict/issues) and describe what your update is planning to address
- It's usually helpful to include a worked example of the update or issue
- If you are not a maintainer, wait for feedback and comments - maybe there are different approaches that need exploring

#### Install software

- install [Git](https://git-scm.com/) on to your machine
- install [R](https://cran.r-project.org/)
- install [Rstudio](https://www.rstudio.com/) - recommended - other editors are available

#### Create new branch


If you have admin/contributor rights on aquaMetrics team, you can create a new branch there. Click 'Branch: ...' button on the main repository page. Then clone the repository to your local machine:
```
git clone https://github.com/aquaMetrics/rict
cd phytobenthosMetrics/
git checkout -b issue-??
```
(Replacing `??` with the issue number found on Github)

The first two lines above will clone the repository and open the directory where the files have been copied to. The last line will 'checkout' and create new branch called issue-...so you can now start work editing your new branch.

Or using RStudio, use open > new project > version control > paste url:

`https://github.com/aquaMetrics/rict`

- If you created the branch on Github remember to Pull to Rstudio and change into that branch using the 'Git' panel

#### Coding style

- Do some coding and/or write documentation etc
- Add comments as required to explain the *why* rather than the *how*
- Not every line needs comments - but any large or unusual sections
- Document code and follow standard CRAN checks and file structure
- [R Package](http://r-pkgs.had.co.nz/) guidance is a very useful resource
- Run checks and tests locally - before commit `devtools::test()`
- Testing allows check coding 'style' with rules from [lintr](https://github.com/jimhester/lintr) package
- See .lintr file for exact linting rules applied during testing

#### Commit

- Commit message should reference the issue number e.g. 'docs: closes #1'- the '#1' is the issue number 
- Not sure how to write a commit message? Try to use this [commit message guidance](https://gist.github.com/stephenparish/9941e89d80e2bc58a153#subject-line), although this is not enforced.
- Push changes to remote branch

#### Pull request

- On Github create a Pull Request to master branch from your issue-?? branch
- Wait for a maintainer to review, they will check no tests break, and the code has been tested if required
- Changes are merged to master and the 'test-1' branch deleted.

#### All Done! ☺


