## How to Contribute Code (DRAFT)

:+1::tada: Hello and a warm welcome to guidelines for contributing code! :tada::+1:

  Thanks for taking the time to read these guidelines. This helps keep this code
  repository easy to maintain and the code easy to follow. These are mostly
  guidelines, not rules. Use your best judgment, and feel free to propose
  changes to this document in a pull request.

### Most changes to code should follow this route:

#### Open Issue

- Open an [issue](https://github.com/aquaMetrics/rict/issues) and describe what your update is planning to address.
- It's usually helpful to include a worked example of the update or issue.
- If you are not a maintainer, wait for feedback and comments - maybe there are different approaches that need exploring.

### Install Software

#### Via the "Cloud" - *Quick and Easy!*
1 Go to https://github.com/aquaMetrics/rict and click 'fork' in top right.
2 Sign-up to [R Studio Cloud](https://rstudio.cloud).
3 Click on New Project dropdown menu and select 'New Project from Git repo'.
4 Paste `https://github.com/YOUR-GITHUB-USERNAME/rict`. - replace `YOUR-GITHUB-USERNAME` or navigate to the forked repo you on your profile and copy paste URL.
5 You can now make changes to the files - look in the 'Files' tab and look for 'R' folder where most R code is held.
6 Add `browser()` to the line above where you wish to  add a breakpoint in the code/function.
7 Now run the function, try typing in the console: `rict(observed_demo_values)` - this will run most functions in the package.
8 The code will break where you added the `browser()` and you can investigate the values and see what needs changing etc.
9 Make changes as required then 'Install and Restart'.
10 Once you are happy with the code, go to 'Commit' section below.

#### Via Local Machine
1 Install [Git](https://git-scm.com/) on to your machine.
2 Install [R](https://cran.r-project.org/).
3 Install [Rstudio](https://www.rstudio.com/) - recommended - other editors are available.
4 Click on New Project dropdown menu and select 'New Project from Git repo'.
5 Follow instructions in 'Via the Cloud' section above from number 5.

#### Commit

- In the 'Git' panel commit select all the files you changes and click commit.
- Add a commit message which should reference the issue number e.g. 'docs: closes #1'- the '#1' is the issue number
- Not sure how to write a commit message? Try to use this [commit message guidance](https://gist.github.com/stephenparish/9941e89d80e2bc58a153#subject-line), although this is not enforced.
- Select 'Push' changes.

### Create Pull Request

- On your github profile page select the forked rict repo. This will now include the recent changes you made.
- Near the top there is an option to create pull request (PR).
- Select create pull request and enter message reference issue number '#1' that it fixes.
- The PR will be submitted and testing automatically run.
- Wait for response from maintainers.

#### All Done! ☺

#### Notes on Coding Style & Testing

- Add comments as required to explain the *why* rather than the *how*.
- Not every line needs comments - but any large or unusual sections.
- [R Package](http://r-pkgs.had.co.nz/) guidance is a very useful resource for writing packages.
- Linting / styling code is not enforced but easy to do.
- Install styler package.
- Run styler to from 'Addins' menu.
- Testing allows check coding 'style' with rules from [lintr](https://github.com/jimhester/lintr) package.
- But linting/style is not enforced (so failures are okay but not desired).
- Document code and follow standard CRAN checks and file structure `devtools::check()`.
- Run tests locally - before commit `devtools::test()`.
- See .lintr file for exact linting rules applied during testing.




