## How to Contribute Code (DRAFT)

:+1::tada: Hello and a warm welcome to guidelines for contributing code! :tada::+1:

  Thanks for taking the time to read these guidelines. This helps keep this code
  repository easy to maintain and the code easy to follow. These are mostly
  guidelines, not rules. Use your best judgment, and feel free to propose
  changes to this document in a pull request.

### Most changes to code should follow this route:

#### Open Issue

1. Sign-up to [Github](https://github.com).
2. Open an [issue](https://github.com/aquaMetrics/rict/issues) and describe what your update is planning to address.
3. It's usually helpful to include a worked example of the feature update or issue.
4. If you are not a maintainer, wait for feedback and comments - maybe there are different approaches that need exploring.

### Install Software

#### Via the "Cloud" - *Quick and Easy!*
1. Go to https://github.com/aquaMetrics/rict and click 'fork' in top right.
2. Sign-up to [R Studio Cloud](https://rstudio.cloud).
3. Click on New Project dropdown menu and select 'New Project from Git repo'.
4. Paste `https://github.com/YOUR-GITHUB-USERNAME/rict`. - replace `YOUR-GITHUB-USERNAME` or navigate to the forked repo you on your profile and copy paste URL.
5. Next install package dependencies. In the Console panel type: `install.packages("devtools")` then `devtools::install_dev_deps()` then enter `1` to install latest versions.
5. You can now make changes to the files - look in the 'Files' tab and look for 'R' folder where most R code is held.
6. Add `browser()` to the line above where you wish to  add a breakpoint in the code/function.
7. In the Build tab select 'Install and Build'. 
7. Now run the function, try typing in the console: `rict(observed_demo_values)` - this will run most functions in the package.
8. The code will break where you added the `browser()` and you can investigate the values and see what needs changing etc.
9. Make changes as required then 'Install and Restart'.
10. Once you are happy with the code, go to 'Commit' section below. Note, if you have updated documentation, run `pkgdown::build_site()` in the console, this will update package website when your changes are merged: https://aquametrics.github.io/rict/.

#### Via Local Machine
1. Install [Git](https://git-scm.com/) on to your machine.
2. Install [R](https://cran.r-project.org/).
3. Install [Rstudio](https://www.rstudio.com/) - recommended - other editors are available.
4. Click on New Project dropdown menu and select 'New Project from Git repo'.
5. Follow instructions in 'Via the Cloud' section above from number 5.

#### Commit

1. This step is only needed on your first commit, select the Terminal panel (in lower left) type:  
 `git config --global user.email "your.email@address.co.uk"`  
 `git config --global user.name "YOUR_GITHUB_USERNAME"`  
 (changing the email and username to match your own)
2. In the 'Git' panel (top-right) commit select all the files you changed and click commit.
3. Add a commit message (which if applicable, includes the reference to the issue number e.g. 'docs: closes #1'- the '#1' is the issue number)
4. Not sure how to write a commit message? Try to use this [commit message guidance](https://gist.github.com/stephenparish/9941e89d80e2bc58a153#subject-line), although this is not enforced.
5. Select 'Push' changes.

### Create Pull Request

1. On your github profile page select the forked rict repo. This will now include the recent changes you made.
2. Near the top there is an option to create pull request (PR).
3. Select create pull request and enter message reference issue number '#1' that it fixes.
4. The PR will be submitted and testing automatically run.
5. Wait for response from maintainers.

### Updating Azure Experiments

1. After, your PR is merged to master branch, automatic tests and builds will be run in "the cloud".
2. These tests check the code for any problems and compatibility by running on Windows, Linux and Mac.
3. On the README document (displayed on the repo in github) are the 'build' badge icons - these indicate that all tests are running correctly in the cloud.
4. The second build badge: [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/aquaMetrics/rict?branch=master&svg=true)](https://ci.appveyor.com/project/ecodata1/rict/branch/master) links to the Appveyor website which builds the package on Windows.
5. Click this icon and go to the Appveyor website, if the package is successfully built, select the 'Artifacts' tab. Or open [Artifacts](https://ci.appveyor.com/project/ecodata1/rict/branch/master/artifacts) page to go straight there.
6. Within the Artifacts tab download the Windows binary package (e.g. rict-1.0.0.zip).
7. Next, on your local file system, create an empty folder called `support-files` and then zip (compress) this folder to give: `support-files.zip`.
8. Add the rict-1.0.0.zip into the `support-files.zip` folder.
9. Within Azure, upload the `support-files.zip` - this will replace the pre-existing `support-files.zip`.
10. Connect the new version of the support-files.zip to the experiment needing to be updated.

#### All Done! ☺

#### Notes on Coding Style & Testing

1. Add comments as required to explain the *why* rather than the *how*.
2. Not every line needs comments - but any large or unusual sections.
3. [R Package](http://r-pkgs.had.co.nz/) guidance is a very useful resource for writing packages.
4. Linting / styling code is not enforced but easy to do.
5. Install styler package.
6. Run styler to from 'Addins' menu.
7. Testing allows check coding 'style' with rules from [lintr](https://github.com/jimhester/lintr) package.
8. But linting/style is not enforced (so failures are okay but not desired).
9. Document code and follow standard CRAN checks and file structure `devtools::check()`.
10. Run tests locally - before commit `devtools::test()`.
11. See .lintr file for exact linting rules applied during testing.

