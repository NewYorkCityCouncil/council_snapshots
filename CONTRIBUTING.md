# Contributing to councilsnapshots

This document outlines the contribution and deployment workflow for `councilsnapshots`.

In general, the goal of this workflow is to achieve 3 things:

1. Stability in production
2. Isolation of the development of different features
3. Clear mappings between code and issues

To do this, the following general workflow will be enforced:

1. File an issue (if there is not already one).
2. Create a branch on which to resolve that issue.
3. Write, test, and re-write code locally.
4. Push commit(s) to the remote branch.
5. Ensure Travis builds pass.
6. Ensure all features (even pre-existing ones) are functional in the staging environment.
7. Issue a pull request and request a review.
8. Merge pull request to master.
9. Check Travis builds and main deployment.

Each of these steps is outlined in more detail below.

## Step 1: Issues

Ideally, every feature or fix or improvement should be tied to an issue explaining what the problem is, why it needs to be addressed, and your proposed solution (if you have one). This lets us keep track of the work we've done and the work we still need to do. Additionally, issues provide a convenient place to have threaded discussions about specific issues without cluttering up other channels of communication like trello or email.

If an issue already exists and nobody else is working on it, then this step is already done! Somebody else (maybe even past-you) has gotten the process started.

## Step 2: Branches

Best practice is to create a different branch to work on each new feature, fix, or enhancement. This serves to isolate different pieces of work from one another. For example, if you're working on several issues at once, it might get messy if you switch from one to the other. Maybe a piece of code in your solution to the first issue is broken and now the app won't run, so you can't test code elsewhere that addresses the second issue. The ability to do this kind of parallel work is usually very worth the added complexity of handling multiple branches and all the merge related headaches that come with them. This becomes especially true when more than one person is working on a project.

In this project we will use the following naming conventions for branches:

- Feature branches: {initials}/feat/{description} (e.g. `ns/feat/incident-timeline`)
- Enhancement branches: {initials}/enhc/{description} (e.g. `ns/enhc/vec-labels`)
- Fix branches: {initials}/fix/{description} (e.g. `ns/fix/missing-dates`)

## Step 3: Development

Once you're on your local branch, you're free to experiment and play without fear of breaking things!

This project is structured as an R package, so the development process is a little different than other projects. Here the process is:

1. Add modules to the `R/` directory. Be sure to include documentation using [Roxygen2 comments](https://roxygen2.r-lib.org/).
2. Call the modules in the main app file. This lives in `inst/shinyApp`. In this file the `councilsnapshots` package is loaded and the modules you write are available as functions in that package.
3. Build the package using `devtools`. In RStudio this can be done using Cmd+Shift+B. If you've added or changes Roxygen comments, don't forget to regenerate `.Rd` files using `devtools` as well. The RStudio keyboard shortcut for this is Cmd+Shift+D.
4. Call `run_snapshots()` to launch the app and make sure everything works.
5. Run `R CMD check` to make sure nothing is missing and that the build will pass on Travis. Use Cmd+Shift+E in RStudio to do this.
6. Repeat!

## Step 4: Pushing

The next step is to send your changes to GitHub. Depending on how you created your local branch and whether or not you've pushed your branch before, you can either do this by clicking the "Push" button in the RStudio Git pane, running `git push` in the terminal, or running `git push -u origin <branch-name>`.

## Step 5: Build and deploy

As soon as you push anything to GitHub, the newest version of your code will be sent to Travis for automatic building and deployment to a staging branch. This let's you ensure that everything you've done works in the production environment we're using. What this does is create a virtual machine from scratch that builds and tests the package, then deploys the app to shinyapps.io. Commits on the master branch will be deployed to nycc.shinyapps.io/council_snapshots. Commits on other branches will be deployed to nycc.shinyapps.io/council_snapshots-staging-{branch name} (with non-alphanumeric characters and "-" and "_" removed).

You can see the status of this build on GitHub and detailed information on Travis.

## Step 6: Testing

Once your updates are successfully built and deployed you must run through the *whole* app to make sure nothing is broken inadvertently. The second-to-worst case scenario is merging something broken to master (the only thing worse is breaking the database). This workflow is not ideal and hopefully automated testing will be implemented soon, but for know we have what we have.

## Step 7: Pull requests

Once your update is finished and you've made sure it works it's time to submit a pull request! Do this on GitHub and request at least one review. Pull requests *may not* be merged to master without an approving review except in extraordinary circumstances. Make any changes that need to be made after the review and repeat until everybody is happy!

## Step 8: Merging

Once a pull request is made another set of checks will be run. If you have made changes in response to the review, be sure to check the deployed app at the staging URL. Pull requests *may not* be merged to master without passing builds.

On merge, a new set of checks will be run on the app and then it will be put live for all to see, all automatically!

## Step 9: Final checks

As soon as the new version of the app is live, check it! Verify *everything* works one last time. If something is broken, make a fix branch right away and deploy a fix (via this process) as soon as possible. Feel free to create an issue after the fact for the sake of speed, however.
