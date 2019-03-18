# Contributing guidelines

## Introduction

### Pleased to see you here!

First off all, thank you for considering contributing to [pacta]. It's people like you that make this project better.

### Why we have guidelines

Following these guidelines helps to communicate that you respect the time of the developers managing and developing this project. In return, they should reciprocate that respect in addressing your issue, assessing changes, and helping you finalize your pull requests.

### What you can contribute

There are many ways to contribute, from writing tutorials, improving the documentation, submitting bug reports and feature requests or writing code which can be incorporated into [pacta](https://github.com/2DegreesInvesting/pacta) itself.

## Ground Rules

### About behavior

This project and everyone participating in it is governed by the [Code of Conduct](https://www.contributor-covenant.org/version/1/4/code-of-conduct.html). By participating, you are expected to uphold this code.

### About content

All significant changes in code should start as an [issue](https://github.com/2DegreesInvesting/pacta/issues). Changes in code go hand in hand with updated tests and documentation. Change acceptance procedure is based on pull-requests. Only changes that do not break product are accepted.

## So how do I do it

### Report a bug or request a feature

* Ensure the bug / feature request was not already reported by [searching all issues](https://github.com/2DegreesInvesting/pacta/issues?&q=is%3Aissue).

* If you're unable to find an issue addressing the problem, [open a new one](https://github.com/2DegreesInvesting/pacta/issues/new). Be sure to include a title and clear description, as much relevant information as possible including screenshot, code sample, executable test case demonstrating the issue.

### Update documentation

_To be defined later_

### Make fix or change in code

If you have found a bug and want to fix it or going to implement new feature, please make sure first it is reported in the list of [issues](https://github.com/2DegreesInvesting/pacta/issues). Process of making changes in code is described in details in [Our workflow](#our-workflow) section, which you can find below.

## Our workflow

In our work we stick to [GitHub workflow](http://scottchacon.com/2011/08/31/github-flow.html) as a simple and efficient model.

### 1. Create a branch

If this is something you think you can fix or improve, then clone the project and create a branch with a descriptive name. Remember to assign issue to yourself before you start working on it.

A good branch name consists of issue number and its title: forexample number `123` and title `Add labels to chart` lead to branch name `123-add-labels-to-chart` as it's shown in the example below:

```sh
git clone https://github.com/2DegreesInvesting/pacta.git
git checkout -b 123-add-labels-to-chart
```

Branch name starts with issue number followed by issue summary, all capital letters put into lower case and all spaces replaced with `-`.

### 2. Implement your fix or feature

At this point, you're ready to make your changes! Feel free to ask for help - everyone is a beginner at first :smile_cat:.

While you are doing small changes (steps) on your way try to make atomic commits that represent logical blocks of change - having many commited changes instead of one huge commit.

```sh
git add <file>
git commit -m "<your-commit-message>"
git push origin HEAD
```

### 3. View your changes in action

Make sure to take a look at real results by running process locally. Execute our standard or your customized workflow script from `./Scripts` directory.

### 4. Get the style right

In our work we follow the guidelines described in [R Packages](http://r-pkgs.had.co.nz/r.html#style) book. Please make sure that your development environment has proper settings and new code styled well.

### 5. Make a Pull Request

Update your feature branch from to current state of remote master, and push it!

```sh
git pull --no-edit origin master
git push origin HEAD
```

Finally, go to GitHub and [make a Pull Request](https://github.com/2DegreesInvesting/pacta/compare) :D

### 6. Keeping your Pull Request updated

Sometimes a lot of code in `master` might have changed during pull requwst review process, and that you need to update your branch so it's easier to merge.

Here's the suggested workflow:

```sh
git pull --no-edit origin master
git push origin HEAD
```

### 7. Merging a PR (maintainers only)

A PR can only be merged into master by a maintainer if:

* It has been approved by at least 1 maintainer. If it was a maintainer who opened the PR, only one extra approval is needed.
* It has only requested changes.
* It is up to date with current master.

Any maintainer is allowed to merge a PR if all of these conditions are met.
