---
title: Deploying a Hakyll website using Github Pages and CircleCI 2.0
tags: hakyll, docker, circleci
---

One of the last things left to figure out when I was about to lauch this website
was finding a workflow to nicely deploy it.
I was using Jekyll + Github Pages for my old website and it was working well enough
for me so I didn't want to radically change the way I was doing things.
On the other hand I didn't update my old website in a while and I am new to
Hakyll so I had to figure out if I could keep a similar workflow. I ended up
spending a few hours figuring out a solution I was happy with and the following
is a description of my present workflow and how I got to it.
<!--more-->
On the Hakyll website there are a [few](https://www.stackbuilders.com/news/dr-hakyll-create-a-github-page-with-hakyll-and-circleci)
[blog](http://kyle.marek-spartz.org/posts/2013-12-09-widely-and-hakyll.html)
[posts](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html) on the
topic. I initially blindly followed the [one](http://kyle.marek-spartz.org/posts/2013-12-09-widely-and-hakyll.html)
using [CircleCI](https://circleci.com/) but I wished there was a simpler solution
not involving git submodules; on the other hand the [other](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html)
was simple but focused developing on a local machine. I wished I could find
a solution that combined both and I ended up finding it thanks to the fact that
CircleCI rolled out their [2.0 release](https://circleci.com/docs/2.0/).

### Setting up CircleCI 2.0
Configuration files in CircleCI 2.0 are quite different from 1.0 so I couldn't
reuse much of the code in this [post](https://www.stackbuilders.com/news/dr-hakyll-create-a-github-page-with-hakyll-and-circleci)
but I found the docs quite good and the whole configuration options quite
intuitive. CircleCI 2.0 adds great support for Docker, so I ended up creating
a custom [docker image](https://hub.docker.com/r/futtetennista/hakyll/) for my website
after trying the [official Haskell image](https://hub.docker.com/_/haskell/)
and stumbling upon two main issues:

1. `ssh` isn't installed by default: this is a problem when checking out or pushing
to a remote git repository (checkout actually works somehow thanks to some tricks
CircleCI does but it logs a warning in its console)
2. `make` isn't installed by default: my website uses [hakyll-sass](https://github.com/meoblast001/hakyll-sass/)
that has a C++ dependency - `libsass` - that needs to be built

After that the project was building but it was compiling all dependecies.
Building a site from scratch takes quite a bit - ~20 minutes on my local machine
and ~12 minutes in CircleCI - so it's critical to use CircleCI's
[caching](https://circleci.com/docs/2.0/caching/) to speed things up.
The `save_cache` and `restore_cache` job-level keys are the ones to
configure in order to speed up the build, this is how the caching section looks
for my project:

```yaml
  - restore_cache:
      key: v1-stack-{{ checksum "futtetennismo.cabal" }}
  ...
  - save_cache:
      paths:
        - ~/futtetennismo/.stack-work
        - /root/.stack/
      key: v1-stack-work-{{ checksum "futtetennismo.cabal" }}
```

It's very simple: just let CircleCI know that it should cache and how the cache
should be named in order to be retrieved at a later time. The improvements on build
time are dramatic: from ~20 minutes in case of build with no cache to ~2 minutes in
the worst case (when a new cache archive needs to be created and uploaded) to ~30
seconds in the average case!

##### TL;DR
Here's something to keep in mind when caching in CircleCI 2.0

> The cache for a specific key is immutable and cannot be changed once written.

In early experiments the `save_cache` job-level key in my `config.yml` looked like this

```yaml
  - save_cache:
      paths:
        - ~/futtetennismo/.stack-work
      key: stack-work-{{ checksum "futtetennismo.cabal" }}
```

but every new build was again taking a long time because the cache was mostly useless.
Then I added `root/.stack` to the `paths` but still nothing, the cache was just a
few under KB. At that point I noticed this tip in the docs:

> Tip: Given the immutability of caches, it might be helpful to start all your
> cache keys with a version prefix v1-... . That way you will be able to regenerate
> all your caches just by incrementing the version in this prefix.

That meant that `save_cache` never overwrites an existing cache! I ended up building a
new cache following that tip.

##### SSH keys
The missing piece in the puzzle is now setting up ssh key in CircleCI to be able to
checkout the project and push new versions of the website. I ended up creating a
read-write deployment key for pushing to github and a checkout key for fetching
from github. The latter is just a matter of a few clicks, the former involves
some manual work but the docs by the Github folks are easy to follow.

###### Bonus goodness
One feature that CircleCI provides (and which proved to be extremely handy when
debugging some issues setting up ssh keys) is the ability to connect to a
running container via ssh. Bravo to the CircleCI folks!

### Workflow
The workflow I ended up adopting is a mix of [this](http://kyle.marek-spartz.org/posts/2013-12-09-widely-and-hakyll.html)
[two](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html) tutorials.
The `master` branch - which is the one deployed by Github Pages in case of a user
page - contains all the static assets of the website like images, html, css etc.
Another branch - that I arbitrarily named `source` - contains all source code
and each time a new commit is pushed to that branch CircleCI will build a
version of my website and will eventually push it to Github (if the build succeeds).
For convenience, I also configured my build in such a way that CircleCI builds
*only* that branch. Here's a snippet of the `deploy` job-level key in the `config.yml`:

```yaml
version: 2
jobs:
  build:
    working_directory: ~/futtetennismo
  branches:
    only:
      - source
  ...
  - deploy:
      name: Deploy master to Github Pages
      command: |
        git config --global user.email robots@circleci.com
        git config --global user.name CircleCI
        stack exec site rebuild
        git checkout master
        git pull --rebase
        # Overwrite existing files with new files
        cp -a _site/. .
        #  Commit
        git add --all
        git commit -m "[`date '+%F %T %Z'`] New release"
        # Push
        git push origin master:master
```

As a last little bonus, I wrote a simple `pre-push` hook to open my browser
and follow the deployment:

```
#!/usr/bin/env sh

if [ $(git rev-parse --abbrev-ref HEAD) == 'source' ]
then
    open "https://circleci.com/gh/futtetennista/futtetennista.github.com"
fi
```

### Wrapping up
I described a workflow to be able to deploy websites built with Hakyll in a
fully automated fashion using Github Pages and CircleCI 2.0 and I pointed out
some of the gotchas I learned in the process. The code is open source and can
be found on [Github](https://github.com/futtetennista/futtetennista.github.com).
