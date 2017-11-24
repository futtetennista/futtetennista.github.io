---
title: Creating slim Docker images for Haskell applications
author: futtetennista
tags: docker, haskell, snippet
---

Providing an application as a [Docker executable image](https://www.infoq.com/articles/docker-executable-images)
is a handy way to distribute a project: no need to install languages, frameworks
and dependencies. One can just pull a Docker image and run it. It's really that simple.
Docker images can grow wildly in size because they need to install all the dependecies
needed to run the application and this as a user can be quite annoying sometimes:
imagine you want to use a tiny application that solves a very specific problem and
you have to download a 2GB Docker image. It's undesirable. But that's not really
needed: if the application can be packaged as an executable than *only* the
executable can be shipped with the Docker image, typically reducing its size. How
can this be achieved if the application is built in Haskell?

<!-- more -->

I faced the problem I while ago while working on a
[pet-project](https://github.com/futtetennista/ServerlessValidator): the Docker
image was almost 2GB! but the only thing the application was doing was validating
a YAML file. I didn't find a good solution until a few day ago the [feram.io](http://feram.io/)
folks pointed me to this [blog post](https://blog.alexellis.io/mutli-stage-docker-builds/)
(thanks guys!). Multi-stage builds?!…I didn't even know that was possible! So I
got back to my pet-project and see how that would work out
in Haskell (the application in the blog post is written in Go lang).

The final solution I ended up implementing after some painful and time-consuming
trial and error was a bit more involved than what is described in that post but
it was worthy: the final size of the [Docker image](https://hub.docker.com/r/futtetennista/serverless-validator/)
dropped from 2GB to 17.4MB! I first used plain multi-stage builds but that had an
issue: since all the Haskell dependencies have to be compiled, the first part of
the multi-stage build was taking a long time to complete while the second part was
taking only a few seconds. For this reason I ended up splitting the two, basically
going back to the builder patter the blog post mentions; first I built a base image
with all Haskell dependencies already compiled and than used a multi-stage build
to create the final image. Here's the `Dockerfile` for the base image

``` bash
# docker build -t futtetennista/serverless-validator-builder \
#  --file Dockerfile.builder .
FROM haskell:8.0

# Install dependecies needed to compile Haskell libraries
RUN apt-get update && apt-get install --yes \
    xz-utils \
    make

RUN stack --resolver lts-9.14 install base \
    protolude \
    text \
    aeson \
    yaml \
    unordered-containers \
    case-insensitive \
    regex-compat

```

and the one for the executable image

``` bash
# docker build -t futtetennista/serverless-validator .
FROM futtetennista/serverless-validator-builder as builder

WORKDIR "/home/serverless-validator/"

# copy the contents of the current directory in the working directory
COPY . .

RUN stack --resolver lts-9.14 install


FROM fpco/haskell-scratch:integer-gmp

COPY --from=builder /root/.local/bin/serverless-validator /bin/

ENTRYPOINT ["/bin/serverless-validator"]
```

The [fpco/haskell-scratch](https://hub.docker.com/r/fpco/haskell-scratch/) Docker
image was created by a personal Haskell-hero of mine, [Michael Snoyberg](https://twitter.com/snoyberg)
and introduced in this [blog post](https://www.fpcomplete.com/blog/2015/05/haskell-web-server-in-5mb)
a while back. It's a minimal Linux image (~2MB) that can be used as a base image
to run Haskell applications; the image hasn't been updated in two years but it
still works beautifully (there is another Docker image tagged
[snoyberg/haskell-scratch](https://hub.docker.com/r/snoyberg/haskell-scratch/) but
I suppose that has been deprecated). So thanks again to Michael and the FP Complete
folks for solving so many practical problems Haskellers face in their day-to-day
coding!
