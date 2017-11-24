---
title: Building slim Docker images for Haskell applications
author: futtetennista
tags: docker, haskell, snippet
---

Providing an application as a [Docker executable image](https://www.infoq.com/articles/docker-executable-images)
is a handy way to distribute an application: no need to install toolchains, frameworks
and dependencies. One can just pull a Docker image and run it. It's really that simple.
Docker images can grow wildly in size because they need to install all the dependecies
needed to run the application: this as a user can be quite annoying.
Imagine you want to use a tiny application that solves a very specific problem and
you have to download a 2GB Docker image! It's undesirable. And it's actually not
needed: why not shipping only the executable in a very compact Docker image?
How can this be achieved if the application is built in Haskell?

<!--more-->

I faced the problem I while ago while working on a
[pet-project](https://github.com/futtetennista/ServerlessValidator): the Docker
image was almost 2GB(!) but the only thing the application was doing was validating
a YAML file. I didn't find a good solution until a few days ago the [feram.io](http://feram.io/)
folks pointed me to this [blog post](https://blog.alexellis.io/mutli-stage-docker-builds/)
(thanks guys!). Multi-stage builds?!…I didn't even know that was possible! So I
got back to my pet-project and see how that would work out
in Haskell (the application in the blog post is written in Go lang).
The final solution I ended up implementing after some painful and time-consuming
trial and error was a bit more involved than what is described in that post but
it was worthy: the final size of the [Docker image](https://hub.docker.com/r/futtetennista/serverless-validator/)
dropped **from 2GB to 17.1MB - 5MB compressed size!**
I first used plain multi-stage builds but that had an
issue: since all the Haskell dependencies have to be compiled, the first part of
the multi-stage build was taking a long time to complete while the second part was
taking only a few seconds. For this reason I ended up splitting the two, basically
going back to the builder patter the blog post mentions: I first built a base image
with all needed Haskell dependencies compiled and than used a multi-stage build
to create the executable image. The `Dockerfile` for the base image is not that
interesting:

``` bash
# docker build -t futtetennista/serverless-validator-builder --file Dockerfile.builder .
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

It just installs some Linux dependencies and builds the Haskell dependencies.
The one for the executable image is a bit more exciting:

``` bash
# docker build -t futtetennista/serverless-validator .
FROM futtetennista/serverless-validator-builder as builder

WORKDIR "/home/serverless-validator/"

# copy the contents of the current directory in the working directory
COPY . .

RUN stack --resolver lts-9.14 install && \
    strip /root/.local/bin/serverless-validator


FROM fpco/haskell-scratch:integer-gmp

COPY --from=builder /root/.local/bin/serverless-validator /bin/

ENTRYPOINT ["/bin/serverless-validator"]
```

First it compiles and links the executable in the base container, removes some
unwanted piece of data as `man strip` illustrates

> strip removes or modifies the symbol table attached to the output of the assembler
>       and link editor. This is useful to save space after a program has been debugged
>       and to limit dynamically bound symbols.

and finally copies the executable from the base container to the executable container.
The [fpco/haskell-scratch](https://hub.docker.com/r/fpco/haskell-scratch/) Docker
image was created by a personal Haskell-hero of mine, [Michael Snoyberg](https://twitter.com/snoyberg)
and introduced in this [blog post](https://www.fpcomplete.com/blog/2015/05/haskell-web-server-in-5mb)
a while back. It's a minimal Linux image (~2MB) that can be used as a base image
to run Haskell applications; the image hasn't been updated in two years but it
still works flawlessly (there is another Docker image tagged
[snoyberg/haskell-scratch](https://hub.docker.com/r/snoyberg/haskell-scratch/) but
I guess it has been deprecated).

Thanks once again to Michael and the FP Complete folks for solving **so** many
practical problems Haskellers face in their day-to-day coding!
