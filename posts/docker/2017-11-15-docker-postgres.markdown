---
title: Connecting to a dockerised postgres instance via psql using user-defined bridge networks
author: futtetennista
tags: docker, postgres, snippet
---

Docker containers can commmunicate with each other either using the deprecated
links machinery or using user-defined networks. The latter also is the way to go
when using `docker-compose` since a user-defined network is created by default
(at least in recent versions).
<!--more-->

The postgres official image on [Docker Hub](https://hub.docker.com/_/postgres/)
contains an example for the former case: when creating the container running the
`psql` command using the default `bridge` network interface, we need to supply
the `--link` option with the name (and optionally alias) of the postgres instance
and the `--host` option with its **alias** (if no alias is supplied the name is
also the alias):

```
$ docker run -it --rm --link=postgres:postgres-instance postgres psql --host=postgres-instance --username=postgres
```

There is no example for the latter case though, so let's see how it can be achieved.
First let's create a user-defined network and give it an arbitrary name - i.e.
`my_bridge` - by simply typing: `docker network create my_bridge`. When using
user-defined networks is essential to know the **IP** of the container
running the postgres instance, this can be retrieved like this:

```
$ docker run -itd --name=postgres-instance postgres # Make sure there's an instance running
$ docker inspect --format='{{ .NetworkSettings.Networks.my_bridge.IPAddress }}' postgres-instance
172.19.0.2
```

Now let's create another container that connects to the postgres instance and runs
the `psql` command, and use the configuration options to set the user-defined network
and to provide the IP of the postgres instance the container has to connect to:

```
$ docker run -it --rm --network=my_bridge postgres psql --host=172.19.0.2 --username=postgres
```

Alternatively, if we leverage the `--add-host` option, the command is going to
look very similar to the one we used when connecting containers using links:

```
$ docker run -it --rm --network=my_bridge --add-host=postgres-instance:172.19.0.2 postgres psql --host=postgres-instance --username=postgres
```

The `--add-host` option simply adds an entry to the `/etc/hosts` file of the
container.
