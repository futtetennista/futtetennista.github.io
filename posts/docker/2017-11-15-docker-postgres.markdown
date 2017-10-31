---
title: Connecting to a postgres instance with user-defined bridge networks
author: futtetennista
tags: docker, postgres, tidbit
---

Docker containers can commmunicate with each other either using the deprecated
links machinery or using user-defined networks.
<!--more-->
In the former case the container is using the default `bridge` network interface
and the host to use when invoking `psql` would be the **alias** of the container,
i.e.:

```
$ docker run -it --rm --link=my-postgres:postgres-instance postgres psql --host=postgres-instance --username=postgres
```
In the latter case the host is **IP** of the container running the postgres
server, i.e.:

```
$ docker run -itd --name=my_postgres postgres
$ docker inspect --format='{{ .NetworkSettings.Networks.my_bridge.IPAddress }}' my_postgres
172.19.0.2 # the IP to pass as the host to psql
$ docker run -it --rm --network=my_bridge postgres psql --host=172.19.0.2 --username=postgres
```
Or alternatively leveraging the `--add-host` option there's no need to change the
options supplied to `psql`:

```
$ docker run -it --rm --network=my_bridge --add-host=postgres-instance:172.19.0.2 postgres psql --host=postgres-instance --username=postgres
```
The `--add-host` option simply adds an entry to the `/etc/hosts` file of the
container.
