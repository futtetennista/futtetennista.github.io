---
title: Connecting to a postgres instance with user-defined bridge networks
author: futtetennista
tags: docker, postgres, snippet
---

Docker containers can commmunicate with each other either using the deprecated
links machinery or using user-defined networks.
<!--more-->
The postgres official image on [Docker Hub](https://hub.docker.com/_/postgres/)
contains an example for the former case, the container is using the default
`bridge` network interface, supplying the `--link` option with the name (and
optionally alias) of the posgres server instance and the host to the `psql`
command - the **alias** has to be used, keeping in mind that if no alias is
supplied the name of the container is also its alias:

```
$ docker run -it --rm --link=postgres:postgres-instance postgres psql --host=postgres-instance --username=postgres
```
There is no example for the latter case though, so let's see how it can be achieved.
When using user-defined networks is essential to know the **IP** of the container
running the postgres server, this can be retrieved with:

```
$ docker run -itd --name=postgres-instance postgres
$ docker inspect --format='{{ .NetworkSettings.Networks.my_bridge.IPAddress }}' postgres-instance
172.19.0.2 # the IP to pass as the host to psql
```
Here `my_bridge` is an arbitrary name given to the user-defined network. Now let's
run the `psql` command to create another container, configuring it to use the
user-defined network and providing the IP of the postgres server it has to connect
to:

```
$ docker run -it --rm --network=my_bridge postgres psql --host=172.19.0.2 --username=postgres
```
Alternatively, leveraging the `--add-host` option, the command looks very similar
to the one used when connecting containers using links:

```
$ docker run -it --rm --network=my_bridge --add-host=postgres-instance:172.19.0.2 postgres psql --host=postgres-instance --username=postgres
```
Here the `--add-host` option simply adds an entry to the `/etc/hosts` file of the
container.
