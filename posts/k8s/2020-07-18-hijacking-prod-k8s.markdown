---
title: Hijacking production with Kubernetes
author: futtetennista
tags: k8s, docker
---

In the best of the worlds deploying your backend will take minutes. If that's not
the case and you need to fix something that is wrong in production, you might be
in trouble. Or maybe you hit an edge case that your code doesn't handle properly
and you need to write a fix just for that particular case and throw that code
away after that. Again ideally you'd go through whatever workflow you use but
in rare occasions being able to bypass all that is really handy or perhaps even
necessary.

In this post we'll explore how to do this with help of Docker and Kubernetes.
<!--more-->
After the code has been changed and tested locally, the first thing to do is to
create a Docker image and push it; once that image can be pulled by the production
Kubernetes cluster, we'll typically need to swap the Docker image of one of the pods
in the service we need to hijack and hit a particular endpoint that will run the code.

Using `kubectl` we can update the labels of a given pod to hijack it

```bash
# Add a label to make it obvious that this pod is hijacked
# and remove the ones that make it ??? to a replica set if
# that is the case
kubectl label pods <pod-name> hijacked=true <label-to-rm>-
```

edit its manifest to update the Docker image used by the pod
```bash
KUBE_EDITOR=’vim’ kubectl edit pod/<pod-name>
# Find the Docker image in the manifest and update its tag
```

save and wait for Kubernetes to update the `Pod`, which usually happens in a matter
of seconds.

One short digression: if the pod was managed by a `ReplicaSet` - which is how Kubernetes
tries to make sure that there are a specified number of copies of a service running
at any given time - Kubernetes will notice that a pod suddently "disappeared" because
the labels on that pod changed and it will create a new pod to replace it. That means
that production performance won't be affected by the hijacking.

Typically a `Pod` is not run in isolation but is managed by a `Deployment` which has a
stable IP that can be used to reach a particular endpoint - `Pod`s are ephemeral and
Kubernetes decides how to handle them based on the cluster configuration - and can act
as a load balancer: now this is a problem because when we send a request to an endpoint
managed by a `Deployment`, the `Pod` that will handle the request is not know ahead of
time. Kubernetes to the rescue! Kubernetes supports port forwarding for `Pod`s and we
can simply type

```bash
kubectl port-forward pod/<pod-name> <local-port>:<remote-port>
```

have a port in our `localhost` to be forwarded to the port to which the remote process
is listening. Now we can for example `curl localhost:<local-port>/<my-api>` to make the
hijacked `Pod` handle that request.

Once we checked that the fix has been successfully applied, we can simply remove the `Pod`
with `kubectl delete pod -l hijacked=true` from the cluster.
