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

Using `kubectl` we can [isolate](https://kubernetes.io/docs/concepts/workloads/controllers/replicaset/#isolating-pods-from-a-replicaset)
a `Pod` by updating its label

```bash
# Add a label to make it obvious that this pod is hijacked and remove the ones
# that match the selector in the corresponding replica set / deployment
kubectl label pods <pod-name> hijacked=true <label-to-rm>-
```

and [update](https://kubernetes.io/docs/concepts/workloads/controllers/deployment/#updating-a-deployment)
its Docker image

```bash
kubectl set image pod/<pod-name> <container-name>=<docker-image> --record
```

and wait for Kubernetes to update the `Pod`, which usually happens in a matter
of seconds.

One short digression: if the pod was managed by a `ReplicaSet` - which is how Kubernetes
tries to make sure that there are a specified number of copies of a service running
at any given time - Kubernetes will notice that a `Pod` suddently "disappeared" because
the labels on that pod changed and it will create a new pod to replace it. That means
that production performance won't be affected by the hijacking.

Now we need to be able to send a request to the hijacked `Pod`, this isn't as straight forward
as it might sound. Typically `Pod`s are not run in isolation, there are multiple copies running
behind a load balancer - a `Service` in Kubernetes - therefore when we sent a request to a given endpoint,
the load balancer decides which `Pod` will handle the request. Kubernetes to the rescue!
Kubernetes supports [port forwarding](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands#port-forward)
for `Pod`s so that we can forward a port in our `localhost` to the remote port o the `Pod`

```bash
kubectl port-forward pod/<pod-name> <local-port>:<remote-port>
```

Now we can for example `curl localhost:<local-port>/<my-api>` to make the
hijacked `Pod` handle that request.

Once we checked that the fix has been successfully applied, we can simply remove the `Pod`
with `kubectl delete pod -l hijacked=true` from the cluster.

## Final thoughts

Hijacking a production environment for good turns out to be really handy in some cases e.g.
fixing one-off bugs or reconciling the state of the database, and it saved the day
in at least a couple of sticky situations.
Keeping that in mind, it should be used parsimoniously and it's probably a sign that the deployment
process should be improved.
