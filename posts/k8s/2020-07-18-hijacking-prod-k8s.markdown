---
title: Hijacking production with Kubernetes
author: futtetennista
tags: k8s, docker
---

Imagine having a glitch in your code that caused an entity in the production
environment to be in an inconsistent state that cannot be easily fixed by existing
code. Luckily a couple of lines of code will fix the issue, if only there was a quick
way to ship that code to production making sure it's targeted to that specific entity and 
that can be safely thrown away as soon as the issue is fixed.

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
the labels on that pod changed and it will create a new pod to replace it. Notice that this means
that we shouldn't observe any unwanted effects like performance degradation in production.

This bring us to the question: how can we send a request to the hijacked `Pod` now? Typically 
`Pod`s are not run in isolation, there are multiple copies runningbehind a load balancer 
- a `Service` in Kubernetes - therefore when we sent a request to a given endpoint,
the load balancer decides which `Pod` will handle the request. Kubernetes to the rescue!
Using [port forwarding](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands#port-forward)
we can forward a port in our `localhost` to the remote port of the hijacked `Pod`

```bash
kubectl port-forward pod/<pod-name> <local-port>:<remote-port>
```

You can find the value of `remote-port` by looking at the description of the pod

```bash
# Look for Containers.<container-name>.Ports
kubectl describe pod/<pod-name>
```

Now we can for example `curl localhost:<local-port>/<my-api>` to make the
hijacked `Pod` handle that request.

Once we checked that the fix has been successfully applied, we can simply remove the `Pod`
with `kubectl delete pod -l hijacked=true` from the cluster.

## Final thoughts and credits

Hijacking a production environment for good was first suggested to me by my former colleague 
[Daniel Gorin](https://github.com/jcpetruzza/) and it turned out to be extremely handy in 
fixing one-off bugs or reconciling state in the database. 
That said, it should be used parsimoniously as it is tempting to use it to bypass the regular 
deployment process, which is potentially a sign that the deployment process is too slow or unreliable.

