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

One short digression: it's not uncommon to have multiple copies of the same code running
at the same time, something Kubernetes handles with `ReplicaSet`s. Their job is to try their best
to make sure that the specified number of `Pod`s are running at any given time; how do `ReplicaSet`s
identify which `Pod`s they need to manage? They use labels and this is how we picked the label
that we needed to remove. We're essentially telling to the `ReplicaSet` to forget about that particular
`Pod`. Does it mean that now we might have caused a degradation of the perfomance in production?
The answer is no because `ReplicaSet` will notice that a `Pod` suddently "disappeared" 
and it will create a new `Pod`.

Consumers in the outside world are agnostic to the fact that there are multiple replicas running
at the same time and they send requests to a single endpoint, a load balancer will then pick
which replica should handle a request. Kubernetes uses the `Service` abstraction to model
components like load balancers, and how does a `Service` identify the replicas it needs to
send requests to? The answer is once again labels - typically the same labels used by the
`ReplicaSet`. That means that by removing that label, we effectively prevented the `Pod` 
from handling requests coming from outside world: this is good because we don't necessarely
want it to handle requests (remember, it's running an ad-hoc modification of the backend code)
but wow you might ask: how can we send a request to the hijacked `Pod` now if it cannot be reached?

Kubernetes to the rescue!
Using [port forwarding](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands#port-forward)
we can forward a port in our `localhost` to a port that the hijacked `Pod` exposes

```bash
kubectl port-forward pod/<pod-name> <local-port>:<remote-port>
```

and than hit that endpoint in our local machine to send a request to the `Pod`. 
For example we can execute `curl http://localhost:<local-port>/<fix-db>` to hit an endpoint
that will run the code that will fix the problem.

Which remote port should you pick? The easiest way is to look at how the `Pod` is configured and
find which port(s) it exposes

```bash
# Look for Containers.<container-name>.Ports
kubectl describe pod/<pod-name>
```

Once we checked that the fix has been successfully applied we won't need the hijacked `Pod` 
anymore and we can simply remove it 

```bash
kubectl delete pod -l hijacked=true
```

## Final thoughts and credits

Hijacking a production environment for good was first suggested to me by my former colleague 
[Daniel Gorin](https://github.com/jcpetruzza/) and it turned out to be extremely handy in 
fixing one-off bugs or reconciling state in the database.
That said, it should be used parsimoniously as it is tempting to use it to bypass the regular 
deployment process, which is potentially a sign that the deployment process is too slow or unreliable.
