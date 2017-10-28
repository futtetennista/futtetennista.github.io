---
title: "Wait for it…a deep dive in Espresso's idling resources"
author: futtetennista
tags: android, espresso, testing, archive
---

##### Update: A new (better) version of this post is available on the [Jimdo Dev Blog](http://dev.jimdo.com/2014/05/09/wait-for-it-a-deep-dive-into-espresso-s-idling-resources/).


Recently I invested a decent amount of time in making our functional tests less clunky, especially when there are async computations involved. We started using [Espresso][espresso] a few days after it was released and never looked back. In this blog post I'd like to focus on how you can tell Espresso to wait for an async computation to finish before performing any actions on a `View`, and a few gotchas I learned.
<!--more-->
Espresso introduces the concept of [`IdlingResource`][ir], a simple interface that

> Represents a resource of an application under test which can cause asynchronous background work to happen during test execution

The interface defines three methods:

* `getName()`: must return a non-null string that identifies an idling resource. Morover, as the docs state:

> it is used for logging and *idempotency* of registration

* `isIdleNow()`: returns the current idle state of the idling resource. If it returns `true`, the `onTransitionToIdle()` method on the registered `ResourceCallback` must have been previously called.
* `registerIdleTransitionCallback(IdlingResource.ResourceCallback callback)`: normally this method is used to store a reference to the callback to notify it of a change in the idle state.

## Idling resource registration
Registering an idling resource is really simple: just call `Espresso.registerIdlingResource(myIdlingResource)`. This call is idempotent, meaning that
> it can be applied multiple times without changing the result beyond the initial application.

This way consequent calls to `Espresso.registerIdlingResource(myIdlingResource)` for an idling resource with the same name won't have any effect (Espresso will simply log a warning). Generally this is no big deal, but it becomes an issue if an idling resource has a dependency to the current `Context`. For example, the application under test can have a `WebView` and the tests need to wait for a page to be fully loaded. If idempotence is not taken into account and an idling resource with a reference to a `WebView` instance is registered - for example in the `setUp()` method of a test class - bad things will happen. First, subsequent tests will rely on a wrong referenced component in idling resource to be checked and will probably fail, and second the first `Context` is leaked since we're holding a strong reference to it. The solution to that is to have an `ActivityLifecycleIdlingResource` and inject and clear the reference to a component when appropriate.
```java
abstract class ActivityLifecycleIdlingResource<T> implements IdlingResource {
  private T component;

  void inject(T component) {
    this.component = component;
  }

  void clear() {
    this.component = null;
  }
}
```

Another - probably less error-prone - solution would be to have an `Espresso.unregisterIdlingResource(myIdlingResource)` API, there is already a [feature request][fr] to add it. As for registering idling resources that are needed in all tests, I ended up registering them in the `callApplicationOnCreate(app)` method of a custom `InstrumentationTestRunner`, this way I am sure the registration happens only once.

## Implementing an idling resource for a thread pool executor
There can be [multiple][efficient-android-threading] [reasons][rx-java-android] why you'd want your application to not use the built-in Android components that handle async operations, in this case you'd need to define an idling resource that checks if the executor(s) used by the application are idle. Looking at the Espresso source code, with a small refactoring to the [`AsyncTaskPoolMonitor`][atpm] class (Espresso uses it to check if there is some tasks running on the `AsyncTask` thread pool executor) a general [`ThreadPoolIdlingResource`][tpir] can be implemented.

## Soundtrack:
<iframe width="100%" height="166" scrolling="no" frameborder="no" src="https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/141161837&amp;color=ff5500&amp;auto_play=false&amp;hide_related=false&amp;show_artwork=true"></iframe>


[espresso]: https://code.google.com/p/android-test-kit/
[ir]: https://android-test-kit.googlecode.com/git/docs/javadocs/apidocs/index.html
[fr]: https://code.google.com/p/android-test-kit/issues/detail?id=65
[atpm]: https://code.google.com/p/android-test-kit/source/browse/espresso/lib/src/main/java/com/google/android/apps/common/testing/ui/espresso/base/AsyncTaskPoolMonitor.java
[tpir]: https://gist.github.com/stefanodacchille/9995163#file-threadpoolidlingresource-java
[rx-java-android]: http://mttkay.github.io/blog/2013/08/25/functional-reactive-programming-on-android-with-rxjava/
[efficient-android-threading]: http://www.slideshare.net/andersgoransson/efficient-android-threading
