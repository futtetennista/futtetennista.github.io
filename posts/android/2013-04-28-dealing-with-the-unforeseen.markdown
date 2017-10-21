---
title: "Dealing with the unforeseen"
author: futtetennista
tags: android, legacy
---

It's quite common to invite users to rate an app on Google Play at some point, on one hand it's good to know that your users are happy and on the other it's a good way to attract new users. It's definitely not the only variable in the equation, but I can definitely say that user satisfaction is inversely proportional to the amount of crashes. But bugs are unfortunately something we have to expect as developers, even after testing our apps thoroughly. One thing we probably don't want to do, is to ask a user to rate our app just after a crash since we can be reasonably sure that he's not going to be too happy about it. How can we make sure that this doesn't happen?
<!--more-->
When an uncaught exception is thrown, the [uncaughtException][1] method of the default [UncaughtExceptionHandler](https://developer.android.com/reference/java/lang/Thread.UncaughtExceptionHandler.html) for that Thread is called. Good news is, you can supply your own [UncaughtExceptionHandler](https://developer.android.com/reference/java/lang/Thread.UncaughtExceptionHandler.html) using the [setDefaultUncaughtExceptionHandler][2] setter (this is what [ACRA](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&ved=0CDYQFjAA&url=http%3A%2F%2Fcode.google.com%2Fp%2Facra%2F&ei=kAwBUezwL8_IsgbIvYH4Dg&usg=AFQjCNF1L9fawVNYyKuqqIa7q22TBcL08w&sig2=_FrQnihkSMcsqGUV5XXp1A&bvm=bv.41524429,d.Yms) and I suppose other libraries like [Crittercism](https://www.crittercism.com/) or [BugSense](https://www.bugsense.com/) do under the hood). Here is a little snippet of what we do in the [Qype app](https://play.google.com/store/apps/details?id=com.qype.radar):
```java
	public class CustomUncaughtExceptionHandlerProxy implements Thread.UncaughtExceptionHandler {

	    private static CustomUncaughtExceptionHandlerProxy proxy;

	    private final boolean enabled;

	    private Thread.UncaughtExceptionHandler customUncaughtExceptionHandler;

	    private CustomUncaughtExceptionHandlerProxy(CustomApplication application) {
	        enabled = !application.inDevMode();
	        if (enabled) {
	            // Save a reference to the current default handler.
	            customUncaughtExceptionHandler = Thread.getDefaultUncaughtExceptionHandler();
	            // Set our custom handler as the default one, so that it's first notified when something ugly happens.
	            Thread.setDefaultUncaughtExceptionHandler(this);
	        }
	    }

	    public static void initialize(CustomApplication application) {
	        if (proxy == null) {
	            proxy = new CustomUncaughtExceptionHandlerProxy(application);
	        }
	    }

	    public static CustomUncaughtExceptionHandlerProxy getInstance() {
	        return proxy;
	    }

	    @Override
	    public void uncaughtException(Thread thread, Throwable ex) {
	        if (enabled) {
	            if (!preferences.rateAppDialogAlreadyShown()) {
	                // reset count only if the 'rate app' dialog hasn't been already shown.
	                preferences.resetShowRateAppDialogCountdown();
	            }
	            // Let the default uncaught exception handler do its job.
	            customUncaughtExceptionHandler.uncaughtException(thread, ex);
	        } else {
	            Thread.getDefaultUncaughtExceptionHandler().uncaughtException(thread, ex);
	        }
	    }
	}
```
This is a typical implementation of the [Proxy design pattern](http://en.wikipedia.org/wiki/Proxy_pattern), it's pretty straightforward but here's a little explanation of how it works. Let's say you want to show a "Rate App" dialog after n times the app is opened by the user, you would use a counter and increment it each time the launcher Activity is created and it will show the dialog when the value of the counter is exactly n. But if the app crashes and the dialog is yet to be shown, you may want to reset the counter.

[1]: https://developer.android.com/reference/java/lang/Thread.UncaughtExceptionHandler.html#uncaughtException(java.lang.Thread, java.lang.Throwable)
[2]: https://developer.android.com/reference/java/lang/Thread.html#setDefaultUncaughtExceptionHandler(java.lang.Thread.UncaughtExceptionHandler)
