---
title: WebView Explorations
author: futtetennista
tags: android, webview, chromium, android+kitkat, archive
---

In the last few weeks I've been playing around with `WebView`s a lot and found out a few interesting differences (not necesserely documented) between the legacy implementation up to Jelly Bean and the brand new Chromium-based one in KitKat. If you don't know what I'm talking about - after all, the API has mostly remained untouched apart some nice additions, more about this in a bit - a couple of useful links by the Google folks are [this](https://developers.google.com/chrome/mobile/docs/webview/overview) and [this](http://developer.android.com/guide/webapps/migrating.html).
<!--more-->
**Disclaimer**: with this post I don't intend to describe all the differences in the API, I just want to highlight some of the problems/solutions I experienced/found by developing an application that uses the `WebView` API extensively.

## Differences in the API implementation

### Hit test result
I discovered this class only recently and found it immensely useful. In the application I'm currently working on, a user can go to a different page in her website by either clicking on a link in the `WebView`, or by clicking on a native list item. These actions have different side effects, in the latter case the app must explicitly request to load a new url but not in the former, otherwise the same page will be loaded twice. The method [`WebView.getHitTestResult()`][ghtr] returns a [`HitTestResult`][htr] object that contains type and url, the type can be used to discover the type of element that has been clicked, it'll be [`WebView.HitTestResult.SRC_ANCHOR_TYPE`][sat] in case of a HTML `a` tag with a http  `src` attribute. The legacy API will return a [`null` result][hit=null] if no supported element in the `WebView` was hit, while the new Chromium-based will always return a [non-`null` result][hit=nnull].

### WebView history
`WebView` keeps track of all visited urls in a data structure called [`WebBackForwardList`][wbfl], this can be retrieved by calling [`WebView.copyBackForwardList()`][cbfl] method, as the method name clearly states it returns a copy of the [`WebBackForwardList`][wbfl] maintained by the `WebView`. In the app I'm currently working on, after calling [`WebView.goBack()`][gb] the app needs to retrieve the previous page url: this can be easily done by calling [`WebBackForwardList.getCurrentItem()`][gci]. The legacy implementation will return the  page displayed *before* going back, while the new implemetation will return the page that will be displayed *after* going back (I didn't go too deep into why that happens though).

### Javascript links
The third one is a little bit more obscure: if a page contains a link like `<a href="javascript:;">...</a>` - which is a quite common thing to do if one wants an element to be correctly rendered as a link, without actually linking to any resource - the Chromium-based implementation will invoke the [`WebViewClient.onPageFinished()`][opf] callback the first time that element is clicked. Not sure about what is happening here, but it's good to be aware of it because if the app is injecting some Javascript when the page finished loading, it could potentially inject it twice and cause some unexpected behaviour (keep reading if you want to know a way to make sure this doesn't happen).

## API additions
* [Remote debugging][rd]: pretty sweet, if you work with `WebView`s a lot you'll love it. The linked resource explains everything in great detail, so I won't say anything. Just try it.

* [`WebView.evaluateJavascript()`][ej]: this method it makes it straightforward to get a result back from injected Javascript, just supply a [`ValueCallback`][vc] when the method is invoked. Something similar can be achieved with the legacy implementation too, but it requires a bit more boilerplate.

## Keep your code sane, encapsulate those API differences
It is not uncommon to have your code cluttered with `if` statements when dealing with different API implementations. A way to avoid it that I find myself using a lot, is to create an adapter interface that hides those differences. In this case, the one I ended up writing looks roughly like this:

```java
public interface IWebViewCompatibility {
  void injectWebView(WebView webView);
  void evaluateJavascript(String script, ValueCallbackAdapter callback);
  boolean httpLinkHit();
  String getPreviousPageUrl();

  // Adapter interface for legacy WebView API
  public static interface ValueCallbackAdapter {
    void evaluateResult(String value);
    String javascriptInterfaceMethodName();
  }
}
```

If the application uses some sort of dependency injection framework - i.e. [Dagger](https://github.com/square/dagger) - we'll have to write only one `if` statement, when the adapter class is instantiated before being injected:

```java
@Module(injects = WebViewFragment.class)
public class WebViewModule {
  ...
  @Provides @Singleton
  public IWebViewCompatibility provideWebViewCompatibility() {
    return SUPPORTS_KITKAT ? new ChromiumWebViewCompatibility()
      : new LegacyWebViewCompatibility();
  }
}
```

That said, let's have a look at how we can get back the result of the evaluation of a piece of Javascript on legacy API. We can leverage the [`JavascriptInterface`][ji] mechanism to add a helper interface to the page that will receive the result of an injected Javascript (my advice: be sure to do this before anything is loaded in the `WebView`, otherwise the content needs to be reloaded in order for the interface to be added to the page). Now let's implement the legacy adapter interface:

```java
public class LegacyWebViewCompatibility implements IWebViewCompatibility {
  private WebView webView;
  private ValueCallbackAdapter callback;

  @Override public void injectWebView(WebView webView) {
    this.webView = webView;
    this.webView.addJavascriptInterface(new LegacyCallbackInterfaceHelper(), NAME);
  }

  @Override
  public void evaluateJavascript(final String script, ValueCallbackAdapter callback) {
    this.callback = callback;
    if (callback != null) {
      String js = String.format("javascript:{var res=%s;%s.%s(res);};void(0);", script, NAME,
        callback.javascriptInterfaceMethodName());
      webView.loadUrl(js);
    } else {
      webView.loadUrl("javascript:{" + script + "};void(0);"));
    }
}
...
  class LegacyCallbackInterfaceHelper {
    static final String NAME = "legacyAndroidCallbackInterfaceHelper";

    @JavascriptInterface @SuppressWarnings("unused") // Called from js
      public void jimdoDefined(String result) {
        ((Activity) webView.getContext()).runOnUiThread(new Runnable() {
          @Override public void run() {
            LegacyWebViewCompatibilityDelegate.this.callback.evaluateResult(result);
          }
      });
    }
  }
}
```
As you can see there's a bit more boilerplate to write, but we managed to achieve the same result. One thing to notice here is that the callback should be run on the main thread - the [`JavascriptInterface`][ji] method runs on a `WebView` thread - otherwise you'll get a nice little warning if you at the logs.

At this point, here's how we can use this to check if a piece of Javascript has already been injected and avoid to inject it twice:

```java
javascriptInjector.injectFunction(screen, "typeof jimdo === \'undefined\'",
  new WebViewCompatibilityDelegate.ValueCallbackAdapter() {

  @Override public void evaluateResult(String jimdoUndefined) {
    if (Boolean.valueOf(jimdoUndefined)) {
      javascriptInjector.injectScript(screen, "my_script.js", null);
    }
  }

  @Override public String javascriptInterfaceMethodName() {
    // This corresponds to LegacyCallbackInterfaceHelper.jimdoDefined()
    return "jimdoDefined";
  }
});
```

First, the app injects a Javascript function that checks if a variable created by the script is already defined, then depending on the returned result the script is injected or not.

## Soundtrack:
<iframe width="660" height="60" src="//www.mixcloud.com/widget/iframe/?feed=http%3A%2F%2Fwww.mixcloud.com%2Fdarkfloor%2Fmantis-radio-150-objekt%2F&amp;mini=1&amp;embed_type=widget_standard&amp;embed_uuid=7b156b59-156d-4204-84bc-1a674fc22a50&amp;hide_tracklist=1&amp;replace=0&amp;hide_cover=1&amp;light=1" frameborder="0"></iframe><div style="clear: both; height: 3px; width: 652px;"></div><p style="display: block; font-size: 11px; font-family: 'Open Sans', Helvetica, Arial, sans-serif; margin: 0px; padding: 3px 4px; color: rgb(153, 153, 153); width: 652px;"><a href="http://www.mixcloud.com/darkfloor/mantis-radio-150-objekt/?utm_source=widget&amp;amp;utm_medium=web&amp;amp;utm_campaign=base_links&amp;amp;utm_term=resource_link" target="_blank" style="color:#808080; font-weight:bold;">Mantis Radio 150 + Objekt</a><span> by </span><a href="http://www.mixcloud.com/darkfloor/?utm_source=widget&amp;amp;utm_medium=web&amp;amp;utm_campaign=base_links&amp;amp;utm_term=profile_link" target="_blank" style="color:#808080; font-weight:bold;">Darkfloor.</a><span> on </span><a href="http://www.mixcloud.com/?utm_source=widget&amp;utm_medium=web&amp;utm_campaign=base_links&amp;utm_term=homepage_link" target="_blank" style="color:#808080; font-weight:bold;"> Mixcloud</a></p><div style="clear: both; height: 3px; width: 652px;"></div>



[ji]: https://developer.android.com/reference/android/webkit/JavascriptInterface.html
[opf]: https://developer.android.com/reference/android/webkit/WebViewClient.html#onPageFinished(android.webkit.WebView,%20java.lang.String)
[rd]: https://developers.google.com/chrome-developer-tools/docs/remote-debugging
[vc]: https://developer.android.com/reference/android/webkit/ValueCallback.html
[hit=nnull]: http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/webview/chromium/WebViewChromium.java#872
[hit=null]: http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.3.1_r1/android/webkit/WebViewClassic.java#7723
[gci]: https://developer.android.com/reference/android/webkit/WebBackForwardList.html#getCurrentItem()
[gb]: https://developer.android.com/reference/android/webkit/WebView.html#goBack()
[cbfl]: https://developer.android.com/reference/android/webkit/WebView.html#copyBackForwardList()
[sat]: https://developer.android.com/reference/android/webkit/WebView.HitTestResult.html#SRC_ANCHOR_TYPE
[ghtr]: https://developer.android.com/reference/android/webkit/WebView.html#getHitTestResult()
[htr]: https://developer.android.com/reference/android/webkit/WebView.HitTestResult.html
[wbfl]: https://developer.android.com/reference/android/webkit/WebBackForwardList.html
[ej]: https://developer.android.com/reference/android/webkit/WebView.html#evaluateJavascript(java.lang.String,%20android.webkit.ValueCallback%3Cjava.lang.String%3E)
