---
title: "ignition v0.1 - the ignited way of doing location"
tags: android, ignition, location, archive
---


Today as part of our Fun Friday we released version 0.1 of [ignition](https://github.com/mttkay/ignition), an Android library that should make your life as an Android developer much less painful. What I'd like to write about here is the module I focused on, that is the ignition-location module. Personally I started working with Android almost 3 years ago, in Android terms that means Android v1.5 - Cupcake - API level 3. It wasn't easy to understand the framework back then, lots of documentation was missing and I spent hours digging in the source code to understand of things were supposed to work. A lot has changed since 1.5, and developing Android applications has become way easier with better documented APIs and better tools.
<!--more-->
It's a bit weird that on Android I always worked on location-aware applications, and sadly if you have to deal with location your life is not that simple. I was literally thrilled when I watched [Reto Meier's talk on the last Google I/O](http://www.google.com/events/io/2011/sessions/android-protips-advanced-topics-for-expert-android-app-developers.html), read his blog posts - on the [Android Developers Blog](http://android-developers.blogspot.com/2011/06/deep-dive-into-location.html), and on his personal blog ([1](http://blog.radioactiveyak.com/2011/06/how-to-build-location-based-apps-that.html), [2](http://blog.radioactiveyak.com/2011/06/deep-dive-into-location-part-2-being.html)), and had a look at his [android-protips-location](http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cts=1331302043597&ved=0CCcQFjAA&url=http%3A%2F%2Fcode.google.com%2Fp%2Fandroid-protips-location%2F&ei=mA5aT8uzOYHStAaryvGZDA&usg=AFQjCNEr7Ee0hLkpQ4-fC8eNw1sEAVqDkA) project - because he showcased some really cool techniques and gave solutions to a lot of issues that I personally experienced. One issue about the sample project is that it's not intented to be used as an Android library, so there is no easy way to integrate and all that code, and it's a lot of code (but fair enough, that project wasn't intented to be an Android library). So I started to think how that code could be reused.

At that time I was working on integrating Analytics in the [Qype application](https://play.google.com/store/apps/details?id=com.qype.radar)([here](http://prezi.com/swcxu2ynsio-/implementing-analytics-with-aspectj/)'s a prezi of a barcamp session I did during the last DroidCon in London) using [AspectJ](http://en.wikipedia.org/wiki/AspectJ), and I really liked its unintrusiveness. That's when I thought: it would be awesome if I wouldn't had to be worried about enabling/disabling location updates, battery consumption and other details while I'm developing a location-aware application, I just want to know the most recent known location and I don't care about all those things! In other words, the logic that handles location should be a separate concern with respect to the logic of the application (i.e.: getting the best places nearby, the nearby checked-in friends etc.).

After spending several months adapting the [android-protips-location](http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cts=1331302043597&ved=0CCcQFjAA&url=http%3A%2F%2Fcode.google.com%2Fp%2Fandroid-protips-location%2F&ei=mA5aT8uzOYHStAaryvGZDA&usg=AFQjCNEr7Ee0hLkpQ4-fC8eNw1sEAVqDkA) project and adding a couple of features so that it could be used as an Android library, here is the final result:

	@IgnitedLocationActivity
	public class IgnitedLocationSampleActivity extends MapActivity {
	    // MUST BE OVERRIDDEN OR IGNITION LOCATION WON'T WORK!
	    @Override
	    public void onCreate(Bundle savedInstanceState) {
	        super.onCreate(savedInstanceState);
	        ...
	    }

	    // MUST BE OVERRIDDEN OR IGNITION LOCATION WON'T WORK!
	    @Override
	    public void onResume() {
	        super.onResume();
	    }

	    // MUST BE OVERRIDDEN OR IGNITION LOCATION WON'T WORK!
	    @Override
	    public void onPause() {
	        super.onPause();
	    }

	    @Override
	    public boolean onIgnitedLocationChanged(Location newLocation) {
	        ...
	        return true;
	    }
	    ...
}

Et voilà ! That's all the code you have to write in your location-aware Activity! Under the hood the ignition-location library will:

* enable/disable location updates
* enable/disable passive location updates
* return the most recent location to your application (using the `onIgnitedLocationChanged()` callback or a Location <u>field</u> with the `@IgnitedLocation` annotation)
* avoid using GPS if the battery level is too low
* automatically switch off GPS and request network updates if a location fix is not returned after a certain interval
without the need for you to do anything else apart from adding the `@IgnitedLocation`Activity annotation to your Activity and make sure you override `onCreate()`, `onResume()` and `onPause()`. Many configuration parameters can be passed to the `@IgnitedLocation` Activity annotation to have fully customized settings.

If Eclipse is your IDE, additionally to your Android setup you must:

1. download and install the [ajdt](http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cts=1331303317995&ved=0CCUQFjAA&url=http%3A%2F%2Fwww.eclipse.org%2Fajdt%2F&ei=lBNaT6D6BofhtQb2wvH6Cw&usg=AFQjCNFDAcExHcXf900Zhjp4pcFF0VexHg)
2. add ignition-location as a library project (have a look [here](http://developer.android.com/guide/developing/projects/projects-eclipse.html#ReferencingLibraryProject) for instructions)
3. add ignition-location to the aspect path (have a look at the image below)


If you are using Maven as your build manager you have to:

1. add ignition-location as a dependency

	<dependency>
	  <groupId>com.github.ignition</groupId>
	  <artifactId>ignition-location</artifactId>
	  <version>0.1</version>
	  <type>apklib</type>
	  <exclusions>
	    <exclusion>
	      <groupId>com.google.guava</groupId>
	      <artifactId>guava-collections</artifactId>
	    </exclusion>
	  </exclusions>
	</dependency>

2. add the aspectj-maven-plugin to your POM file, adding the ignition-location library to the aspect path

	<plugin>
	  <groupId>org.codehaus.mojo</groupId>
	    <artifactId>aspectj-maven-plugin</artifactId>
	    <configuration>
	      <aspectLibraries>
	        <aspectLibrary>
	          <groupId>com.github.ignition</groupId>
	          <artifactId>ignition-location</artifactId>
	          <type>apklib</type>
	        </aspectLibrary>
	      </aspectLibraries>
	      <source>1.6</source>
	    </configuration>
	    <executions>
	      <execution>
	      <!-- phase need to be before compile, or the build will fail. More info here: http://stackoverflow.com/questions/2610633/maven-compile-aspectj-project-containing-java-1-6-source -->
	      <phase>process-sources</phase>
	      <goals>
	        <goal>compile</goal>
	      </goals>
	    </execution>
	  </executions>
	</plugin>


One last good thing about it is that it's open source and on [github](https://github.com/mttkay/ignition)! So fork it and help us make it better.



NOTE:

If you are wondering if the ignition-location magic will slow your application down, the simple answer is: no. Reflection is not used, AspectJ's [Context binding](http://stackoverflow.com/questions/7646097/what-is-aspectj-context-binding) is used instead. That means that all the magic is happening at compile time. Isn't that cool?
