---
title: "Location on Android...the Qype way"
author: futtetennista
tags: android, location, qype, legacy
---

Some months ago we released the first stable version of ignition. In this post I explaned how the ignition-location library works and how it's possible to include it in an already existing application. This time I'll explain how we integrated it in the [Qype Android](https://play.google.com/store/apps/details?id=com.qype.radar) application.
<!--more-->
A very typical use case when using our app is to look for a place to eat, drink or go party nearby. In order to display the best places around, the app needs to know the user's location, and it needs to know it as quickly as possible - nobody likes to stare at his/her phone for 5 minutes waiting for something to happen right? Ignition-location is pretty good at that but there are a couple of things the Qype app itself must take care of. In particular, when developing our new location manager, we had two requirements in mind:

1. a good trade off between quickness and accuracy is achieved.
2. battery is not drained.

There are two key components in our location logic: one is `QypeLocationManager`, the other `QypeNewLocationHandler`. The former contains all the location logic, in particular it takes care of deciding if a location is *good enough* - I'll go back to this in a bit - and if the app should keep requesting location updates, the latter handles UI-related behaviour. For the sake of completeness, there is a third component which is a small interface called QypeLocationAwareActivity implemented by all our location-aware activities.

So what happens when a new location is received? As you already know from the previous post, `IgnitedLocationManager` invokes the `onIgnitedLocationChanged()` callback in the current `IgnitedActivity`. Since we want our logic to be shared across all out location-aware activities, each of them has a reference to an instance of the `QypeLocationManager` and it invokes a callback on this object each time a new location is returned:

	@Override
	public boolean onIgnitedLocationChanged(Location location) {
	    return locationManager.onNewLocation(this, location, lastSearchedLocation);
	}

This callback has got two locations as parameters: the current location and the location used for the last search. I'll expain in the next few lines why both are needed. The `onNewLocation()` callback is the hearth of our location logic, It checks if it's the first time the current Activity is making a request - in this case the Activity is probably being created for the first time - and, if that's the case, it checks if the current location is not null and if it's "good enough" or not. If it is, it invokes the `onLocationAvailable()` callback (defined in the `QypeLocationAwareActivity` interface) on the `Activity`, that will perform whatever task it needs to perform, i.e. searching the best nearby places. Here is a code snippet:

	public boolean onNewLocation(final QypeLocationAwareActivity context, Location newLocation,
	        Location lastSearchedLocation) {

	    ...

	    // Check if the new location is too old, if so wait for a most recent location.
	    if (lastSearchedLocation == null) {
	        // Delete any pending API request with an old location and send a new request right
	        // away using the most up-to-date location.
	        if (locationHandler
	                .hasMessages(QypeNewLocationHandler.MSG_SEND_REQUEST_USING_UNRELIABLE_LOCATION)) {
	            locationHandler
	                    .removeMessages(QypeNewLocationHandler.MSG_SEND_REQUEST_USING_UNRELIABLE_LOCATION);
	        }
	        Activity activity = (Activity) context;
	        // Wait some time if the location is too old, but if a new fix doesn't arrive send the
	        // request using the location we've got.
	        if (isOldLocation(newLocation)) {
	            DialogSupport.safeShowDialog(activity, R.id.ign_loc_dialog_wait_for_fix);
	            locationHandler.sendEmptyMessageDelayed(
	                    QypeNewLocationHandler.MSG_SEND_REQUEST_USING_UNRELIABLE_LOCATION,
	                    INTERVAL_SEND_REQUEST_USING_UNRELIABLE_LOCATION);
	        } else {
	            Dialog waitForLocationDialog = context.getWaitForLocationDialog();
	            if (waitForLocationDialog != null && waitForLocationDialog.isShowing()) {
	                activity.dismissDialog(R.id.ign_loc_dialog_wait_for_fix);
	            }
	            context.onLocationAvailable();
	        }
	}

So when is a location *good enough*? In this specific case, that simply means fresh enough. You might ask: so it doesn't matter if the location is too coarse? Well...yes and no. Keep in mind the requirements in first point above, the app should return results as quickly as possible. Moreover in a densely populated area, i.e. a city, a location returned by a coarse provider, i.e. the the network provider, is often a good approximation of the current location. I'll explain in a bit when accuracy is taken into account.

Going back to the `onLocationAvailable()` callback, what happens if the location is not *good enough*? A couple of possible approaches are: waiting for a better location or using this location anyway. Our approach is somewhere in the middle: the app waits for a certain amount of time, 5 seconds, for a new location. If no new location is received, the app sends an API request using the old location. This is a tricky situation: on one hand we don't want our users to wait too much before they can get some results, but we'd like to make sure that they get the best results we can give them. In this particular case we decided to take an optimistic approach and the app sends a request anyway, it can be that a user didn't move too much from the last known location saved by the location manager. But the app does a little bit more under the hood. Let's jump for a second at the end of this callback:

	    ...
	    return requestMoreLocationUpdates(newLocation);
	}

	private boolean requestMoreLocationUpdates(Location newLocation) {
	    double accuracy = newLocation.getAccuracy();
	    int desiredAccuracy = getDesiredAccuracy();

	    // Check if the new location is fresh and accurate enough.
	    return isOldLocation(newLocation) || accuracy > desiredAccuracy;
	}

	private boolean isOldLocation(Location location) {
	    long locationTime = location.getTime();
	    long timeDiff = getDesiredTimeDifference();
	    long desiredTime = System.currentTimeMillis() - timeDiff;
	    return locationTime < desiredTime;
	}

Let's assume that the location isn't either precise or fresh enough, in this case the location manager will keep asking for location updates until it gets a location that is accurate enough. I'd like to point out something here: in our specific use case, the app doesn't need to continuously ask for location updates. If the app finds a location that is fresh and accurate enough, the app will simply stop requesting location updates. The tricky part here is to find a good threshold: if it requires the location to be too accurate this logic will be useless, otherwise the risk is to use really coarse locations and have bad results returned (if, like in our case, the API you're using searches for results in an area centered around user's location). If you read the previous post you know that the logic that requests a new location is always re-triggered `onResume()`, so by "stopping" I don't actually mean that location updates won't be requested at any point in the future, but only that as long as the user stays withiin the context of that `Activity` the location logic will be turned off, saving battery power. This fulfills our second requirement.

If the last location isn't either precise or fresh enough, at some point in the future a new location will be returned by one of the available providers. The `onNewLocation()` callback will be invoked again but in this case the last searched location won't be null.

    ...

    } else {
        boolean diffDistanceTooBig = diffDistanceTooBig(newLocation, lastSearchedLocation);
        if (diffDistanceTooBig) {
            boolean hasBetterLocationMessages = locationHandler
                    .hasMessages(MSG_BETTER_LOCATION);
            if (!hasBetterLocationMessages) {
                // It's a better location, invite the user to refresh his location
                locationHandler.obtainMessage(MSG_BETTER_LOCATION).sendToTarget();
            } else {
                locationHandler.removeMessages(MSG_BETTER_LOCATION);
                Message msg = locationHandler.obtainMessage(MSG_BETTER_LOCATION);
                locationHandler.sendMessageDelayed(msg,
                        QypeNewLocationHandler.DELAY_SHOW_BETTER_LOCATION_RELOAD_TOOLTIP);
            }
        }
    }

    ...

The location manager will calculate the distance between the new and the old distance and, if this distance is greater than a defined threshold (in our case this is simply the desired location accuracy), it will send a new message to the location handler. The rationale here is that if the new location doesn't differ too much from the old one, it means that the old location wasn't that bad and the app doesn't need to do anything special (note: it doesn't matter how coarse it was!). But if the new location is too far away from the last one, the app will show a reload view that asks the user if he/she wants to reload the results.

![Qype Android app nearby results reload](../../images/nearby_results_reload.jpg)

The last piece of the puzzle is the `QypeNewLocationHandler` class, which is just a simple implementation of a Handler that takes advantage of delayed messages to show/hide the reload view.

	@Override
	public void handleMessage(Message msg) {
	    ReloadTooltip reloadTooltip = context.getReloadTooltip();
	    switch (msg.what) {
	    case MSG_BETTER_LOCATION:
	        if (showReloadTooltip(reloadTooltip)) {
	            reloadTooltip.setText(R.string.msg_reload_results_on_new_location);
	            reloadTooltip.show(true);
	            sendEmptyMessageDelayed(MSG_HIDE_RELOAD_TOOLTIP, TIMEOUT_HIDE_RELOAD_TOOLTIP);
	            reloadTooltip.setTag(System.currentTimeMillis());
	            hasBetterLocation = false;
	        } else if (context.isLoadingData()) {
	            hasBetterLocation = true;
	        }
	        break;
	    case MSG_HIDE_RELOAD_TOOLTIP:
	        reloadTooltip.hide(true);
	        break;
	    case MSG_SEND_REQUEST_USING_UNRELIABLE_LOCATION:
	        Dialog waitForLocationDialog = context.getWaitForLocationDialog();
	        if (waitForLocationDialog != null && waitForLocationDialog.isShowing()) {
	            ((Activity) context).dismissDialog(R.id.ign_loc_dialog_wait_for_fix);
	        }
	        context.onLocationAvailable();
	        break;
	    default:
	        super.handleMessage(msg);
	    }
	}

Nothing special here, apart from a tweak we added while we were testing the app that prevents the reload view from being shown too often and becoming annoying for the user.
