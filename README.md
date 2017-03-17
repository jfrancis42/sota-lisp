# SOTA

SOTA, or Summits on the Air, is a ham radio organization where ham operators climb mountains to operate, and make contacts to people on other peaks, as well as home stations. Coordination of activities is done with the SOTA web page at http://www.sotawatch.org/

When an operator is heard on a mountain-top, he is "spotted" on the web page (it's not uncommon for operators to spot themselves when cell coverage is available) so that other operators know where he is and what frequency and mode he is operating on. Points are earned by both parties, and the entire process is somewhat competitive, meaning that timely awareness of mountain-top operators is crucial.

The SOTA site at one time provided an API to programmatically extract data from their web page, but this no longer seems to be the case. This software provides much of the functionality of this missing API, providing the ability to create applications, such as an activity notifier. A specific application is provided for this functionality that uses this library.

The SOTA web page is updated at sixty second intervals. This library is capable of scraping the raw HTML, parsing it, and extracting all of the relevant information and providing it to client applications. This data is kept in a hash, and a lock is available for manipulating this hash by using (bt:with-lock-held (sota:\*spot-lock\*) foo).

There are no options to configure. Once the library has been loaded, you simply start the main thread:

```
(sota:start-spotter)
```

The thread will run forever, keeping the sota:\*spots\* hash up to date with the latest data.

 There are at least two smartphone apps available, of which SOTAGoat is by far the most popular. SOTAGoat provides maps, schedules, and at one time in the past, used to provide pop-up alerts so the phone user would be immediately notified when a peak was activated. Unfortunately, the SOTAGoat app seems to no longer provide this functionality, making it considerably more difficult to be aware of activity without careful attention to the SOTA web page. The entire purpose behind writing this code was to be able to write a replacement for this missing functionality to provide updates when an activator goes on the air. This is provided in the form of the alert.lisp application included in this git archive.

If you'd like to use the alert.lisp program, you'll first want to do a little customizing. The first thing you'll want to do is construct a list of the associations you'll want to receive spots from. A full list can be obtained by calling:

```
(spot:get-association-list)
```

The list of desired associations must be configured in the source, by setting the value of \*favorites\*. I live in the Seattle area, and the list is set to the areas I care most about (ie, the areas I can reliably communicate with, given my mediocre antenna). Once \*favorites\* has been properly set, you can call the following function to display a list of spots that match your selected associations within the specified number of seconds (one hour, in this example):

```
(sota:get-my-spots 3600 *favorites* nil)
```

Including the nil is important, as this prevents the code from updating the processed field, which prevents a given entry from showing up again (it's the mechanism that ensures that each activation is only sent once).

alert.lisp sends updates by way of Pushover is a free text-like service with clients for iOS and Android, and requires the pushover library for lisp (lucky enough, I wrote it, and it's avaiable on github). Credentials for Pushover are stored using creds, another library available on github. The two values needed are 'pouser' and 'potoken', both of which are supplied when you sign up on the pushover web site. A different Pushover notification sound may be selected by changing the appropriate parameter in the source (see the pushover library documentation for details). The sounds is currently set to :spacealarm.

To start sending updates, load alert.lisp, and call:

```
(start-alerts)
```

This function calls sota:start-spotter itself, then starts a thread to start polling once per minute and sending updates via Pushover. The first "batch" includes all spots from the last hour, then only new spots going forward.

That's it. As long as you don't close the REPL, it should run forever.

Future enhancements will include being able to filter based on frequency/band, mode, and distance to the activated peak. Basic support for most of this has already been built, it's just a matter of completing the front-end code. I also intend to add the ability to post spots and alerts using this library. I've been experimenting with adding a RESTful API, as well, allowing for easy access from any kind of code. This would allow providing a SOTA API as a service. But I'll have a chat with the SOTA guys before I make any plans for offering this as a public service.
