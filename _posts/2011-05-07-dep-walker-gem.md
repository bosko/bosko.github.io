---
layout: post
title: "dep_walker Gem"
date: 2011-05-07 06:40
comments: true
tags: ruby rubyinstaller devkit
---
Common problem that Ruby developers face on Windows is missing dll message box that appears when they try to use gem that has extension library. Usually these Gems are packed with pre-built binary\ extensions for windows and, even though, installation passes without any error or warning, when they try to use them they realize that dll these Gems depend on are missing on the system.

Maybe most common Gem for which this happens is sqlite3, Ruby binding for the SQlite3 embedded database. If sqlite3.dll is missing from the system, after installation of sqlite3 Gem any attempt to use it causes following message box to appear.

![Missing DLL message box.]({{ site.url }}/images/dep_walker/missing_dll_msg_box.png)

If two versions of a required dll exist on the system Gem might use the wrong one and strange crashes can appear.

These errors are not limited to the Gems that have pre-built binary extensions but can also happen if extension library was built with the [RubyInstaller’s](http://www.rubyinstaller.org) [DevKit](http://www.rubyinstaller.org/add-ons/devkit). If development files (header and library files) needed for the extension library to be built exist on the system while target dll is missing gem usage\ will cause the same message box to appear.

Frequently novice Ruby users on Windows ask question on mailing lists why Gem is not working even if it was installed without any error. That’s why I made dep\_walker, small utility Gem, that can be used to check whether all dependencies for extension libraries used by installed Gem (s) are met or not. Source of the Gem can be found on [GitHub](http://github.com/bosko/dep_walker).

Usage is very simple. If you want to check all installed Gems just invoke dep\_walker with the ’-a’ switch.

{% highlight sh %}
dep_walker -a
{% endhighlight %}

And for particular gem swith `-c` can be used.

{% highlight sh %}
dep_walker -c sqlite3
{% endhighlight %}

More verbose output is obtained via `-t` and colour with `--color` swith. Happy dependencies walking!