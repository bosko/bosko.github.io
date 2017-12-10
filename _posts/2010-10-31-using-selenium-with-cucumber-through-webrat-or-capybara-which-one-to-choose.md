---
layout: post
title: "Using Selenium with Cucumber through Webrat or Capybara. Which one to choose?"
date: 2010-10-31 13:55
comments: true
tags: ruby rails bdd testing
---
Introduction
------------

Testing is (or should be) important part of every software development. Over time various testing strategies and supporting tools and frameworks have been developed. Regarding Web development biggest advance has been made in Behavior Driven Development. Consequently many tools for BDD are published and used.

[Ruby on Rails](http://www.rubyonrails.org) framework had great built-in support for testing from the very beginning. As it usually happens, lot of specialised testing tools appeared aside of it and among all of them my favourites are [RSpec](http://rspec.info) and [Cucumber](http://github.com/aslakhellesoy/cucumber/wiki). The first one for unit and the second for functional tests. Both of them are well integrated with Ruby on Rails and are very easy to set up and start with. Moreover there is no need to use real browsers which results in fast tests execution. Perfect way for BDD.

But what if you have to perform functional tests on non Ruby on Rails applications or your application relies heavily on JavaScript (no matter in which framework it is written)? Luckily Cucumber can be used in that case too. Since Cucumber supports Rails out of the box there is basically no need for some special configuration. On the other hand if a real browser must be used in tests, or functional testing must be done outside of the Rails environment setting up Cucumber can be little tricky but still simple enough. In this article I will focus on this scenario - _testing non Rails applications with [Cucumber](http://github.com/aslakhellesoy/cucumber/wiki) and [Selenium](http://seleniumhq.org)_.

Using Selenium in Cucumber tests is done through [Webrat](http://github.com/brynary/webrat/wiki) or [Capybara](http://github.com/jnicklas/capybara). First we must set up complete environment, and in the first step all necessary gems must be installed:

{% highlight ruby %}
gem install launchy
gem install rspec
gem install cucumber
gem install webrat
gem install capybara --pre
gem install selenium-client
gem install selenium-webdriver
{% endhighlight %}

_Option —pre is used to install Capybara 0.4.0 rc_

**Important notice for MS Windows users: Webrat depends on Json gem which installs binaries compiled against [Rubyinstaller](http://www.rubyinstaller.org) Ruby 1.8.x version. If you are using 1.9.2 Ruby you must uninstall Json gem and install it again but with `--platform=ruby` option:**

{% highlight ruby %}
gem uninstall json
gem install json --platform=ruby
{% endhighlight %}

Since article focuses on the functional testing outside of Rails we should manually create folder structure that Cucumber expects.

    tests
     |- features
         |- support
         |- step_definitions

All `.feature` files go in the `features` folder. In the `support` folder `env.rb` file should be created and within it all set up must be made. Finally steps are implemented in Ruby files in `step_definitions` folder.

Webrat
------

[Webrat](http://github.com/brynary/webrat/wiki) controls Selenium through Selenium RC (remote control) and selenium-client gem. In order to use Selenium through Webrat put following code in your `env.rb`:

{% highlight ruby %}
require 'cucumber/formatter/unicode'

require 'webrat'
require 'webrat/core/matchers'

Webrat.configure do |config|
  config.mode = :selenium
  config.application_framework = :external
  config.selenium_server_address = '127.0.0.1'
    if RbConfig::CONFIG['host_os'] =~ /mingw|mswin/
  config.selenium_browser_startup_timeout = 60
  config.application_address = 'localhost'
  config.application_port = '3000'
end

World do
  session = Webrat::Session.new
  session.extend(Webrat::Methods)
  session.extend(Webrat::Selenium::Methods)
  session.extend(Webrat::Selenium::Matchers)
  session
end
{% endhighlight %}

That’s all if you are running Linux based system. On Windows a little bit more effort must be made. First of all, Webrat usess 0.0.0.0 IP address when it starts Selenium and MS Windows does not like it at all. Secondly it uses `/dev/null` stream which is not available on MS Windows. Patch is already submitted and you can follow a ticket at [Webrat Lighthouse](https://webrat.lighthouseapp.com/projects/10503/tickets/387-tiny-patch-for-work-with-selenium-on-windows#ticket-387-2). But until fix is accepted and new version is released, you can take a patch from [Github gitst](http://gist.github.com/584005) and apply it to Webrat sources.

{% gist 584005 %}

Besides this patch few more things must be done. Line:

{% highlight ruby %}
config.selenium_server_address = '127.0.0.1' if RbConfig::CONFIG['host_os'] =~ /mingw|mswin/
{% endhighlight %}

must be added to the `config` block as is already shown in the above snippet. Unfortunately selenium-client gem does not recognize [Rubyinstaller](http://www.rubyinstaller.org) since it is built using MinGW tools. Therefore one more tiny patch must be made in the `selenium-client-1.2.18/lib/nautilus/shell.rb` file. Function `windows?` must be replaced with:

{% highlight ruby %}
def windows?
  ::RbConfig::CONFIG['host_os'] =~ /mswin|mingw/
end
{% endhighlight %}

You are ready for application testing. By default Selenium will use Firefox and if you want to use other browser (in the example Internet Explorer is set) add following line to `config` block:

{% highlight ruby %}
config.selenium_browser_key = '*iexplore'
{% endhighlight %}

Capybara
--------

Although [Capybara](http://github.com/jnicklas/capybara) can use Selenium RC, it primarily uses Selenium WebDriver which is still in beta phase but is working good. Since we already installed all necessary gems we can go on with configuring our testing environment. File `env.rb` should look like this:

{% highlight ruby %}
require 'rbconfig'
require 'cucumber/formatter/unicode'

require 'capybara'
require 'capybara/dsl'
require "capybara/cucumber"

Capybara.default_driver = :selenium
Capybara.app_host = "http://127.0.0.1:8000/"
Capybara.register_driver :selenium do |app|
  Capybara::Driver::Selenium.new(app, :browser => :firefox)
end

World(Capybara)
{% endhighlight %}

Setting up Capybara is definitely much easier. But on MS Windows systems, if you want to use Internet Explorer, you still have to patch sources. Authors are already [notified about required patch](http://groups.google.com/group/webdriver/browse_thread/thread/a8ed4ce6f98e8322) and I believe that new version of `selenium-webdriver` gem will be released with it. In the meantime you just have to change definition of `initialize` method in `selenium-webdriver-0.0.28/lib/selenium/webdriver/ie/bridge.rb` from:

{% highlight ruby %}
def initialize()
{% endhighlight %}

to

{% highlight ruby %}
def initialize(opts = {})
{% endhighlight %}

Changing browser is as easy as changing `:firefox` to `:ie` or `:chrome`. Instead of `:firefox` you can also use `:ff` and for Internet Explorer `:internet_explorer`. One more notice about differences if you are switching from Webrat to Capybara. Capybara will reset session after each step. If you do not want that (for example you log in to your application in the first scenario, and do not want to repeat it in each succeeding one) just add:

{% highlight ruby %}
After do
end
{% endhighlight %}

in `env.rb` file.

With Capybara you are not limited to Selenium WebDriver. If you want to use Selenium RC you just need to configure it in `env.rb` file:

{% highlight ruby %}
require 'rbconfig'
require 'cucumber/formatter/unicode'

require 'capybara'
require 'capybara/dsl'
require "capybara/cucumber"

Capybara.default_driver = :selenium
Capybara.app_host = "http://127.0.0.1:9000/"
Capybara.register_driver :selenium do |app|
  # This way we are using Selenium-RC
  Capybara::Driver::Selenium.new(app,
                                 :browser => :remote,
                                 :url => "http://127.0.0.1:4444/wd/hub",
                                 :desired_capabilities => :internet_explorer)
end

World(Capybara)
{% endhighlight %}

Conclusion
----------

Both gems for running Selenium as a base for functional tests - Webrat and Capybara are easy to use. Although Webrat needs more patching to work under Windows it has one advantage. It can be used with [Mechanize](http://mechanize.rubyforge.org/mechanize/) if you do not need real browser and you still want to test non Rails application. But as much as it is advantage for “classic” Web application Mechanize cannot interpret JavaScript. So if you want to include JavaScript testing you either have to use real browser or switch to Capybara.

Capybara, on the other hand, needs significantly less patching on MS Windows systems and it cannot use Mechanize as far as I know. But, from my point of view, it is easier to use then Webrat. Currently it cannot use Mechanize, but it can use [Culerity](http://github.com/langalex/culerity/) and [Celerity](http://celerity.rubyforge.org/) for JavaScript testing. Moreover [capybara-envjs driver](http://github.com/smparkes/capybara-envjs) can be used to interpret JavaScript outside of the browser.

Although I’m still not sure which one is better to use, I switched from Webrat to Capybara and I think that tests that use Selenium WebDriver are running faster. There is an [initiative for merging](http://groups.google.com/group/ruby-capybara/browse_thread/thread/4bcc26a9cfa20ef2) these projects into one but I do not know if it will happen and when. I would like to hear what you think. What is your choice: Webrat or Capybara?
