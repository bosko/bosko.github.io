---
layout: post
title: "Exefy ‘Em All"
date: 2012-06-25 01:51
comments: true
tags: ruby rubygems RubyInstaller
---
Recently Luis Lavena started thread [Idea: executable stubs to replace batch files](https://groups.google.com/d/topic/rubyinstaller/fQCuPfiuuRc/discussion) on RubyInstaller mailing list. In short, the idea is to use command line applications - executable stubs - instead of batch files in RubyInstaller Ruby versions. Using command line applications instead of batch files has several benefits. First one, maybe not so important, is to avoid annoying

Terminate batch job (Y/N)?

question when execution is interrupted with Ctrl-C key combination. The second is to get meaningful list of processes in the system. With batch files we can only see bunch of ruby.exe processes in the list. This gives us possibility to define firewall rules for these applications which will not be applied globally for all Ruby scripts. Finally, installing Ruby applications as services, with the help of some service wrapper, is usually easier if we use executable file. These are reasons why new [gem-exefy](http://github.com/bosko/gem-exefy) gem was made.

### Gem-exefy Internals

Gem-exefy mimics behavior of batch files installed by RubyGems and to see how it works we must know how existing batch files work. First step is to install gem that has executable value defined in gem specification. Example of such gem is Bundler. After installing it on Windows, RubyGems will create bundle.bat file in `<path_to_ruby_installation>/bin` folder. Content of that file is:

{% highlight bat %}
@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
@"ruby.exe" "c:/path/to/ruby/installation/bin/bundle" %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO :EOF
:WinNT
@"ruby.exe" "%~dpn0" %*
{% endhighlight %}

Without digging too much into all details we can see that batch file starts ruby.exe passing it full path to Ruby script (bundle, in this case) using all arguments passed to batch file as arguments of Ruby script. So all we have to do is to make application which will be able to execute Ruby script and will accept arguments passed in the command line. Sounds familiar, isn’t it? Exactly! We already have ruby.exe and in Ruby code we can find almost everything we need. Here is a slightly simplified version of Ruby’s main.c file:

{% highlight c %}
#include "ruby.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

int
main(int argc, char **argv)
{
#ifdef HAVE_LOCALE_H
  setlocale(LC_CTYPE, "");
#endif

  ruby_sysinit(&argc, &argv);
  {
    RUBY_INIT_STACK;
    ruby_init();
    return ruby_run_node(ruby_options(argc, argv));
  }
}
{% endhighlight %}

If we build application from this source we will get the same application as ruby.exe. This means if we want to execute some Ruby script we will have to pass path to it as a first argument with optional arguments following it. But our goal is not to invoke application with the path to Ruby script. Instead we want to invoke predefined script. In order to achieve that, we obviously have to alter the list of arguments (argv) and to insert path to target Ruby script. But there is a catch (I spent almost whole day to figure it out). We must change the list of arguments after the call

{% highlight c %}
ruby_sysinit(&argc, &argv);
{% endhighlight %}

Ruby performs system dependent arguments initialization in the above method and the list of arguments will be reverted back if we change it before this initialization. Of course there are some additional details that we must take care of, but you can figure them out directly from "the source":https://github.com/bosko/gem-exefy/blob/master/templates/gem_exe.c. After we change the list of arguments, Ruby will execute script we passed it and that’s all the magic gem-exefy does.

### Let’s Exefy

We are now ready to exefy existing and new gems on our RubyInstaller version. gem-exefy is made as RubyGems plugin. After installing it with:

{% highlight sh %}
gem install gem-exefy
{% endhighlight %}

new gem command will be available - exefy. This command is used for replacing batch files for single or all installed gems. Replacing batch files with executable stubs for single gem is performed by passing name of the targeted gem to the exefy command.

{% highlight sh %}
gem exefy bundler
{% endhighlight %}

Exefying all installed gems is simple - just pass `--all` to `exefy` command

{% highlight sh %}
gem exefy --all
{% endhighlight %}

If you are not satisfied and still want to use batch files - don’t worry. You can always revert old batch files for single or all gems with `--revert` argument

{% highlight sh %}
gem exefy bundler --revert
gem exefy --all --revert
{% endhighlight %}

After gem-exefy is installed it will, by default, install executable stubs instead of batch files for all gems installed after it.

It is important to mention that gem-exefy will not replace batch files for commands installed with RubyInstaller (irb, rake...) and are part of Ruby core. Will support for converting these batch files be implemented is yet to be seen.

### Acknowledgements

I want to thanks [@luislavena](https://github.com/luislavena),[@azolo](https://github.com/azolo) and [@jonforums](https://github.com/jonforums) for helping me out making gem-exefy.
