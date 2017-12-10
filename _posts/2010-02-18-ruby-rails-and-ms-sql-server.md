---
layout: post
title: "Ruby, Rails and MS SQL server"
date: 2010-02-18 22:34
comments: true
tags: ruby ActiveRecord rails mssql
---
Setting up Rails and Ruby to use MS SQL server was always painful task. Fortunately things have changed - a lot! With new [Rails SQL Server 2000, 2005 and 2008 Adapter](http://github.com/rails-sqlserver/2000-2005-adapter) and Christian Werner’s [ruby-odbc gem](http://www.ch-werner.de/rubyodbc) you can do it in a few minutes.

If you want to use these gems on Windows grab Ruby installation from [RubyInstaller](http://www.rubyinstaller.org) site and be sure to install [DevKit](http://rubyforge.org/frs/download.php/66888/devkit-3.4.5r3-20091110.7z) prior to installing ruby-odbc.

Versions of ruby-odbc before 0.9999 do not work on mingw based (RubyInstaller) Ruby. Luckily author was very fast and made new version very quickly after I sent him a patch. Thanks Christian!

Both gems work well on Ruby 1.8.6 and 1.9.1 Ruby versions on Windows with old ActiveRecords, but I hope rails adapter will be ported to ActiveRecords 3 soon.