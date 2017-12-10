---
layout: post
title: "Splitting Ruby Array"
date: 2009-06-04 19:00
comments: true
categories: ruby
tags: ruby
---
Recently I had to process arrays of data with more than thousand of members but requirement was not to process them one by one. Instead I had to split array into chunks and to process each of sub-arrays separately. I've found one definition of chunk method within comments on this [post](http://snippets.dzone.com/posts/show/3486) and I'm including it here. This method splits array in given number of sub-arrays and I needed a version that will split array in the sub-arrays that have predefined size (ok, I know I could simply calculate a number of chunks by dividing total number of elements by required number of elements, but I just wanted one method that'll do all that for me). So I wrote chunk_max_num method. If maximum number of elements per array is zero or negative it will return original array and for all other cases it will return array which has arrays of given size as members.

{% highlight ruby %}
require 'enumerator'

class Array
  def chunk_max_num(max_num)
    return slice(0...length) unless max_num > 0
    quot, mod = length.divmod(max_num)
    quot += 1 if mod > 0
    (0..quot).map {|i| i*max_num}.enum_cons(2).map {|a,b| slice(a...b)}
  end

  def chunk(pieces)
    q,r = length.divmod(pieces)
    (0..pieces).map {|i| i * q + [r,i].min}.enum_cons(2).map {|a,b| slice(a...b)}
  end
end
{% endhighlight %}
