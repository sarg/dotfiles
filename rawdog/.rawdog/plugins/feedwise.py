# Feedwise Plugin version 0.2 for Rawdog.
# Copyright 2005 Ian Glover <ian@manicai.net>
# Copyright 2006 Virgil Bucoci
#
# Sort articles into chunks by feed rather than date.
#
# Wed Sep  6 16:32:40 EEST 2006
# Modified by Virgil Bucoci <vbucoci at acm.org>
# * use the right hook to write the feed headings
# * sort feeds by feed name
# * added navigation links between feeds
# * feed content gets wrapped in ordered-list tags, so if you add
#   list-item tags <li> to your item template, you will get per-feed
#   numbered articles
# * added default value for articles_per_feed config option (idea
#   stolen from Dog Walker <forestiero at qwest.net>,
#   http://lists.us-lot.org/pipermail/rawdog-users/2006-September/000254.html)
# * minor code cleanup

import rawdoglib.plugins

class FeedwisePlugin:
    def __init__(self):
        self.last_feed = None
        self.feed_no = 0

    def startup(self, rawdog, config):
        # daysections and timesections config options are not used
        # when this plugin is used.  We disable them just to be safe.
        msg = "FeedwisePlugin: %s config option is NOT used when this plugin is active."
        if config["daysections"]:
            print msg % "daysections"
            config["daysections"] = 0 # not really necessary
        if config["timesections"]:
            print msg % "timesections"
            config["timesections"] = 0 # not really necessary
        try:
            articles_per_feed = config["articles_per_feed"]
        except KeyError:
            config["articles_per_feed"] = 100

    # Scan the sorted list of articles and through away old ones to
    # so we don't exceed the configured limit.
    def limit_articles_per_feed(self, rawdog, config, articles):
        feed_counts = {}
        to_remove = []
        for i in range(len(articles)):
            feed = articles[i].feed
            if feed not in feed_counts:
                feed_counts[feed] = 1
            else:
                feed_counts[feed] += 1
            if feed_counts[feed] > config["articles_per_feed"]:
                # We don't want to from the list whilst iterating through it.
                # So use None as a marker for deletion.
                to_remove.append(articles[i])
        for x in to_remove:
            articles.remove(x)
        
    # Sort the articles by feed and inside each feed by time.
    def sort_by_feed(self, rawdog, config, articles):
        def comparator(lhs, rhs):
            if cmp(lhs.feed, rhs.feed) != 0:
                #return cmp(lhs.feed, rhs.feed)
                return cmp(rawdog.feeds[lhs.feed].get_html_name(config).lower(),
                           rawdog.feeds[rhs.feed].get_html_name(config).lower())
            else:
                # Inverted as we want the most recent first.
                return -cmp(lhs.date, rhs.date)
        articles.sort(comparator)
        self.limit_articles_per_feed(rawdog, config, articles)
    
    def _link(self, no):
        a = """<a href="#%s">%s</a>"""
        if no == 0:  # first link
            return a % (no+1, 'next')
        elif no < 0: # last link
            return a % (-no-1, 'prev')            
        else:
            return ("%s | %s" % (a, a)) % (no-1, 'prev', no+1, 'next')

    # Split each feed into its own block and add links to the other
    # feeds.
    def write_divider(self, rawdog, config, f, article, date):
        feed = rawdog.feeds[article.feed]

        # Basic division header.
        basic_divider =  '''<div class="feeddisplay">
<h3 class="feedtitle"><a id="%s">%s</a></h3>
%s
<ol class="feedarticles">''' \
        % (self.feed_no, feed.get_html_link(config), '')

        basic_divider = basic_divider.encode('utf-8')

        if self.last_feed != feed:       # A new feed
            if self.last_feed != None:   # not the first feed
                print >>f, '</ol></div>\n'
            print >>f,  basic_divider
            self.feed_no += 1
            self.last_feed = feed
        return False

    # All the items have been written but we have a couple of tags
    # open so close those and add the last anchor and link.
    def write_end(self, rawdog, config, f):
        f.write('</ol></div>\n<a id="%s">%s</a>'
                % (self.feed_no, self._link(-self.feed_no)))

    # Handle the articles_per_feed configuration option
    def handle_config(self, config, name, value):
        if name == "articles_per_feed":
            config["articles_per_feed"] = int(value)
            return False
        return True


plugin = FeedwisePlugin()

rawdoglib.plugins.attach_hook( "startup", plugin.startup )
rawdoglib.plugins.attach_hook( "output_sort", plugin.sort_by_feed )
rawdoglib.plugins.attach_hook( "output_items_heading", plugin.write_divider )
rawdoglib.plugins.attach_hook( "output_items_end", plugin.write_end )
rawdoglib.plugins.attach_hook( "config_option", plugin.handle_config )
