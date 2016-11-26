"""
since-last plugin for rawdog
Copyright 2012 Adam Sampson <ats@offog.org>

When writing output, only include articles that haven't previously been
written. This is probably what you want if you run "rawdog -w" by hand whenever
you want to read feeds.

This assumes that new articles will only be added at the end of the output
(i.e. using it in conjunction with sortbyfeeddate is not a good idea).
"""

import rawdoglib.plugins

def output_sort_articles(rawdog, config, articles):
	storage = rawdog.get_plugin_storage("org.offog.ats.since-last")

	last_latest = storage.get("latest_date", 0)

	new_articles = []
	latest = 0
	discarded = 0
	for a in articles:
		# The sort date is inverted in the list...
		sort_date = -a[0]
		if sort_date > latest:
			latest = sort_date

		if sort_date <= last_latest:
			# We wrote this one last time.
			discarded += 1
		else:
			new_articles.append(a)

	storage["latest_date"] = latest
	rawdog.modified()

	config.log("Selected articles from ", last_latest, " to ", latest,
	           "; discarded ", discarded, " of ", len(articles))

	articles[:] = new_articles
	return True

rawdoglib.plugins.attach_hook("output_sort_articles", output_sort_articles)
