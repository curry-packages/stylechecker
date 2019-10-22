default: curry-style-guide.html

# translate to single HTML document:
curry-style-guide.html: curry-style-guide.md style.html
	pandoc -s -N --toc -H style.html -o $@ $<

# translate to mediawiki syntax:
curry-style-guide.wiki: curry-style-guide.md style.html
	pandoc --to=mediawiki -o $@ $<
