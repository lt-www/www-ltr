%.gr.png : %.png
	convert $< -colorspace Gray $@

%.bw.png : %.png
	convert $< -monochrome $@

I = fb-14 ig-14 ms-14 rss-14 sc-14 fb-28 ig-28 ms-28 rss-28 sc-28

all: $(I:=.bw.png) $(I:=.gr.png)

clean:
	rm -f *.bw.png *.gr.png

