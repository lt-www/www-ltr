%.gr.png : %.png
	convert $< -colorspace Gray $@

%.bw.png : %.png
	convert $< -monochrome $@

all:
	convert -resize 28x28 bc.png bc-28.png
	convert -resize 28x28 fb.png fb-28.png
	convert -resize 28x28 ig.png ig-28.png
	convert -resize 28x28 sc.png sc-28.png

I = fb-14 ig-14 ms-14 rss-14 sc-14 fb-28 ig-28 ms-28 rss-28 sc-28

obsolete: $(I:=.bw.png) $(I:=.gr.png)

clean:
	rm -f *.bw.png *.gr.png

