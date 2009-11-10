all: html-split html-single css pdf dvi ps txt nroff local

html-split: build/_html-split.done
html-single: build/_html-single.done
css: build/_css.done
pdf: build/_pdf.done
dvi: build/_dvi.done
ps: build/_ps.done
txt: build/_txt.done
nroff: build/_nroff.done
meta: release/meta
local: build/_local.done

generated_sources = \
src/m_docid.ud meta/id \
src/m_pkg.ud   src/m_supp.ud \
src/m_title.ud meta/title_full \
src/footer.html src/header.html \
src/footer.txt src/header.txt \
src/main.tex

#----------------------------------------------------------------------
# meta

meta/id: mk-docid meta/id_prefix
	./mk-docid meta/id_prefix > meta/id.tmp && mv meta/id.tmp meta/id
src/m_docid.ud: mk-ud-docid meta/id
	./mk-ud-docid meta/id > src/m_docid.ud.tmp && mv src/m_docid.ud.tmp src/m_docid.ud
src/m_pkg.ud: mk-ud-pkg meta/pkg
	./mk-ud-pkg meta/pkg > src/m_pkg.ud.tmp && mv src/m_pkg.ud.tmp src/m_pkg.ud
src/m_supp.ud: mk-ud-supp
	./mk-ud-supp meta/supported > src/m_supp.ud.tmp && mv src/m_supp.ud.tmp src/m_supp.ud
meta/title_full: mk-title meta/pkg meta/title
	./mk-title meta/pkg meta/title > meta/title_full.tmp && mv meta/title_full.tmp meta/title_full
src/m_title.ud: mk-ud-title meta/title_full
	./mk-ud-title meta/title_full > src/m_title.ud.tmp && mv src/m_title.ud.tmp src/m_title.ud

#----------------------------------------------------------------------
# source generation

src/footer.html: meta/site meta/title_full mk-html-footer
	./mk-html-footer meta/site meta/title_full > src/footer.html.tmp && mv src/footer.html.tmp src/footer.html
src/header.html: meta/site meta/title_full mk-html-header
	./mk-html-header meta/site meta/title_full > src/header.html.tmp && mv src/header.html.tmp src/header.html
src/footer.txt: meta/site meta/title_full mk-txt-footer
	./mk-txt-footer meta/site meta/title_full > src/footer.txt.tmp && mv src/footer.txt.tmp src/footer.txt
src/header.txt: meta/site meta/title_full mk-txt-header
	./mk-txt-header meta/site meta/title_full > src/header.txt.tmp && mv src/header.txt.tmp src/header.txt
src/main.tex: meta/title_full mk-tex-code
	./mk-tex-code meta/title_full > src/main.tex.tmp && mv src/main.tex.tmp src/main.tex

#----------------------------------------------------------------------
# build targets

build/_html-split.done:\
meta/id src/main.ud release build build/_local.done $(generated_sources)
	(cd src && udoc-render \
		-c `head -n 1 ../conf/xh-toc` \
		-s `head -n 1 ../conf/xh-split` \
		-r xhtml main.ud ../build)
	cp build/*.html release
	touch build/_html-split.done

build/_html-single.done:\
meta/id src/main.ud release build build/_local.done $(generated_sources)
	(cd src && udoc-render \
		-c `head -n 1 ../conf/xh-toc` \
		-s 0 \
		-r xhtml main.ud ../build)
	cp build/0.html release/single.html
	touch build/_html-single.done

build/_css.done: src/main.css
	cp src/main.css build
	cp build/main.css release
	touch build/_css.done

build/_txt.done:\
meta/id meta/pkg src/main.ud pkg-name release build build/_local.done \
$(generated_sources)
	(cd src && udoc-render -r plain main.ud ../build)
	cp build/0.txt release/`./pkg-name meta/pkg`.txt
	touch build/_txt.done

build/_nroff.done:\
meta/id meta/pkg src/main.ud pkg-name release build build/_local.done \
$(generated_sources)
	(cd src && udoc-render -r nroff main.ud ../build)
	cp build/0.nrf release/`./pkg-name meta/pkg`.nrf
	touch build/_nroff.done

build/0.tex:\
meta/id meta/pkg src/main.ud release build build/_local.done conf/ctex-split \
$(generated_sources)
	(cd src && udoc-render \
		-s `head -n 1 ../conf/ctex-split` \
		-r context main.ud ../build)

build/_dvi.done:\
meta/id meta/pkg src/main.ud build/0.tex pkg-name release build \
build/_local.done $(generated_sources)
	(cd build && texexec --dvi 0.tex)
	cp build/0.dvi release/`./pkg-name meta/pkg`.dvi
	touch build/_dvi.done

build/_pdf.done:\
meta/id meta/pkg src/main.ud build/0.tex pkg-name release build \
build/_local.done $(generated_sources)
	(cd build && texexec --pdf 0.tex)
	cp build/0.pdf release/`./pkg-name meta/pkg`.pdf
	touch build/_pdf.done

build/_ps.done:\
meta/id meta/pkg src/main.ud build/0.pdf pkg-name release build \
build/_local.done $(generated_sources)
	(cd build && pdf2ps 0.pdf)
	cp build/0.ps release/`./pkg-name meta/pkg`.ps
	touch build/_ps.done

build/_local.done:\
local.sh
	./local.sh

release:
	mkdir release

build:
	mkdir build

release/meta: meta/id meta/title_full meta/pkg pkg-meta pkg-name
	./pkg-meta

package: meta pkg-make pkg-name meta/pkg
	./pkg-make

#----------------------------------------------------------------------

clean:
	rm -f $(generated_sources)
	rm -rf build
	rm -rf release

clean-all: clean
