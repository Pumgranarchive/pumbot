NAME :=		pum_bot

ML :=		argParser.ml	\
		utils.ml	\
		extractTools.ml	\
		youtube.ml	\
		pboilerpipe.ml	\
		pxtractor.ml	\
		readability.ml 	\
		dbpedia.ml	\
		crawler.ml	\
		main.ml

MLI :=		argParser.mli	\
		extractTools.mli\
		youtube.mli	\
		pboilerpipe.mli	\
		pxtractor.mli	\
		readability.mli	\
		dbpedia.mli

PACKAGES :=	lwt,cohttp,cohttp.lwt,pghttp,str,rdf,rdf.lwt,yojson,netstring,urlnorm,ptools,pumgrana_http,bfy,dbpedia,tidy,readability_http,boilerpipe,xtractor,opencalais_http

CMX :=		$(ML:.ml=.cmx)
CMO :=		$(ML:.ml=.cmo)
CMI :=		$(MLI:.mli=.cmi)
LIB :=		-package $(PACKAGES)
SYNTAX :=	-syntax camlp4o -package lwt.syntax
DOC_DIR :=	doc/html
OCAMLFIND :=	ocamlfind
OCAMLDOC :=	$(OCAMLFIND) ocamldoc $(SYNTAX) $(LIB) -intro doc/indexdoc -html -d $(DOC_DIR)
OCAMLC :=	$(OCAMLFIND) ocamlc $(SYNTAX) $(LIB)
OCAMLOPT :=	$(OCAMLFIND) ocamlopt $(SYNTAX) $(LIB)
OCAMLDEP :=	$(OCAMLFIND) ocamldep $(SYNTAX) $(LIB)

RM :=		rm -fv

all:		$(NAME)

re:		clean all

$(NAME):	$(CMI) $(CMO)
		$(OCAMLC) -linkpkg $(CMO) -o $@

doc:
		mkdir -p $(DOC_DIR)
		$(OCAMLDOC) $(MLI)

.SUFFIXES:	.ml .mli .cmo .cmi .cmx

.ml.cmx:
		$(OCAMLOPT) -c $<

.ml.cmo:
		$(OCAMLC) -c $<

.mli.cmi:
		$(OCAMLC) -c $<

clean:
		@$(RM) *.cm[iox] *.o
		@$(RM) $(NAME) $(NAME).cma

.depend:
		@$(RM) .depend
		$(OCAMLDEP) $(MLI) $(ML) > .depend

.PHONY:		doc

include .depend
