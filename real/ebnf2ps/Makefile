TOP = ../../..
PROG = ebnf2ps
SRCS = Parsers.hs StringMatch.hs Color.hs Fonts.hs Info.hs IOSupplement.hs \
       EbnfGrammar.hs EbnfLayout.hs PsOutput.hs FigOutput.hs \
       CommandLine.hs AbstractSyntax.hs Lexer.hs HappyParser.hs \
       GrammarTransform.hs Main.hs
EXTRA_RUNTESTFLAGS = ebnf2ps.stdin apat -o1 ebnf2ps.stdout
include $(TOP)/nofib/mk/nofib.mk

HappyParser.hs : HappyParser.ly
	$(RM) HappyParser.hs
	happy HappyParser.ly
	@chmod 444 HappyParser.hs

clean ::
	$(RM) HappyParser.hs
