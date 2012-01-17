TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = Data/Graph/Inductive/Internal/Thread.hs \
       Data/Graph/Inductive/Graph.hs \
       Data/Graph/Inductive/Graphviz.hs \
       Data/Graph/Inductive/PatriciaTree.hs \
       Data/Graph/Inductive/Query/ArtPoint.hs \
       Data/Graph/Inductive/Basic.hs \
       Data/Graph/Inductive/Query/DFS.hs \
       Data/Graph/Inductive/Query/BCC.hs \
       Main.hs
PROG_ARGS += le450_15a.col le450_15b.col le450_15c.col le450_15d.col le450_25a.col le450_25b.col le450_25c.col le450_25d.col le450_5a.col le450_5b.col le450_5c.col le450_5d.col
HC_OPTS += -i. -package array -package base -package bytestring -package containers -package mtl
include $(TOP)/mk/target.mk
