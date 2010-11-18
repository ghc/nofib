% -*- mode: prolog -*-

%% CPSA tools in Prolog

%% This module contains a Prolog implementation of the CPSA specific
%% S-expression pretty printer algorithm used in the Haskell program.
%% It also contains a translator between an S-expression and an
%% internal representation that eases transformations on CPSA data
%% structures.

%% Known to work in SWI-Prolog, but not with GNU Prolog.

%% Copyright (c) 2009 The MITRE Corporation
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the BSD License as published by the
%% University of California.

:- module(cpsa, [cpsa_sexprs_pp/2, cpsa/2, sexprs_to_cpsas/2,
                 cpsas_to_sexprs/2]).

:- use_module(pp).
:- use_module(sexpr).

%% cpsa_sexprs_pp(+Out, +Sexprs)
%% Pretty print a list of S-expressions to stream Out.
cpsa_sexprs_pp(_, []).
cpsa_sexprs_pp(Out, [Sexpr|Sexprs]) :-
	cpsa_sexpr_pp(Out, Sexpr),
	nl(Out),
	cpsa_sexprs_pp(Out, Sexprs).

cpsa_sexpr_pp(Out, Sexpr) :-
	cpsa_sexpr_to_pretty(Sexpr, Pretty),
	pp:pr(Out, 72, Pretty),
	nl(Out).

%% Create a pretty printing tree for each top-level CPSA form.
cpsa_sexpr_to_pretty([comment|X], Pretty) :-
	sexpr:sexpr_to_pretty([comment|X], Pretty).
cpsa_sexpr_to_pretty([defskeleton|X], Pretty) :-
	sexpr_to_cpsa_pretty([defskeleton|X], Pretty).
cpsa_sexpr_to_pretty([defprotocol, Name, Alg|Roles], Pretty) :-
	pp:atm(defprotocol, X),
	pp:atm(Name, Y),
	pp:atm(Alg, Z),
	pp:atm('(', P),
	pp:atm(' ', S),
	sexpr_to_role_pretty(Roles, Pretties),
	pp:grp(2, [P, X, S, Y, S, Z, S|Pretties], Pretty).

%% CPSA role specific pretty printing support.
sexpr_to_role_pretty([[defrole|X]|Roles], [Space, Pretty|Pretties]) :-
	!,
	pp:brk(1, Space),
	sexpr_to_cpsa_pretty([defrole|X], Pretty),
	sexpr_to_role_pretty(Roles, Pretties).
sexpr_to_role_pretty(Sexprs, Pretties) :-
	sexprs_to_pretty(Sexprs, Pretties).

%% Creates a group in which breaks are allowed only before lists.
sexpr_to_cpsa_pretty(Thing, Pretty) :-
	atomic(Thing),
	!,
	sexpr:atom_to_pretty(Thing, Pretty).
sexpr_to_cpsa_pretty([Sexpr|Sexprs], Pretty) :-
	pp:atm('(', First),
	sexpr:sexpr_to_pretty(Sexpr, Second),
	sexprs_to_cpsa_pretty(Sexprs, Rest),
	pp:grp(2, [First, Second|Rest], Pretty).

sexprs_to_cpsa_pretty([], [Pretty]) :-
	pp:atm(')', Pretty).
sexprs_to_cpsa_pretty([Sexpr|Sexprs], [Space, Pretty|Pretties]) :-
	atomic(Sexpr),
	!,
	pp:atm(' ', Space),
	sexpr:sexpr_to_pretty(Sexpr, Pretty),
	sexprs_to_cpsa_pretty(Sexprs, Pretties).
sexprs_to_cpsa_pretty([Sexpr|Sexprs], [Space, Pretty|Pretties]) :-
	pp:brk(1, Space),
	sexpr:sexpr_to_pretty(Sexpr, Pretty),
	sexprs_to_cpsa_pretty(Sexprs, Pretties).

%% Translate between S-expressions and an internal form.

%% Currently, you have to read the code or look at output to
%% understand the internal form.

cpsa(File, Cpsas) :-
	sexpr:read_sexpr_list(File, Sexprs),
	sexprs_to_cpsas(Sexprs, Cpsas).

sexprs_to_cpsas([], []).
sexprs_to_cpsas([Sexpr|Sexprs], [Cpsa|Cpsas]) :-
	sexpr_to_cpsa(Sexpr, Cpsa),
	sexprs_to_cpsas(Sexprs, Cpsas).

sexpr_to_cpsa([comment|X], c(X)).
sexpr_to_cpsa([defprotocol, Name, Alg|Rest],
	p(Name, Alg, Roles)) :-
	atom(Name), atom(Alg),
	sexpr_to_roles(Rest, Roles).
sexpr_to_cpsa([defskeleton, Prot, [vars|Vars]|Rest],
	k(Prot, Decls, Strands, Precedes, Nons, Uniqs)) :-
	atom(Prot),
	decls(Vars, Decls),
	sexpr_to_strands(Rest, Strands, Alist),
	lookup(precedes, Alist, Values),
	edges(Values, Precedes),
	lookup('non-orig', Alist, Nons),
	lookup('uniq-orig', Alist, Uniqs).

sexpr_to_roles([[defrole|Sexpr]|Rest], [Role|Roles]) :-
	!,
	sexpr_to_role(Sexpr, Role),
	sexpr_to_roles(Rest, Roles).
sexpr_to_roles(_, []).

sexpr_to_role([Name, [vars|Vars], [trace|Trace]|Alist],
	r(Name, Decls, Trace, Nons, Uniqs)) :-
	decls(Vars, Decls),
	lookup('non-orig', Alist, Nons),
	lookup('uniq-orig', Alist, Uniqs).

sexpr_to_strands([[defstrand|Sexpr]|Rest], [Strand|Strands], Alist) :-
	!,
	sexpr_to_strand([defstrand|Sexpr], Strand),
	sexpr_to_strands(Rest, Strands, Alist).
sexpr_to_strands([[deflistener|Sexpr]|Rest], [Strand|Strands], Alist) :-
	!,
	sexpr_to_strand([deflistener|Sexpr], Strand),
	sexpr_to_strands(Rest, Strands, Alist).
sexpr_to_strands(Alist, [], Alist).

sexpr_to_strand([defstrand, Role, Height|Sexprs], s(Role, Height, Map)) :-
	atom(Role),
	integer(Height),
	maplets(Sexprs, Map).
sexpr_to_strand([deflistener, Term], l(Term)).

decls([], []).
decls([Sexpr|Sexprs], [Decl|Decls]) :-
	decl(Sexpr, Decl),
	decls(Sexprs, Decls).

decl(Sexprs, d(Type, Vars)) :-
	decl_vars(Sexprs, Type, Vars).

decl_vars([Type], Type, []) :-
	!,
	atom(Type).
decl_vars([Var|Decl], Type, [Var|Vars]) :-
	atom(Var),
	decl_vars(Decl, Type, Vars).

maplets([], []).
maplets([[Key, Value]|Sexprs], [[Key|Value]|Map]) :-
	maplets(Sexprs, Map).

lookup(_, [], []).
lookup(Key, [[Key|Value]|Alist], Values) :-
	!,
	append(Value, Rest, Values),
	lookup(Key, Alist, Rest).
lookup(Key, [[_|_]|Alist], Values) :-
	lookup(Key, Alist, Values).

edges([], []).
edges([Sexpr|Sexprs], [Edge|Edges]) :-
	edge(Sexpr, Edge),
	edges(Sexprs, Edges).

edge([S0, S1], [N0|N1]) :-
	node(S0, N0),
	node(S1, N1).

node([S, P], [S|P]) :-
	integer(S),
	integer(P).

%% Translate the internal form to an S-expression.

cpsas_to_sexprs([], []).
cpsas_to_sexprs([Cpsa|Cpsas], [Sexpr|Sexprs]) :-
	cpsa_to_sexpr(Cpsa, Sexpr),
	cpsas_to_sexprs(Cpsas, Sexprs).

cpsa_to_sexpr(c(X), [comment|X]).
cpsa_to_sexpr(p(Name, Alg, Roles), [defprotocol, Name, Alg|Rest]) :-
	roles_to_sexprs(Roles, Rest).
cpsa_to_sexpr(k(Prot, Decls, Strands, Precedes, Nons, Uniqs),
	[defskeleton, Prot, [vars|Vars]|Rest]) :-
	decls(Vars, Decls),
	preds(Precedes, Nons, Uniqs, Alist),
	strands_to_sexprs(Strands, Alist, Rest).

preds([], Nons, Uniqs, Alist) :-
	!,
	origs(Nons, Uniqs, Alist).
preds(Precedes, Nons, Uniqs, [[precedes|Sexprs]|Alist]) :-
	edges(Sexprs, Precedes),
	origs(Nons, Uniqs, Alist).

strands_to_sexprs([], Alist, Alist).
strands_to_sexprs([Strand|Strands], Alist, [Sexpr|Sexprs]) :-
	sexpr_to_strand(Sexpr, Strand),
	strands_to_sexprs(Strands, Alist, Sexprs).

roles_to_sexprs([], []).
roles_to_sexprs([Role|Roles], [Sexpr|Sexprs]) :-
	role_to_sexpr(Role, Sexpr),
	roles_to_sexprs(Roles, Sexprs).

role_to_sexpr(r(Name, Decls, Trace, Nons, Uniqs),
	[defrole, Name, [vars|Vars], [trace|Trace]|Alist]) :-
	decls(Vars, Decls),
	origs(Nons, Uniqs, Alist).

origs([], Uniqs, Alist) :-
	!,
	uniq_origs(Uniqs, Alist).
origs(Nons, Uniqs, [['non-orig'|Nons]|Alist]) :-
	uniq_origs(Uniqs, Alist).

uniq_origs([], []) :- !.
uniq_origs(Uniqs, [['uniq-orig'|Uniqs]]).
