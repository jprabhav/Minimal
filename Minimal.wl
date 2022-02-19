(* ::Package:: *)

(* :Name: Minimal` *)


(* :Title: A minimal working package to evalute and check CWI for four point functions. *)


(* :Author: Prabhav Jain and Bibhut Chandan Sahoo *)


(* :Summary:
     This package is a stripped down and modified version of the TripleK package (arXiv:2005.10841 [hep-th]). All code related to the evaluation of Triple-K integrals has been 
     trimmed and the functionality for the Contract and Diff functions is extended to accomodate two sets of momenta (p and q) for n=4. A primitive support for spinorial derivatives is added too.*)


(* :Package Version: 0.4 *)


(* :History:
     Version 0.4 by Prabhav Jain, Bibhut Chandan Sahoo, September 2021.
*)


(* :Copyright: GNU General Public License v3.0, Prabhav Jain & Bibhut Chandan Sahoo, 2021 *)


(* :Mathematica Version: 12.3 *)


BeginPackage["Minimal`", "Global`"];
Unprotect @@ Names["Minimal`*"];
ClearAll @@ Names["Minimal`*"];


(* ::Section:: *)
(*Interface*)


(* ::Text:: *)
(*Definitions.*)


p::usage = "\!\(\*SubsuperscriptBox[
StyleBox[\"p,q\",\nFontSlant->\"Italic\"], 
StyleBox[\"j\",\nFontSlant->\"Italic\"], \(\[Mu]\)]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2 represent external momenta.
\!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2,3,4 represent magnitudes of vectors \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\), \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\), and \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(3\), \(\[Mu]\)]\)=-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\)-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\).";
q::usage = "\!\(\*SubsuperscriptBox[
StyleBox[\"p,q\",\nFontSlant->\"Italic\"], 
StyleBox[\"j\",\nFontSlant->\"Italic\"], \(\[Mu]\)]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2 represent external momenta.
\!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2,3,4 represent magnitudes of vectors \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\), \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\), and \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(3\), \(\[Mu]\)]\)=-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\)-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\).";
\[Lambda]::usage = "\!\(\*SubsuperscriptBox[
StyleBox[\"p,q\",\nFontSlant->\"Italic\"], 
StyleBox[\"j\",\nFontSlant->\"Italic\"], \(\[Mu]\)]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2 represent external momenta.
\!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2,3,4 represent magnitudes of vectors \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\), \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\), and \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(3\), \(\[Mu]\)]\)=-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\)-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\).";
\[Delta]::usage = "\!\(\*SubsuperscriptBox[
StyleBox[\"p,q\",\nFontSlant->\"Italic\"], 
StyleBox[\"j\",\nFontSlant->\"Italic\"], \(\[Mu]\)]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2 represent external momenta.
\!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\) for \!\(\*
StyleBox[\"j\",\nFontSlant->\"Italic\"]\)=1,2,3,4 represent magnitudes of vectors \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\), \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\), and \!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(3\), \(\[Mu]\)]\)=-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(1\), \(\[Mu]\)]\)-\!\(\*SubsuperscriptBox[
StyleBox[\"p\",\nFontSlant->\"Italic\"], \(2\), \(\[Mu]\)]\).";



(* ::Text:: *)
(*Simplifications.*)


Swap::usage = "Swap[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"x\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"y\",\nFontSlant->\"Italic\"]\)] swaps \!\(\*
StyleBox[\"x\",\nFontSlant->\"Italic\"]\) and \!\(\*
StyleBox[\"y\",\nFontSlant->\"Italic\"]\) in \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\).";


(* ::Text:: *)
(*Momentum manipulations.*)


Contract::usage = "Contract[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\)] contracts all repeated indices of recognized vectors in \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\".\",\nFontSlant->\"Italic\"]\)
Contract[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \[Mu]] contracts all repeated \[Mu]'s in \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\).
Contract[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), {\[Mu], ...}] contracts all repreated \[Mu], ... in \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\).
Contract[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \[Mu], \[Nu]] contracts indices \[Mu] and \[Nu] in \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\).";


Contract::argbt = "`1` called with `2` arguments; between `3` and `4` arguments are expected.";


Diff::usage = "Diff[\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"p\",\nFontSlant->\"Italic\"]\), \[Mu]] calculates the derivative of \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\) with respect to \!\(\*
StyleBox[\"p\",\nFontSlant->\"Italic\"]\)[\[Mu]].";
Diff\[Lambda]::usage = "Diff[\!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"p\",\nFontSlant->\"Italic\"]\), \[Mu]] calculates the derivative of \!\(\*
StyleBox[\"f\",\nFontSlant->\"Italic\"]\) with respect to \!\(\*
StyleBox[\"p\",\nFontSlant->\"Italic\"]\)[\[Mu]].";


Diff::darg = "Cannot differentiate over the variable `1`, which appears as the integration variable in `2`.";


(* ::Text:: *)
(*Conformal operators.*)


ScalarKOp::usage = "KOp[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"p\",\nFontSlant->\"Italic\"]\), \[Beta]] applies single K\!\(\*
StyleBox[\"(\",\nFontSize->12,\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"\[Beta]\",\nFontSize->12,\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSize->12,\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSize->12]\) operator to \!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\) with respect to momentum magnitude \!\(\*
StyleBox[\"p\",\nFontSlant->\"Italic\"]\) and with parameter \[Beta].";
ScalarKKOp::usage = "KKOp[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"i\"],\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"i\",\nFontSlant->\"Italic\"]]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"j\",\nFontSlant->\"Italic\"]]\)] = KOp[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"i\"],\nFontSlant->\"Italic\"]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"i\",\nFontSlant->\"Italic\"]]\)] - KOp[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"j\",\nFontSlant->\"Italic\"]]\)] applies the conformal Ward identity operator in its scalar form.";
a::usage = "KKOp[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"i\"],\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"i\",\nFontSlant->\"Italic\"]]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"j\",\nFontSlant->\"Italic\"]]\)] = KOp[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"i\"],\nFontSlant->\"Italic\"]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"i\",\nFontSlant->\"Italic\"]]\)] - KOp[\!\(\*
StyleBox[\"expr\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[SubscriptBox[\"p\", \"j\"],\nFontSlant->\"Italic\"]\), \!\(\*SubscriptBox[\(\[Beta]\), 
StyleBox[\"j\",\nFontSlant->\"Italic\"]]\)] applies the conformal Ward identity operator in its scalar form.";


(* ::Text:: *)
(*Options.*)


Options[Contract] = { Dimension -> d, Vectors -> Automatic, Indices -> All };


Begin["Minimal`Private`"];


(* ::Section:: *)
(*Definitions*)


Attributes[p] = { NHoldAll };
Attributes[q] = { NHoldAll };
Attributes[l] = { NHoldAll };
Attributes[lb] = { NHoldAll };
Attributes[\[Delta]] = { Orderless };
Attributes[\[Lambda]] = { Orderless };
Attributes[CenterDot] = { Orderless, OneIdentity };


$Assumptions = If[$Assumptions === True, 
	{ p[1]>0, p[2]>0, p[3]>0, p[4]>0, q[1]>0, q[2]>0, q[3]>0, q[4]>0 }, 
	Union[$Assumptions, { p[1]>0, p[2]>0, p[3]>0, p[4]>0, q[1]>0, q[2]>0, q[3]>0, q[4]>0 }]];


Format[p[j_][\[Mu]_]] := Subsuperscript[p,j,\[Mu]];
Format[p[j_]] := Subscript[p,j];
Format[q[j_][\[Mu]_]] := Subsuperscript[q,j,\[Mu]];
Format[q[j_]] := Subscript[q,j];
Format[l[j_][\[Mu]_]] := Subsuperscript[\[Lambda],j,\[Mu]];
Format[l[j_]] := Subscript[\[Lambda],j];
Format[lb[j_][\[Mu]_]] := Subsuperscript[OverBar[\[Lambda]],j,\[Mu]];
Format[lb[j_]] := Subscript[OverBar[\[Lambda]],j];
Format[a[i_,j_]]:=\[LeftAngleBracket]i,j\[RightAngleBracket]
Format[a[i_,j_,b]]:=\[LeftAngleBracket]i,OverBar[j]\[RightAngleBracket]
Format[a[i_,b,j_,b]]:=\[LeftAngleBracket]OverBar[i],OverBar[j]\[RightAngleBracket]
Format[\[Delta][i_,j_]] := Subscript[\[Delta],i,j];


(* ::Section::Closed:: *)
(*Simplifications*)


Swap[exp_,toswap1stterm_,toswap2ndterm_] := Module[{toswap3rdterm},((exp/.toswap1stterm->toswap3rdterm)/.toswap2ndterm->toswap1stterm)/.toswap3rdterm->toswap2ndterm];
Swap[x___] := Null /; Message[Swap::argrx, "Swap", Length[{x}], 3];


suMomentumUnformat = { 			 
			Subsuperscript[p, j_, \[Mu]_] :> p[j][\[Mu]],
			Superscript[Subscript[p, j_], \[Mu]_] :> p[j][\[Mu]],
			Subscript[p, j_] :> p[j],
			Superscript[p[j_], \[Mu]_] :> p[j][\[Mu]],
			Subsuperscript[q, j_, \[Mu]_] :> q[j][\[Mu]],
			Superscript[Subscript[q, j_], \[Mu]_] :> q[j][\[Mu]],
			Subscript[q, j_] :> q[j],
			Superscript[q[j_], \[Mu]_] :> q[j][\[Mu]],
			\[LeftAngleBracket]i_,j_\[RightAngleBracket]:>a[i,j],
			\[LeftAngleBracket]i_,OverBar[j_]\[RightAngleBracket]:>a[i,j,b],
			\[LeftAngleBracket]OverBar[i_],OverBar[j_]\[RightAngleBracket]:>a[i,b,j,b],
			Superscript[\[Epsilon],{i_,j_,k_}]:>e[i,j,k],
			Superscript[\[Epsilon],{i_,j_}]:>e[i,j]};


Unformat[exp_] := Module[{tmpNL,tmp\[Lambda],ii}, 
	exp /. { Subscript[i, a_, b_] :> i[a,b],
			 Subsuperscript[p, j_, \[Mu]_] :> p[j][\[Mu]],
			 Superscript[Subscript[p, j_], \[Mu]_] :> p[j][\[Mu]],
			 Subscript[p, j_] :> p[j],
			 Superscript[p[j_], \[Mu]_] :> p[j][\[Mu]],
			 Subsuperscript[q, j_, \[Mu]_] :> q[j][\[Mu]],
			 Superscript[Subscript[q, j_], \[Mu]_] :> q[j][\[Mu]],
			 Subscript[q, j_] :> q[j],
			 Superscript[q[j_], \[Mu]_] :> q[j][\[Mu]],
			 Subscript[\[Delta], i_, j_] :> \[Delta][i,j],
			 \[LeftAngleBracket]i_,j_\[RightAngleBracket]:>a[i,j],
			 \[LeftAngleBracket]i_,OverBar[j_]\[RightAngleBracket]:>a[i,j,b],
			 \[LeftAngleBracket]OverBar[i_],OverBar[j_]\[RightAngleBracket]:>a[i,b,j,b],
			 Superscript[\[Epsilon],{i_,j_,k_}]:>e[i,j,k],
			 Superscript[\[Epsilon],{i_,j_}]:>e[i,j]
		   }
];


Unformat[x___] := Null /; Message[Unformat::argrx, "Unformat", Length[{x}], 1];


suMomentum3Identities[assume___] := {
	c3_. p[3][\[Mu]_] + c1_. p[1][\[Mu]_] :> -c1 p[2][\[Mu]]/; Simplify[c1==c3,assume,TimeConstraint->0.1],
	c3_. p[3][\[Mu]_] + c2_. p[2][\[Mu]_] :> -c2 p[1][\[Mu]]/; Simplify[c2==c3,assume,TimeConstraint->0.1],
	c2_. p[2][\[Mu]_] + c1_. p[1][\[Mu]_] :> -c1 p[3][\[Mu]]/; Simplify[c1==c2,assume,TimeConstraint->0.1]
	 };


suMomentum = {
	(x_+y_)[j_][\[Mu]_] :> x[j][\[Mu]]+y[j][\[Mu]],
	(x_+y_)[j_] :> x[j]+y[j],
	(a_*x_)[j_] :> a*x[j] /; NumericQ[a],
	(-x_)[j_] :> -x[j],
	0[j_] :> 0,
	x_\[CenterDot](y_+z_) :> x\[CenterDot]y + x\[CenterDot]z,
	x_\[CenterDot](a_*y_) :> a(x\[CenterDot]y) /; NumericQ[a],
	x_\[CenterDot](-y_) :> -(x\[CenterDot]y),
	x_\[CenterDot]0 :> 0 };


suMomentum2 = {
	x_[j_][\[Mu]_]+y_[k_][\[Mu]_] :> (x[j]+y[k])[\[Mu]],
	(a_*x_)[j_] :> a*x[j] /; NumericQ[a],
	(-x_)[j_] :> -x[j],
	0[j_] :> 0,
	x_\[CenterDot](y_+z_) :> x\[CenterDot]y + x\[CenterDot]z,
	x_\[CenterDot](a_*y_) :> a(x\[CenterDot]y) /; NumericQ[a],
	x_\[CenterDot](-y_) :> -(x\[CenterDot]y),
	x_\[CenterDot]0 :> 0 };


suMomentumEx[vector_] := { 
	(x_+y_)[j_] :> x[j]+y[j],
	(x_+y_)[j_][\[Mu]_] :> x[j][\[Mu]]+y[j][\[Mu]],
	(a_*x_)[j_] :> a*x[j] /; FreeQ[a,p,q] && FreeQ[a,vector],
	(-x_)[j_] :> -x[j],
	0[j_] :> 0,
	x_\[CenterDot](y_+z_) :> x\[CenterDot]y + x\[CenterDot]z,
	x_\[CenterDot](a_*y_) :> a(x\[CenterDot]y) /; FreeQ[a,p,q] && FreeQ[a,vector],
	x_\[CenterDot](-y_) :> -(x\[CenterDot]y),
	x_\[CenterDot]0 :> 0 };


suMomentumEx[vectors_List] := { 
	(x_+y_)[j_] :> x[j]+y[j],
	(x_+y_)[j_][\[Mu]_] :> x[j][\[Mu]]+y[j][\[Mu]],
	(a_*x_)[j_] :> a*x[j] /; FreeQ[a,p,q] && AllTrue[vectors, FreeQ[a,#]&],
	(-x_)[j_] :> -x[j],
	0[j_] :> 0,
	x_\[CenterDot](y_+z_) :> x\[CenterDot]y + x\[CenterDot]z,
	x_\[CenterDot](a_*y_) :> a(x\[CenterDot]y) /; FreeQ[a,p,q] && AllTrue[vectors, FreeQ[a,#]&],
	x_\[CenterDot](-y_) :> -(x\[CenterDot]y),
	x_\[CenterDot]0 :> 0 };


suProductsToMagnitudes = { 
	Sqrt[x_[n_]\[CenterDot]x_[n_]] :> x[n],
	Sqrt[x_\[CenterDot]x_] -> x,
	Sqrt[x_[n_]^2] :> x[n],
	Sqrt[x_^2] -> x,
	x_[n_]\[CenterDot]x_[n_] :> x[n]^2,
	x_\[CenterDot]x_ -> x^2 };


suMomentum4 = {
	k_[4][\[Mu]_] :> -k[1][\[Mu]]-k[2][\[Mu]]-k[3][\[Mu]],
	k_[4]\[CenterDot]x_ :> -(k[1]\[CenterDot]x)-(k[2]\[CenterDot]x)-(k[3]\[CenterDot]x)
 };


sigmaProduct = {
	\[Sigma][\[Mu]_,\[Alpha]_,\[Beta]_]\[Sigma][\[Nu]_,\[Beta]_,\[Gamma]_]:>\[Delta][\[Mu],\[Nu]]\[Delta][\[Alpha],\[Gamma]]+I e[\[Mu],\[Nu],\[Rho]]\[Sigma][\[Rho],\[Alpha],\[Gamma]]
};


sphFormat[exp_]:= Unformat[exp]/.{ CenterDot[l[i_],l[j_]] :> a[i,j], CenterDot[l[i_],lb[j_]] :> a[i,j,b], CenterDot[lb[i_],lb[j_]] :> a[i,b,j,b]}


(* ::Section::Closed:: *)
(*Momentum manipulations*)


suContract[d_] := {
		\[Delta][i_,j_]\[Delta][j_,k_] :> \[Delta][i,k] /; !NumericQ[j],
		\[Delta][i_,j_]\[Delta][i_,j_] :> d /; !NumericQ[i]&&!NumericQ[j],
		\[Delta][i_,i_] :> d /; !NumericQ[i],
		x_[i_]y_[i_] :> x\[CenterDot]y /; !NumericQ[i],
		\[Delta][i_,j_]x_[j_] :> x[i] /; !NumericQ[j],
		x_[i_]^2 :> x\[CenterDot]x /; !NumericQ[i],
		e[i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_,k_] \[Delta][j_,i_] :> 0,
		e[i_,j_,k_] \[Delta][i_,k_] :> 0,
		e[i_,j_,k_] x_[i_]x_[j_]:> 0,
		e[i_,j_,k_] x_[i_]x_[k_]:> 0,
		e[i_,j_,k_] x_[j_]x_[k_]:> 0,
		e[i_,j_,k_] \[Delta][k_,b_] :> e[i,j,b],
		e[i_,j_,k_] \[Delta][j_,b_] :> -e[i,k,b],
		e[i_,j_,k_] \[Delta][i_,b_] :> e[j,k,b],
		e[j_,k_] \[Delta][j_,k_] :> 0,
		e[j_,k_] \[Delta][k_,j_] :> 0,
		e[i_,j_] x_[i_]x_[j_]:> 0,
		e[i_,j_] \[Delta][j_,b_] :> e[i,b],
		e[i_,j_] \[Delta][i_,b_] :> e[b,j],
		\[Delta][i_,j_]l[k_][i_]:> l[k][j],
		\[Delta][i_,j_]lb[k_][i_]:> lb[k][j],
		\[Sigma][i_,j_,k_]z[m_][i_]:>l[m][j]l[m][k]/(2p[m]),
		\[Sigma][i_,j_,k_]l[m_][k_]lb[m_][j_]:>2p[m][i],
		\[Sigma][i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_] l[k_][i_]:> l[k][j],
		e[i_,j_] lb[k_][i_]:> lb[k][j],
		e[i_,j_] l[k_][j_]:> -l[k][i],
		e[i_,j_] lb[k_][j_]:> -lb[k][i],
		l[i_][\[Mu]_]l[j_][\[Mu]_]:> a[i,j],
		l[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,j,b],
		lb[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,b,j,b],
		l[i_][\[Mu]_]l[i_][\[Mu]_]:> a[i,i],
		l[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,i,b],
		lb[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,b,i,b]
			};


suContractVectors[d_, vects_] := {
		\[Delta][i_,j_]\[Delta][j_,k_] :> \[Delta][i,k] /; !NumericQ[j],
		\[Delta][i_,j_]\[Delta][i_,j_] :> d /; !NumericQ[i]&&!NumericQ[j],
		\[Delta][i_,i_] :> d /; !NumericQ[i],
		x_[i_]y_[i_] :> x\[CenterDot]y /; !NumericQ[i] && MemberQ[vects, x] && MemberQ[vects, y],
		\[Delta][i_,j_]x_[j_] :> x[i] /; !NumericQ[j] && MemberQ[vects, x],
		x_[i_]^2 :> x\[CenterDot]x /; !NumericQ[i] && MemberQ[vects, x],
		e[i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_,k_] \[Delta][j_,i_] :> 0,
		e[i_,j_,k_] \[Delta][i_,k_] :> 0,
		e[i_,j_,k_] x_[i_]x_[j_]:> 0,
		e[i_,j_,k_] x_[i_]x_[k_]:> 0,
		e[i_,j_,k_] x_[j_]x_[k_]:> 0,
		e[i_,j_,k_] \[Delta][k_,b_] :> e[i,j,b],
		e[i_,j_,k_] \[Delta][j_,b_] :> -e[i,k,b],
		e[i_,j_,k_] \[Delta][i_,b_] :> e[j,k,b],
		e[j_,k_] \[Delta][j_,k_] :> 0,
		e[j_,k_] \[Delta][k_,j_] :> 0,
		e[i_,j_] x_[i_]x_[j_]:> 0,
		e[i_,j_] \[Delta][j_,b_] :> e[i,b],
		e[i_,j_] \[Delta][i_,b_] :> e[b,j],
		\[Delta][i_,j_]l[k_][i_]:> l[k][j],
		\[Delta][i_,j_]lb[k_][i_]:> lb[k][j],
		\[Sigma][i_,j_,k_]z[m_][i_]:>l[m][j]l[m][k]/(2p[m]),
		\[Sigma][i_,j_,k_]l[m_][k_]lb[m_][j_]:>2p[m][i],
		\[Sigma][i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_] l[k_][i_]:> l[k][j],
		e[i_,j_] lb[k_][i_]:> lb[k][j],
		e[i_,j_] l[k_][j_]:> -l[k][i],
		e[i_,j_] lb[k_][j_]:> -lb[k][i],
		l[i_][\[Mu]_]l[j_][\[Mu]_]:> a[i,j],
		l[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,j,b],
		lb[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,b,j,b],
		l[i_][\[Mu]_]l[i_][\[Mu]_]:> a[i,i],
		l[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,i,b],
		lb[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,b,i,b]
			};


suContractIndices[d_, js_List] := {
		\[Delta][i_,j_]\[Delta][j_,k_] :> \[Delta][i,k] /; MemberQ[js,j],
		\[Delta][i_,j_]^2 :> \[Delta][i,i] /; MemberQ[js,j],
		\[Delta][j_,j_] :> d /; MemberQ[js,j],
		x_[j_]y_[j_] :> x\[CenterDot]y /; MemberQ[js,j],
		\[Delta][i_,j_]x_[j_] :> x[i] /; MemberQ[js,j],
		x_[j_]^2 :> x\[CenterDot]x  /; MemberQ[js,j],
		e[i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_,k_] \[Delta][j_,i_] :> 0,
		e[i_,j_,k_] \[Delta][i_,k_] :> 0,
		e[i_,j_,k_] x_[i_]x_[j_]:> 0,
		e[i_,j_,k_] x_[i_]x_[k_]:> 0,
		e[i_,j_,k_] x_[j_]x_[k_]:> 0,
		e[i_,j_,k_] \[Delta][k_,b_] :> e[i,j,b],
		e[i_,j_,k_] \[Delta][j_,b_] :> -e[i,k,b],
		e[i_,j_,k_] \[Delta][i_,b_] :> e[j,k,b],
		e[j_,k_] \[Delta][j_,k_] :> 0,
		e[j_,k_] \[Delta][k_,j_] :> 0,
		e[i_,j_] x_[i_]x_[j_]:> 0,
		e[i_,j_] \[Delta][j_,b_] :> e[i,b],
		e[i_,j_] \[Delta][i_,b_] :> e[b,j],
		\[Delta][i_,j_]l[k_][i_]:> l[k][j],
		\[Delta][i_,j_]lb[k_][i_]:> lb[k][j],
		\[Sigma][i_,j_,k_]z[m_][i_]:>l[m][j]l[m][k]/(2p[m]),
		\[Sigma][i_,j_,k_]l[m_][k_]lb[m_][j_]:>2p[m][i],
		\[Sigma][i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_] l[k_][i_]:> l[k][j],
		e[i_,j_] lb[k_][i_]:> lb[k][j],
		e[i_,j_] l[k_][j_]:> -l[k][i],
		e[i_,j_] lb[k_][j_]:> -lb[k][i],
		l[i_][\[Mu]_]l[j_][\[Mu]_]:> a[i,j],
		l[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,j,b],
		lb[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,b,j,b],
		l[i_][\[Mu]_]l[i_][\[Mu]_]:> a[i,i],
		l[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,i,b],
		lb[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,b,i,b]
			};


suContractIndices[d_, j_] := {
		\[Delta][i_,j]\[Delta][j,k_] :> \[Delta][i,k],
		\[Delta][i_,j]^2 :> \[Delta][i,i],
		\[Delta][j,j] :> d,
		x_[j]y_[j] :> x\[CenterDot]y,
		\[Delta][i_,j]x_[j] :> x[i],
		x_[j]^2 :> x\[CenterDot]x,
		e[i_,j,k_] \[Delta][j,k_] :> 0,
		e[i_,j,k_] \[Delta][j,i_] :> 0,
		e[i_,j,k_] \[Delta][i_,k_] :> 0,
		e[i_,j,k_] x_[i_]x_[j]:> 0,
		e[i_,j,k_] x_[i_]x_[k_]:> 0,
		e[i_,j,k_] x_[j]x_[k_]:> 0,
		e[i_,j,k_] \[Delta][k_,b_] :> e[i,j,b],
		e[i_,j,k_] \[Delta][j,b_] :> -e[i,k,b],
		e[i_,j,k_] \[Delta][i_,b_] :> e[j,k,b],
		e[j,k_] \[Delta][j,k_] :> 0,
		e[j,k_] \[Delta][k_,j] :> 0,
		e[i_,j] x_[i_]x_[j]:> 0,
		e[i_,j] \[Delta][j,b_] :> e[i,b],
		e[i_,j] \[Delta][i_,b_] :> e[b,j],
		\[Delta][i_,j]l[k_][i_]:> l[k][j],
		\[Delta][i_,j]lb[k_][i_]:> lb[k][j],
		\[Sigma][i_,j,k_]z[m_][i_]:>l[m][j]l[m][k]/(2p[m]),
		\[Sigma][i_,j,k_]l[m_][k_]lb[m_][j]:>2p[m][i],
		\[Sigma][i_,j,k_] \[Delta][j,k_] :> 0,
		e[i_,j] l[k_][i_]:> l[k][j],
		e[i_,j] lb[k_][i_]:> lb[k][j],
		e[i_,j] l[k_][j]:> -l[k][i],
		e[i_,j] lb[k_][j]:> -lb[k][i],
		l[i_][\[Mu]_]l[j][\[Mu]_]:> a[i,j],
		l[i_][\[Mu]_]lb[j][\[Mu]_]:> a[i,j,b],
		lb[i_][\[Mu]_]lb[j][\[Mu]_]:> a[i,b,j,b],
		l[i_][\[Mu]_]l[i_][\[Mu]_]:> a[i,i],
		l[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,i,b],
		lb[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,b,i,b]
			};


suContractVectorsIndices[d_, vects_, js_List] := {
		\[Delta][i_,j_]\[Delta][j_,k_] :> \[Delta][i,k] /; MemberQ[js,j],
		\[Delta][i_,j_]^2 :> \[Delta][i,i] /; MemberQ[js,j],
		\[Delta][j_,j_] :> d /; MemberQ[js,j],
		x_[j_]y_[j_] :> x\[CenterDot]y /; MemberQ[js,j] && MemberQ[vects, x] && MemberQ[vects, y],
		\[Delta][i_,j_]x_[j_] :> x[i] /; MemberQ[js,j] && MemberQ[vects, x],
		x_[j_]^2 :> x\[CenterDot]x  /; MemberQ[js,j] && MemberQ[vects, x],
		e[i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_,k_] \[Delta][j_,i_] :> 0,
		e[i_,j_,k_] \[Delta][i_,k_] :> 0,
		e[i_,j_,k_] x_[i_]x_[j_]:> 0,
		e[i_,j_,k_] x_[i_]x_[k_]:> 0,
		e[i_,j_,k_] x_[j_]x_[k_]:> 0,
		e[i_,j_,k_] \[Delta][k_,b_] :> e[i,j,b],
		e[i_,j_,k_] \[Delta][j_,b_] :> -e[i,k,b],
		e[i_,j_,k_] \[Delta][i_,b_] :> e[j,k,b],
		e[j_,k_] \[Delta][j_,k_] :> 0,
		e[j_,k_] \[Delta][k_,j_] :> 0,
		e[i_,j_] x_[i_]x_[j_]:> 0,
		e[i_,j_] \[Delta][j_,b_] :> e[i,b],
		e[i_,j_] \[Delta][i_,b_] :> e[b,j],
		\[Delta][i_,j_]l[k_][i_]:> l[k][j],
		\[Delta][i_,j_]lb[k_][i_]:> lb[k][j],
		\[Sigma][i_,j_,k_]z[m_][i_]:>l[m][j]l[m][k]/(2p[m]),
		\[Sigma][i_,j_,k_]l[m_][k_]lb[m_][j_]:>2p[m][i],
		\[Sigma][i_,j_,k_] \[Delta][j_,k_] :> 0,
		e[i_,j_] l[k_][i_]:> l[k][j],
		e[i_,j_] lb[k_][i_]:> lb[k][j],
		e[i_,j_] l[k_][j_]:> -l[k][i],
		e[i_,j_] lb[k_][j_]:> -lb[k][i],
		l[i_][\[Mu]_]l[j_][\[Mu]_]:> a[i,j],
		l[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,j,b],
		lb[i_][\[Mu]_]lb[j_][\[Mu]_]:> a[i,b,j,b],
		l[i_][\[Mu]_]l[i_][\[Mu]_]:> a[i,i],
		l[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,i,b],
		lb[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,b,i,b]
			};


suContractVectorsIndices[d_, vects_, j_] := {
		\[Delta][i_,j]\[Delta][j,k_] :> \[Delta][i,k],
		\[Delta][i_,j]^2 :> \[Delta][i,i],
		\[Delta][j,j] :> d,
		x_[j]y_[j] :> x\[CenterDot]y /; MemberQ[vects, x] && MemberQ[vects, y],
		\[Delta][i_,j]x_[j] :> x[i] /; MemberQ[vects, x],
		x_[j]^2 :> x\[CenterDot]x /; MemberQ[vects, x],
		e[i_,j,k_] \[Delta][j,k_] :> 0,
		e[i_,j,k_] \[Delta][j,i_] :> 0,
		e[i_,j,k_] \[Delta][i_,k_] :> 0,
		e[i_,j,k_] x_[i_]x_[j]:> 0,
		e[i_,j,k_] x_[i_]x_[k_]:> 0,
		e[i_,j,k_] x_[j]x_[k_]:> 0,
		e[i_,j,k_] \[Delta][k_,b_] :> e[i,j,b],
		e[i_,j,k_] \[Delta][j,b_] :> -e[i,k,b],
		e[i_,j,k_] \[Delta][i_,b_] :> e[j,k,b],
		e[j,k_] \[Delta][j,k_] :> 0,
		e[j,k_] \[Delta][k_,j] :> 0,
		e[i_,j] x_[i_]x_[j]:> 0,
		e[i_,j] \[Delta][j,b_] :> e[i,b],
		e[i_,j] \[Delta][i_,b_] :> e[b,j],
		\[Delta][i_,j]l[k_][i_]:> l[k][j],
		\[Delta][i_,j]lb[k_][i_]:> lb[k][j],
		\[Sigma][i_,j,k_]z[m_][i_]:>l[m][j]l[m][k]/(2p[m]),
		\[Sigma][i_,j,k_]l[m_][k_]lb[m_][j]:>2p[m][i],
		\[Sigma][i_,j,k_] \[Delta][j,k_] :> 0,
		e[i_,j] l[k_][i_]:> l[k][j],
		e[i_,j] lb[k_][i_]:> lb[k][j],
		e[i_,j] l[k_][j]:> -l[k][i],
		e[i_,j] lb[k_][j]:> -lb[k][i],
		l[i_][\[Mu]_]l[j][\[Mu]_]:> a[i,j],
		l[i_][\[Mu]_]lb[j][\[Mu]_]:> a[i,j,b],
		lb[i_][\[Mu]_]lb[j][\[Mu]_]:> a[i,b,j,b],
		l[i_][\[Mu]_]l[i_][\[Mu]_]:> a[i,i],
		l[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,i,b],
		lb[i_][\[Mu]_]lb[i_][\[Mu]_]:> a[i,b,i,b]
			};


ContractOnce[exp_, vects_, dim_, idx_] :=
	Expand[exp] /. 
		If[!FreeQ[exp, LoopIntegral], {
			LoopIntegral[d_,\[Delta]s_][num_,k_] :> LoopIntegral[d,\[Delta]s][
				ContractOnce[num, Union[vects,{k}], dim, idx], k],
			LoopIntegral[d_,\[Delta]s_][num_,k_,p_] :> LoopIntegral[d,\[Delta]s][
				ContractOnce[num, Union[vects,{k}], dim, idx], k, p]
		}, {}] //.
		If[idx === All,
			suContractVectors[dim, vects],
			suContractVectorsIndices[dim, vects, idx]
		];


DoContract[exp_, dim_, vects_, idx_] := 
	If[vects === All,
		If[idx === All,
			Expand[Unformat[exp]] //. suContract[dim],
			Expand[Unformat[exp]] //. suContractIndices[dim, idx]
		],
		ContractOnce[
			Unformat[exp], 
			If[ListQ[vects], Union[vects, {p, p[1],p[2],p[3],p[4], q, q[1],q[2],q[3],q[4]}], {vects,p, p[1],p[2],p[3],p[4], q, q[1],q[2],q[3],q[4]}], 
			dim, 
			idx
		]
	] /. suProductsToMagnitudes;


fcall:Contract[exp_, opts___?OptionQ] := Module[{ValidOpts, dim, vects, idx},
	ValidOpts = First /@ Options[Contract];
	Scan[If[!MemberQ[ValidOpts, First[#]],
		Message[Contract::optx, ToString[First[#]], ToString[Unevaluated[fcall]]]]&, Flatten[{opts}]];
	{dim, vects, idx} = {Dimension, Vectors, Indices} /. Flatten[{opts}] /. Options[Contract];
	DoContract[exp, dim, If[vects === Automatic, If[idx === All, {}, All], vects], idx]
];


fcall:Contract[exp_, \[Mu]_, opts___?OptionQ] := Module[{ValidOpts, dim, vects},
	ValidOpts = First /@ Options[Contract];
	Scan[If[!MemberQ[ValidOpts, First[#]],
		Message[Contract::optx, ToString[First[#]], ToString[Unevaluated[fcall]]]]&, Flatten[{opts}]];
	{dim, vects} = {Dimension, Vectors} /. Flatten[{opts}] /. Options[Contract];
	DoContract[exp, dim, If[vects === Automatic, All, vects], \[Mu]]
];


fcall:Contract[exp_, \[Mu]_, \[Nu]_, opts___?OptionQ] := Module[{ValidOpts, dim, vects},
	ValidOpts = First /@ Options[Contract];
	Scan[If[!MemberQ[ValidOpts, First[#]],
		Message[Contract::optx, ToString[First[#]], ToString[Unevaluated[fcall]]]]&, Flatten[{opts}]];
	{dim, vects} = {Dimension, Vectors} /. Flatten[{opts}] /. Options[Contract];
	DoContract[exp /. \[Nu]->\[Mu], dim, If[vects === Automatic, All, vects], \[Mu]]
];


Contract[exp_List, opts___?OptionQ] := Contract[#,opts]& /@ exp;
Contract[exp_List, \[Mu]_, opts___?OptionQ] := Contract[#,\[Mu],opts]& /@ exp;
Contract[exp_List, \[Mu]_, \[Nu]_, opts___?OptionQ] := Contract[#,\[Mu],\[Nu],opts]& /@ exp;


Contract[exp_SeriesData, opts___?OptionQ] :=
	SeriesData[exp[[1]],exp[[2]], Contract[#,opts]& /@ (exp[[3]]), exp[[4]],exp[[5]],exp[[6]]];
Contract[exp_SeriesData, \[Mu]_, opts___?OptionQ] :=
	SeriesData[exp[[1]],exp[[2]], Contract[#,\[Mu],opts]& /@ (exp[[3]]), exp[[4]],exp[[5]],exp[[6]]];
Contract[exp_SeriesData, \[Mu]_, \[Nu]_, opts___?OptionQ] :=
	SeriesData[exp[[1]],exp[[2]], Contract[#,\[Mu],\[Nu],opts]& /@ (exp[[3]]), exp[[4]],exp[[5]],exp[[6]]];


fcall:Contract[exp_, k___] := Null /; Message[Contract::nonopt, Last[{k}], 1, ToString[Unevaluated[fcall]]];
Contract[] := Null /; Message[Contract::argbt, "Contract", 0, 1, 3];


(* ::Section::Closed:: *)
(*Derivative operators*)


OldDiff[exp_, k_[mom_], idx_] /; mom===1 || mom===2 || mom===3 || mom===4 :=
Module[{mag, Vtemp, Ltemp, L4temp,testexp},
	(*mag = !FreeQ[exp, M];*)
	D[Unformat[exp]
		(*/. If[mag,  { 
				M[exprs_] :> If[Length[exprs/.{-p[j_]->p[j]}]==1,
									exprs/.{-p[j_]->p[j]},
									Sqrt[Contract[Sum[exprs[[j]][\[Alpha]]/.{(-p[m_])[\[Alpha]]->-p[m][\[Alpha]]},{j,Length[exprs]}]Sum[exprs[[j]][\[Alpha]]/.{(-p[m_])[\[Alpha]]->-p[m][\[Alpha]]},{j,Length[exprs]}]]]
								  ]
			}, {}]*)
		//.suMomentum4
		//.suMomentum
		/. { k[mom][i_] :> Vtemp[i] }
		/. { k[mom] -> Ltemp, k[4] -> L4temp }
		//.{ Ltemp\[CenterDot]x_ :> k[mom]\[CenterDot]x }
		/.{ Ltemp -> Ltemp[k[mom]], 
			 Vtemp[i_] :> Vtemp[k[mom],i],
			 L4temp -> L4temp[k[mom]] },
		k[mom]]
		/. { 
			Derivative[1][Ltemp][k[mom]] -> k[mom][idx] / k[mom],
			Derivative[1][L4temp][k[mom]] -> (k[1][idx] + k[2][idx]+k[3][idx]) / k[4],
			Derivative[1,0][Vtemp][k[mom],i_] :> \[Delta][idx,i],
			Derivative[0,1][CenterDot][x_, y_] :> x[idx],
			Derivative[1,0][CenterDot][x_, y_] :> y[idx],
			Ltemp[k[mom]] -> k[mom],
			L4temp[k[mom]] -> k[4],
			Vtemp[k[mom],i_] :> k[mom][i]
		   }
		 (*/. If[mag, {
		    Derivative[1][M[x_]][k[mom]] -> 0
			}, {}]*)
];


Diff\[Lambda][exp_, mom_, idx_] := Module[{Vtemp, Ltemp, ptemp, itemp},
	D[exp
		/. { a[i_,j_] :> CenterDot[l[i],l[j]]}
		/. { a[i_,j_,b] :> CenterDot[l[i],lb[j]]}
		/. { a[i_,b,j_,b] :> CenterDot[lb[i],lb[j]]}
		/. { mom[i_] :> Vtemp[mom,i] }
		/. { mom :> Ltemp[mom] }
		//.{ Vtemp[Ltemp[mom],i_] :> Vtemp[mom,i], Ltemp[mom]\[CenterDot]x_ :> mom\[CenterDot]x },
		mom]
		/. { 
			Derivative[1][Ltemp][mom] :> (1/2)\[Sigma][\[Rho],\[Gamma],idx]lb[mom][\[Gamma]]p[mom][\[Rho]]/p,
			Derivative[1,0][Vtemp][mom,i_] :> \[Delta][idx,i],
			Derivative[0,1][CenterDot][x_, y_] :> x[idx],
			Derivative[1,0][CenterDot][x_, y_] :> y[idx],
			Ltemp[mom] :> mom,
			Vtemp[mom,i_] :> mom[i]
			 }
]//sphFormat;


OldDiff[exp_, mom_, idx_] := Module[{Vtemp, Ltemp},
	D[exp
		//.suMomentumEx[mom]
		/. { mom[i_] :> Vtemp[mom,i] }
		/. { mom :> Ltemp[mom] }
		//.{ Vtemp[Ltemp[mom],i_] :> Vtemp[mom,i], Ltemp[mom]\[CenterDot]x_ :> mom\[CenterDot]x },
		mom]
		/. { 
			Derivative[1][Ltemp][mom] :> mom[idx] / mom,
			Derivative[1,0][Vtemp][mom,i_] :> \[Delta][idx,i],
			Derivative[0,1][CenterDot][x_, y_] :> x[idx],
			Derivative[1,0][CenterDot][x_, y_] :> y[idx],
			Ltemp[mom] :> mom,
			Vtemp[mom,i_] :> mom[i]
			 }
];


DiffOnce[exp_, q_[mom_], idx_, ks_] /; mom===1 || mom===2 || mom===3 || mom===4  := 
Module[{loopint, Vtemp, Ltemp, L4temp, ptemp, itemp},
	loopint = !FreeQ[exp, LoopIntegral];
	D[exp
		/. If[loopint,  { 
				LoopIntegral[d_, {\[Delta]1_,\[Delta]2_,\[Delta]3_}][num_,k_] :> 
					LoopIntegral[d, {\[Delta]1,\[Delta]2,\[Delta]3}][num /. q->ptemp, k][Vtemp[itemp]],
				LoopIntegral[d_, {\[Delta]1_,\[Delta]2_}][num_,k_,pp_] :> 
					LoopIntegral[d, {\[Delta]1,\[Delta]2}][num /. q->ptemp, k, pp /. q->ptemp][Vtemp[itemp]]
			}, {}]
		//.suMomentum4
		//.suMomentumEx[ks]
		/. { q[mom][i_] :> Vtemp[i] }
		/. { q[mom] -> Ltemp, q[4] -> L4temp }
		//.{ Ltemp\[CenterDot]x_ :> q[mom]\[CenterDot]x }
		/. { Ltemp -> Ltemp[q[mom]], 
			 Vtemp[i_] :> Vtemp[q[mom],i],
			 L4temp -> L4temp[q[mom]] }, 
		q[mom]]
		/. { 
			Derivative[1][Ltemp][q[mom]] -> q[mom][idx] / q[mom],
			Derivative[1][L4temp][q[mom]] -> (q[1][idx] + q[2][idx]+q[3][idx]) / q[4],
			Derivative[1,0][Vtemp][q[mom],i_] :> \[Delta][idx,i],
			Derivative[0,1][CenterDot][x_, y_] :> x[idx],
			Derivative[1,0][CenterDot][x_, y_] :> y[idx],
			Ltemp[q[mom]] -> q[mom],
			L4temp[q[mom]] -> q[4],
			Vtemp[q[mom],i_] :> q[mom][i]
			 }
		/. If[loopint, {
 			\[Delta][idx, itemp] -> 1,
				Derivative[1][LoopIntegral[d_, {\[Delta]1_,\[Delta]2_,\[Delta]3_}][num_,k_]][p[mom][itemp]] :>
					  If[mom===1, +2\[Delta]2 LoopIntegral[d, {\[Delta]1,\[Delta]2+1,\[Delta]3}][num*(k[idx]-p[1][idx]) /. ptemp->p, k], 0]
					+ If[mom===2, -2\[Delta]1 LoopIntegral[d, {\[Delta]1+1,\[Delta]2,\[Delta]3}][num*(p[2][idx]+k[idx]) /. ptemp->p, k], 0]
					+ LoopIntegral[d, {\[Delta]1,\[Delta]2,\[Delta]3}][DiffOnce[num /. ptemp->p, p[mom], idx, Union[ks, {k}]], k],
				Derivative[1][LoopIntegral[d_, {\[Delta]1_,\[Delta]2_}][num_,k_,pp_]][p[mom][itemp]] :>
					+ 2\[Delta]2 LoopIntegral[d, {\[Delta]1,\[Delta]2+1}][num*(k[idx]-pp[idx]) 
									/. ptemp->p 
									/. suMomentum4
									//.suMomentumEx[Union[ks, {k}]], 
							k, pp /. ptemp->p] * D[pp /. ptemp->p /. p[4]->-p[1]-p[2]-p[3], p[mom]]
					+ LoopIntegral[d, {\[Delta]1,\[Delta]2}][DiffOnce[num /. ptemp->p, p[mom], idx, Union[ks, {k}]], k, pp /. ptemp->p],
				LoopIntegral[d_,{\[Delta]1_,\[Delta]2_,\[Delta]3_}][num_, k_][p[mom][itemp]] :> 
					LoopIntegral[d,{\[Delta]1,\[Delta]2,\[Delta]3}][num /. ptemp->p, k],
				LoopIntegral[d_,{\[Delta]1_,\[Delta]2_}][num_, k_, pp_][p[mom][itemp]] :> 
					LoopIntegral[d,{\[Delta]1,\[Delta]2}][num /. ptemp->p, k, pp /. ptemp->p]
			}, {}]
		/. { LoopIntegral[x__][0, y__] :> 0 }
];


DiffOnce[exp_, mom_, idx_, ks_] := Module[{loopint, Vtemp, Ltemp, ptemp, itemp},
	loopint = !FreeQ[exp, LoopIntegral];
	D[exp
		//.suMomentumEx[Flatten[{mom,ks}]]
		/. If[loopint, {
				LoopIntegral[d_, {\[Delta]1_,\[Delta]2_,\[Delta]3_}][num_,k_] :> 
					LoopIntegral[d, {\[Delta]1,\[Delta]2,\[Delta]3}][num /. mom->ptemp, k][Vtemp[mom, itemp]],
				LoopIntegral[d_, {\[Delta]1_,\[Delta]2_}][num_,k_,pp_] :> 
					LoopIntegral[d, {\[Delta]1,\[Delta]2}][num /. mom->ptemp, k, pp /. mom->ptemp][Vtemp[mom, itemp]]
			}, {}]
		/. { mom[i_] :> Vtemp[mom,i] }
		/. { mom :> Ltemp[mom] }
		//.{ Vtemp[Ltemp[mom],i_] :> Vtemp[mom,i], Ltemp[mom]\[CenterDot]x_ :> mom\[CenterDot]x },
		mom]
		/. { 
			Derivative[1][Ltemp][mom] :> mom[idx] / mom,
			Derivative[1,0][Vtemp][mom,i_] :> \[Delta][idx,i],
			Derivative[0,1][CenterDot][x_, y_] :> x[idx],
			Derivative[1,0][CenterDot][x_, y_] :> y[idx],
			Ltemp[mom] :> mom,
			Vtemp[mom,i_] :> mom[i]
			 }
		/. If[loopint, {
				\[Delta][idx, itemp] -> 1,
				Derivative[1][LoopIntegral[d_, {\[Delta]1_,\[Delta]2_,\[Delta]3_}][num_,k_]][mom[itemp]] :>
					LoopIntegral[d, {\[Delta]1,\[Delta]2,\[Delta]3}][DiffOnce[num /. ptemp->mom, mom, idx, Union[ks, {k}]], k],
				Derivative[1][LoopIntegral[d_, {\[Delta]1_,\[Delta]2_}][num_,k_,pp_]][mom[itemp]] :>
					+ 2\[Delta]2 LoopIntegral[d, {\[Delta]1,\[Delta]2+1}][num*(k[idx]-pp[idx]) 
										/. ptemp->mom 
										//.suMomentumEx[Union[ks, {k}]], 
							k, pp /. ptemp->mom] * D[pp /. ptemp->mom, mom]
					+ LoopIntegral[d, {\[Delta]1,\[Delta]2}][DiffOnce[num /. ptemp->mom, mom, idx, Union[ks, {k}]], k, pp /. ptemp->mom],
				LoopIntegral[d_,{\[Delta]1_,\[Delta]2_,\[Delta]3_}][num_, k_][mom[itemp]] :> 
					LoopIntegral[d,{\[Delta]1,\[Delta]2,\[Delta]3}][num /. ptemp->mom, k],
				LoopIntegral[d_,{\[Delta]1_,\[Delta]2_}][num_, k_, pp_][mom[itemp]] :> 
					LoopIntegral[d,{\[Delta]1,\[Delta]2}][num /. ptemp->mom, k, pp /. ptemp->mom]
			}, {}]
		/. { LoopIntegral[x__][0, y__] :> 0 }
];


Diff[exp_, mom_, idx_] := Module[{pos},
	If[FreeQ[exp, LoopIntegral],
		OldDiff[exp, mom /. suMomentumUnformat, idx],
		pos = FirstPosition[{exp}, LoopIntegral[__][_,mom,___]];
		If[Head[pos]===Missing,
			DiffOnce[Unformat[exp], mom /. suMomentumUnformat, idx, {}],
			Message[Diff::darg, mom, Extract[{exp}, pos]]; Null
		]
	] 
	/. suProductsToMagnitudes
];


Diff[exp_List, mom_, idx_] := Diff[#, mom, idx]& /@ exp;
Diff[exp_SeriesData, mom_, idx_] := 
	SeriesData[exp[[1]],exp[[2]], Diff[#, mom, idx]& /@ (exp[[3]]), exp[[4]],exp[[5]],exp[[6]]];


Diff[x___] := Null /; Message[Diff::argrx, "Diff", Length[{x}], 3];


(* ::Section::Closed:: *)
(*Conformal operators*)


ScalarKOp[F_, p_, \[Beta]_] := D[F,p,p] - (2\[Beta]-1)/p * D[F,p];
ScalarKKOp[F_,p1_,p2_,\[Beta]1_,\[Beta]2_] := ScalarKOp[F, p1, \[Beta]1] - ScalarKOp[F, p2, \[Beta]2];


ScalarKOp[x___] := Null /; Message[ScalarKOp::argrx, "KOp", Length[{x}], 3];
ScalarKKOp[x___] := Null /; Message[ScalarKOp::argrx, "KOp", Length[{x}], 5];


(* ::Section:: *)
(*End*)


End[];
Protect @@ Names["Minimal`*"];
EndPackage[];
