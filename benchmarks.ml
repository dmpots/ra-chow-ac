type benchmark = 
  | Fmin
  | Seval
  | Rkf45
  | Solve
  | Svd
  | Urand
  | Zeroin
  | Doduc
  | Fpppp
  | Matrix300
  | Tomcatv
  | Applu
  | Wave5X

let valid_names = [
  "fmin"; "seval"; "rkf45"; "solve"; "svd"; "urand"; "zeroin";
  "doduc"; "matrix300"; "tomcatv"; "fpppp";
  "applu"; "wave5X"
]
let from_name = function
  | "fmin" -> Fmin
  | "seval" -> Seval
  | "rkf45" -> Rkf45
  | "solve" -> Solve
  | "svd" -> Svd
  | "urand" -> Urand
  | "zeroin" -> Zeroin
  | "doduc" -> Doduc
  | "fpppp" -> Fpppp
  | "matrix300" -> Matrix300
  | "tomcatv" -> Tomcatv
  | "applu" -> Applu
  | "wave5X" -> Wave5X
  | _ as s -> failwith ("unknown benchmark "^s)

(* fmm *)
let svd = ["svd.i"]
let zeroin = ["zeroin.i"]
let solve = ["decomp.i"; "solve.i"]
let seval = ["seval.i"; "spline.i"]
let fmin = ["fmin.i"]
let rkf45 = ["fehl.i"; "rkfs.i"; "rkf45.i"]
let urand = ["urand.i"]

(* spec *)
let doduc = ["sortie.i"; "vgjyeh.i"; "colbur.i"; "dyeh.i";
  "ihbtr.i"; "inisla.i"; "coeray.i"; "drigl.i"; "orgpar.i";
  "pastem.i"; "supp.i"; "hmoy.i"; "inithx.i"; "saturr.i"; "prophy.i";
  "x21y21.i"; "cardeb.i"; "bilsla.i"; "inter.i"; "ddeflu.i";
  "paroi.i"; "iniset.i"; "integr.i"; "debico.i"; "drepvi.i"; "si.i";
  "lissag.i"; "arret.i"; "subb.i"; "sigma.i"; "repvid.i"; "dcoera.i";
  "bilan.i"; "yeh.i"; "debflu.i"; "deseco.i"; "inideb.i"; "heat.i";
]
let fpppp = [
  "intowp.i"; "fpppp.i"; "gamgen.i"; "ilsw.i";
  "twldrv.i"; "fmtgen.i"; "efill.i"; "aclear.i";
  "lclear.i"; "fmtset.i"
] (* excluded: "nprio.i"; *)
let matrix300 = ["saxpy.i"; "sgemv.i"; "sgemm.i"]
let tomcatv = ["tomcatv.i"]

(* spec95X *)
let applu = [
  "exact.i"; "buts.i"; "error.i"; "ssor.i";
  "verify.i"; "l2norm.i"; "jacu.i"; "erhs.i"; "jacld.i";
  "setiv.i"; "rhs.i"; "pintgr.i"; "setbv.i"; "blts.i"
] (* excluded: applu.i, maxnorm.i *)


let wave5X = [
  "bcndX.i";"celbndX.i";"denptX.i";"densX.i";"densxX.i";"densyX.i"; 
  "diagnsX.i";"ecrdX.i";"ecwrX.i";"energyX.i";"fftbX.i";"fftfX.i";
  "fieldX.i";"genbX.i";"genprbX.i";"getbX.i";"initX.i";"injchkX.i";
  "jobtimX.i";"lasdenX.i";"laspowX.i";"numbX.i";"parmvrX.i";
  "pdiagX.i"; "putbX.i";"radb2X.i";"radb4X.i";"radb5X.i";
  "radf2X.i";"radf4X.i";"radf5X.i";"ranfX.i";"rfftbX.i";
  "rfftb1X.i";"rfftfX.i";"rfftf1X.i";"rfftiX.i";"rffti1X.i";
  "setbX.i";"setinjX.i";"slv2xyX.i";"smoothX.i";"solv2yX.i";
  "transX.i";"vslv1pX.i"
] (* excluded: tons *)

let get_files_from_name s =
  match (from_name s) with
    | Fmin -> fmin
    | Seval -> seval
    | Rkf45 -> rkf45
    | Solve -> solve
    | Svd   -> svd
    | Urand  -> urand
    | Zeroin -> zeroin
    | Doduc -> doduc
    | Fpppp -> fpppp
    | Matrix300 -> matrix300
    | Tomcatv -> tomcatv
    | Applu -> applu
    | Wave5X -> wave5X

