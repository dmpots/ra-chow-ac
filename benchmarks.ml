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
  "injbatX.i"; "vslv1pX.i"; "ecrdX.i"; "bcndbX.i";
  "linjX.i"; "sinqbX.i"; "cosqiX.i"; "smoothX.i"; "erfX.i";
  "lasdenX.i"; "sinqfX.i"; "sinqiX.i"; "cosqfX.i"; "getbX.i";
  "fftfX.i"; "tpartX.i"; "radfgX.i"; "putdtX.i"; "rfftb1X.i";
  "rffti1X.i"; "transX.i"; "radb4X.i"; "abrtX.i"; "cosqbX.i";
  "denitrX.i"; "fftbX.i"; "recreX.i"; "cosqf1X.i"; "solv2yX.i";
  "energyX.i"; "rfftbX.i"; "radb5X.i"; "rinjX.i"; "genprbX.i";
  "pdiagX.i"; "densX.i"; "radb2X.i"; "rfftfX.i"; "rfftiX.i";
  "ranfX.i"; "advbndX.i"; "radb3X.i"; "vnewlX.i"; "laserX.i";
  "celbndX.i"; "parmovX.i"; "denitlX.i"; "clrdtX.i"; "bcndX.i";
  "putbX.i"; "tcompX.i"; "fieldX.i"; "rfftf1X.i"; "sudtblX.i";
  "genbX.i"; "radf4X.i"; "diagnsX.i"; "waveX.i"; "setbX.i";
  "parmvrX.i"; "radf5X.i"; "densxX.i"; "radf2X.i"; "getdtX.i";
  "injallX.i"; "densyX.i"; "setinjX.i"; "radf3X.i"; "ecwrX.i";
  "bcndtX.i"; "laspowX.i"; "radbgX.i"; "ibinX.i"; "initX.i";
  "bcndrX.i"; "parmveX.i"; "endrunX.i"; "bcndlX.i"; "injchkX.i";
  "injconX.i"; "vavgX.i"; "rewdtX.i"; "vslv1xX.i"; "inibndX.i";
  "cosqb1X.i"; "slv2xyX.i"; "jobtimX.i"; "numbX.i"; "denptX.i"
]

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

