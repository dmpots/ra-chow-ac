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



(* we can exclude searches on files that need more registers than we
 * are using for allocation to speed up the search, since those results
 * should not be used in the results comparison anyway 
 *)

let excludes_for_r = function
  | 32 -> []
  | 24 -> ["efill.i"; "twldrv.i"]
  | 16 -> [
            "twldrv.i";
            "vslv1pX.i";
            "drigl.i";
            "rfftb1X.i";
            "rkf45.i";
            "rfftf1X.i";
            "solv2yX.i";
            "efill.i";
            "debflu.i";
            "slv2xyX.i";
            "rkfs.i";
            "yeh.i";
           ]
  | 8 ->  [
            "twldrv.i";
            "celbndX.i";
            "vslv1pX.i";
            "injchkX.i";
            "transX.i";
            "prophy.i";
            "fehl.i";
            "ssor.i";
            "buts.i";
            "densyX.i";
            "radb4X.i";
            "drigl.i";
            "subb.i";
            "dcoera.i";
            "rfftfX.i";
            "blts.i";
            "sgemv.i";
            "rfftb1X.i";
            "saturr.i";
            "radf4X.i";
            "rkf45.i";
            "deseco.i";
            "colbur.i";
            "inter.i";
            "svd.i";
            "rfftf1X.i";
            "l2norm.i";
            "radb2X.i";
            "supp.i";
            "solv2yX.i";
            "efill.i";
            "radb5X.i";
            "debflu.i";
            "radf2X.i";
            "radf5X.i";
            "slv2xyX.i";
            "smoothX.i";
            "denptX.i";
            "rkfs.i";
            "yeh.i";
            "dyeh.i";
            "rfftbX.i";
            "seval.i";
            "ddeflu.i";
            "fieldX.i";
            "sgemm.i";
          ]
    | _ -> []

let filter_files_for_r r bench_files =
  let excluded_files = excludes_for_r r in
  let ok_files = 
    List.map (fun files ->
      List.filter (fun file -> 
        not (List.exists (fun exclude -> file = exclude) excluded_files)
      ) files
    ) bench_files
  in
  (* get rid of any benchmarks that have been completly filtered *)
  List.filter (fun files -> files <> []) ok_files

