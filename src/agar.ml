(*********)
(* types *)
(*********)

type coord = int * int

type cible = coord

type rayon = int

type cellule = coord * rayon

type zombie  = cellule * cible

type monde = zombie list * cellule

(***************)
(* utilitaires *)
(***************)

(* renvoie un point aleatoire de la fenetre *)
let rand_point () = 50+(Random.int 800),50+(Random.int 800)

(* distance euclidienne entre deux points *)
let dist a b =
  let xa,ya = a
  and xb,yb = b in
  let x = xa-xb in
  let y = ya-yb in
  int_of_float (sqrt (float_of_int (x*x + y*y)))

(* prend un vecteur de deplacement et le ramene à une norme constante *)
(* cela permettra d'avoir une vitesse homogene *)
let normalize (vect: coord) : coord =
  let dx,dy = vect in
  let dxf = float dx and dyf = float dy in
  let norme = sqrt (dxf*.dxf +. (dyf*.dyf)) /. 4. in
  int_of_float (dxf/.norme), int_of_float (dyf/.norme)

(********************)
(* code à completer *)
(********************)

(* deplacement d'un point A vers un point B  *)
(* on effectue à partir de A une translation de vecteur AB.
   on n'oubliera pas de normaliser le vecteur avant la translation *)
let deplace_point (a:coord) (b:coord) =
  (* REMPLACER LE 'assert false' PAR VOTRE CODE *)
  let xa, ya = a
  and xb, yb = b in
  let x = xb - xa in
  let y = yb - ya in
  let nx, ny = normalize (x,y) in
  xa + nx, ya + ny

(* deplacement d'un  zombie vers son point cible *)
(* Si il atteint son point_cible on en tire un nouveau *)
let deplace_zombie (z: zombie) : zombie =
  let (coord, r), target = z in
  let newpos = deplace_point coord target in
  if (dist newpos target) < r then (newpos, r), rand_point()
  else (newpos, r), target

(* prend un monde et retourne un nouveau monde ou les zombies et le heros
   se sont déplacés *)
let deplace_tous (m:monde) (souris:coord) : monde =
  let zl, (coord, r) = m in
  List.map deplace_zombie zl, (deplace_point coord souris, r)

(* prend deux cellules et renvoie vrai si la premiere peut manger la seconde *)
let peut_manger (cell1:cellule) (cell2:cellule) =
  let (coord1, r1) = cell1
  and (coord2, r2) = cell2 in
  (r1 > r2) && (dist coord1 coord2 < r1 + r2)

(* prend une cellule 'c' et une liste de zombies
   et retourne 'c' apres qu'elle ait absorbé les zombies plus petits qu'elle
   qui sont à portée *)
let grossir (cell:cellule) (zombies: zombie list) : cellule =
  let (coord, r) = cell in
  let lm = List.filter (fun (c,_) -> peut_manger cell c) zombies in
  coord, List.fold_left (fun r ((_,r2),_) -> r + int_of_float (sqrt(float_of_int r2))) r lm

(* fait manger le heros et tous les zombies *)
let grossir_tous (m:monde) : monde =
  let zl, cell = m in
  List.map (fun (c,t) -> (grossir c zl,t)) zl, grossir cell zl

(* retourne true si la cellule c se fait manger par au moins
   un des zombies de la liste zombies *)
let se_fait_manger (c:cellule) (zombies:zombie list) =
  List.exists (fun (cell, t) -> peut_manger cell c) zombies

(* prend un monde et retourne un monde ou tous les zombies mangés ce tour-ci
   ont été retirés *)
let retirer_mangees (m:monde) : monde =
  let (zl, c) = m in
  List.filter (fun (cell, t) -> not (se_fait_manger cell zl) && not (peut_manger c cell)) zl, c

(******************)
(* gestion du jeu *)
(******************)

let fini (m:monde) : bool =
  let (zl, cell) = m in
  List.exists (fun (c, _) -> peut_manger c cell) zl || zl = []

(* applique un instant de déplacement à tous les participants *)
let un_temps (m:monde) (point:coord) : monde =
  if (fini m) then raise Exit
  else retirer_mangees (grossir_tous (deplace_tous m point))

(**********)
(* dessin *)
(**********)

let draw_cell ((x,y),rad) =
  Graphics.set_color Graphics.blue;
  Graphics.fill_circle x y rad

let draw_heros ((x,y),rad) =
  Graphics.set_color (Graphics.red);
  Graphics.fill_circle x y rad

let draw_monde ((zombies,heros):monde) : unit =
  List.iter (fun (c,_) -> draw_cell c) zombies;
  draw_heros heros

(*****************)
(* initialisation *)
(*****************)

let init_cell rad : cellule = ((rand_point ()),rad)

let init_zombies nb : zombie list =
  let rec aux acc nb =
    if nb = 0 then acc
    else
      let zombie = (init_cell (5 + Random.int 20)),(rand_point ()) in
      aux (zombie::acc) (nb-1)
  in aux [] nb

(********)
(* main *)
(********)

let _ =
  (* on fabrique une fonction qui appelle la commande sleep du system *)
  let sleep millis = ignore(Sys.command ("sleep "^(string_of_float millis))) in

  (* initialisation du générateur aléatoire *)
  Random.self_init ();

  (* initialisation de la fenetre graphique *)
  Graphics.open_graph " 900x900";
  Graphics.set_window_title "agar.ml";
  Graphics.auto_synchronize false;

  (* on crée un héros et des zombies *)
  let heros   : cellule = init_cell 20 in
  let zombies : zombie list = init_zombies 30 in
  let world   : monde = zombies,heros in

  (* boucle d'interaction *)
  let rec loop (w:monde) =
    (* on attend un peu entre chaque tour pour réduire la vitesse du jeu *)
    sleep 0.03;

    (* on efface le contenu de la fenetre *)
    Graphics.synchronize();
    Graphics.clear_graph();

    (* on dessine le monde *)
    draw_monde w;

    (* on recupere les coordonnées de la souris *)
    let mx,my = Graphics.mouse_pos() in

    (* on fait avancer le monde d'un instant *)
    let temps_suivant = un_temps w (mx,my) in

    loop temps_suivant
  in
  loop world
