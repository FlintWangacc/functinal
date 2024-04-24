(*type planar_point = {xcoord: float; ycoord:float; }

type circle = {center:planar_point; radius:float; }

type triangle = {ptA:planar_point;
                 ptB:planar_point;
                 ptC:planar_point; }

let rec translate dis = function
  | coord -> Some (coord +. dis)
  | {xcoord = x; ycoord = y;} -> Some { translate dis x; y }
  | {center = c; radius = r;} -> Some {translate dis c.center; r }
  | {ptA = a; ptb = b; ptc = c;} -> Some {translate dis a; translate dis b; translate dis c }*)


  type planar_point = { xcoord: float; ycoord: float }
  type circle = { center: planar_point; radius: float }
  type triangle = { ptA: planar_point; ptB: planar_point; ptC: planar_point }
  
  let translatePoint p dis = {xcoord = p.xcoord +. dis; ycoord = p.ycoord}

  let translate g dis = match g with
    | Some {xcoord = x; ycoord = y} as p -> translatePoint p
    | Some {center = c; radius = r} as c -> { center = translatePoint c; radius = r}
