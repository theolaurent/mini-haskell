module type VERTEX =
  sig
    include Set.OrderedType
  end

    
module Make =
  functor(V: VERTEX) ->
	 struct
	   module VertexSet = Set.Make(V)
	   module VertexMap = Map.Make(V)		       
				      
	   type t =
	       {
		 vertices: VertexSet.t ;
		 children: VertexSet.t VertexMap.t ;
		 parents: VertexSet.t VertexMap.t
	       }

	   let empty =
	     { vertices = VertexSet.empty ;
	       children = VertexMap.empty ;
	       parents = VertexMap.empty }
	       
	   let add_vertex x g =
	     {g with vertices = VertexSet.add x g.vertices }

	   let children x g =
	     try
	       VertexMap.find x g.children
	     with Not_found ->
	       VertexSet.empty

	   let parents x g =
	     try
	       VertexMap.find x g.parents
	     with Not_found ->
	       VertexSet.empty
		 
		 
	   let add_edge x y g =
	     let g = add_vertex x g in
	     let g = add_vertex y g in
	     
	     let children = VertexMap.add x (VertexSet.add y (children x g)) g.children in
	     let parents = VertexMap.add y (VertexSet.add x (parents y g)) g.parents in 
	     { g with parents ; children }

	   let find_roots g =
	     VertexSet.filter (fun v -> not (VertexMap.mem v g.parents)) g.vertices

	   let topological_sort g =
	     let rec impl v (l, visited) =
	       let ch = children v g in
	       let (l,visited) =
		 VertexSet.fold (fun w (l, visited) ->
				 if not (VertexSet.mem w visited)
				 then begin
				     let visited = VertexSet.add w visited in
				     impl w (l, visited)
				   end
				 else (l, visited)
				) ch (l, visited)
	       in
	       (v :: l, visited)
	     in
	     let roots = find_roots g in
	     fst (VertexSet.fold impl roots ([], VertexSet.empty))

		 
	   type ornment =
	       {
		 mutable num: int ;
		 mutable numAcc: int ;
		 mutable inStack: bool ;	  
	       }
		 
	   module VertexSetMap = Map.Make(VertexSet)

	   let topologically_sorted_components g =
	     let num = ref 0 in
	     let ornments = Hashtbl.create (VertexSet.cardinal g.vertices) in
	     let stack = Stack.create () in
	     
	     let rec impl comps vertex =
	       let ornment = { num = !num ; numAcc = !num ; inStack = true } in
	       Hashtbl.add ornments vertex ornment ;
	       incr num ;
	       
	       Stack.push vertex stack ;

	       
	       let succs = children vertex g in
	       let comps =
		 VertexSet.fold
		   (fun w comps ->
		    begin
		      try
			let orn = Hashtbl.find ornments w in
			if orn.inStack
			then ornment.numAcc <- min ornment.numAcc orn.numAcc ;
			comps
		      with Not_found ->
			let (comps, orn) = impl comps w in
			
			ornment.numAcc <- min ornment.numAcc orn.numAcc ;
			comps
		    end
		   ) succs comps
	       in

	       if ornment.num = ornment.numAcc
	       then
		 let rec create_scc comps =
		   let w = Stack.pop stack in
		   let orn = Hashtbl.find ornments w in
		   orn.inStack <- false ;
		   let comps = VertexMap.add w vertex comps in
		   if w = vertex
		   then comps
		   else create_scc comps
		 in
		 let comps = create_scc comps in
		 (comps, ornment)
	       else (comps, ornment)
	     in
	     
	     let comps =
	       VertexSet.fold
		 (fun v comps ->
		  if not (Hashtbl.mem ornments v)
		  then fst (impl comps v)
		  else comps) g.vertices VertexMap.empty
	     in

	     let comp_graph =
	       VertexMap.fold
		 (fun x rx cg ->
		  let cg = add_vertex rx cg in
		  let ch = children x g in
		  VertexSet.fold
		    (fun y cg ->
		     let ry = VertexMap.find y comps in
		     if rx <> ry
		     then add_edge rx ry cg
		     else cg
		    ) ch cg
		 ) comps empty
	     in

	     let sorted = topological_sort comp_graph in

	     let bindings = VertexMap.bindings comps in
	     
	     List.map (fun x -> 
		       List.filter (fun (_, y) -> x = y) bindings
		       |> List.map fst) sorted
	 end
