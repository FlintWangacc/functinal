type ('a,'b) trie = TrieNode of 'a * 'b * ('a, 'b) trie list

type dictionary = (char, string) trie

exception Emptyword of string

exception NotFound of string

let rec retrieveDefinition trieComp keyList (TrieNode (word, def, trielist)) =
  match keyList with
  | [] -> def
  | h1::h2::t -> 
            let nextTrie = List.find (fun (TrieNode (w,d,t)) -> trieComp w h2) trielist
            in
            if h1 = word then
              retrieveDefinition trieComp t nextTrie
            else
              raise (NotFound "Not found")
  | h::t ->
    if h = word then
      def
    else
      raise (NotFound "Not found")


let initTrie = TrieNode ('_', "This is dummy head",[])
              
exception EmptyInsert of string

let rec insertTries order elem (TrieNode (word, def, trielist)) =
  match (fst elem) with
  | key::(_::_ as tl) ->
    let value = snd elem in
    if List.exists (fun (TrieNode (w, d, tl)) ->
                    order w key = 0
                    ) trielist then
      let it = List.find (fun (TrieNode(w, d, t)) -> order w key = 0) trielist in
      let insertTrieSub = insertTries order (tl, value) it in
      let newtrieList = List.map (fun (TrieNode (w, d, t) as tn) ->
                                    if order w key = 0 then
                                      insertTrieSub
                                    else
                                      tn) trielist in
      TrieNode (word, def, newtrieList)
    else
      let newtireNode = TrieNode (key, value, []) in
      let newtireNodeIns = insertTries order (tl, value) newtireNode in
      let newtrielist = List.sort (fun (TrieNode(w1, _, _)) (TrieNode(w2, _, _)) ->
                              order w1 w2) (newtireNodeIns::trielist) in
      TrieNode (word, def, newtrielist)
  | key::tl -> let value = snd elem in
                (assert (tl = []);
                if List.exists (fun (TrieNode (tkey, tval, tlst)) ->
                                order tkey key = 0
                               ) trielist then
                  TrieNode (word, def, List.map (fun (TrieNode (w1, _, tl1) as tmp) ->
                                                            if order w1 key = 0 then
                                                              TrieNode (w1, def, tl1)
                                                            else
                                                              tmp) trielist)
                else
                  let newtrieList = List.sort (fun (TrieNode(w1, _, _)) (TrieNode(w2, _, _))
                                                -> order w1 w2) (TrieNode(key, value, [])::trielist) in
                  TrieNode (word, def, newtrieList)
              )
  | [] -> raise (EmptyInsert "insert on empty word")

    

let buildTries order kvlst initTrie =
  List.fold_left (fun trie t ->
                    insertTries order t trie)
                initTrie
                kvlst 