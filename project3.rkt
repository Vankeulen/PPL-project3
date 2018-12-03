#lang racket
; 
;(rest L) ~= L.next
; rest is like chasing the 'next' pointer 
;	on a typical linked list node
;(first L) ~= L.data
;'first' is like grabbing the data in that node.
;	All 'lists' in lisp like languages are linked lists.

; Insert item at index pos into list L
(define (insert L item pos)
	; (printf "\tinsert ~s ~s ~s\n" L item pos)
	; like switch case statement
	(cond 
		; case pos < 0, return new Node(item, L)
		((<= pos 0) (cons item L))
		; else return new Node(L.data, insert(L.next, item, pos - 1));
		(else (cons (first L) (insert (rest L) item (- pos 1))) )
	)
)

; Entry point to permutation logic
(define (perm L item)
	(perm_helper L item (length L) )
)

; Actual permutation logic
(define (perm_helper L item pos)
	(cond
		; Base case, return empty list (basically just a null pointer)
		((< pos 1) empty)
		; else, cons a new node with the permutation for the current position
		; onto  all of the other returned permutations
		(else (cons (insert L item pos) (perm_helper L item (- pos 1)) ) )
	)
)
; Logic to permute all lists within a list with the same item,
; and create one massive list of all permutations.
(define (perm_all L item)
	(cond
		; Base case, return empty list.
		((empty? L) empty)
		; else, append current permutations to the rest of the permutations
		(else (append 
			(perm (first L) item) 
			(perm_all (rest L) item) )
		)
	)
)

; Gen all permutations of trips up to n in length
(define (genTours n)
	(cond
		; base case, just ((1))
		((<= n 1) '((1)))
		; Otherwise, generate all new permutations for the current length
		(else (perm_all (genTours (- n 1)) n) )
	)
)

; Calculates distance between points a and b
; a and b are both lists like (3 2) or (16 12)
(define (dist a b)
	;(printf "\t  dist ~s ~s\n" a b)
	(sqrt
		(+ 
			(expt (- (first a) (first b)) 2)
			(expt (- (second a) (second b)) 2)
		)
	)
)

; Returns 'index' element of L, or empty value if impossible
(define (nth index L) 
	;(printf "\t  nth ~s ~s\n" index L)
	(cond
		((empty? L) empty)
		((= index 0) (first L))
		(else (nth (- index 1) (rest L)) )
	)
)

(define (score verts tour) 
	;(printf "\tscore ~s ~s\n" verts tour)
	(score_ (cons "bump" verts) tour)
)
; Score a tour with given vert data
(define (score_ verts tour) 
	;(printf "\t  score_ ~s ~s\n" verts tour)
	(cond 
		; base case, last element in tour, goes back to point 1
		( (empty? (rest tour)) (dist (nth 1 verts) (nth(first tour) verts) ) )
		; Sum 
		;		dist between the next 2 verts 
		;		and the total distance of the rest of the tour
		(else (+
				(dist 
					(nth (first tour) verts) 
					(nth (second tour) verts))
				(score_ verts (rest tour) )
			)
		)
	
	)
)

; Takes a list of verts, and finds the best tour
(define (etsp verts) 
	(etsp_ verts (genTours (length verts)) )
)
; Takes a list of verts and list of tours, and finds the best tour
(define (etsp_ verts tours)
	; If we have just one tour, that tour is the best tour
	(cond 
		((empty? (rest tours)) (first tours) )
		(else 
			; Reduce amount of computation that it needs to do
			; by calculating stuff once and referencing the results
			(define bestTour (etsp_ verts (rest tours)))
			
			(cond 
				; Compare the two scores and return the better tour
				; Return this tour if it has a lower score
				((< (score verts (first tours)) (score verts bestTour)) (first tours))
				; otherwise returns bestTour
				(else bestTour)
			)
		)
	)
)

(printf "Testing append\n" )
(printf "append (1 2 3) empty: ~s\n" (append (list 1 2 3) empty))
(printf "append (1 2 3) (4 5 6): ~s\n" (append (list 1 2 3) (list 4 5 6)) )
(printf "append ((1 2 3)) ((4 5 6)): ~s\n" (append (list (list 1 2 3)) (list (list 4 5 6)) ))

(printf "Testing insert\n" )
(printf "insert (1 2 3) 4 3: ~s\n" (insert (list 1 2 3) 4 3) )
(printf "insert (1 2 3) 4 2: ~s\n" (insert (list 1 2 3) 4 2) )
(printf "insert (1 2 3) 4 1: ~s\n" (insert (list 1 2 3) 4 1) )
(printf "\n" )
(printf "Testing perm\n" )
(printf "perm (1) 2: ~s\n" (perm (list 1) 2) )
(printf "perm (1 2) 3: ~s\n" (perm (list 1 2) 3) )
(printf "perm (1 2 3) 4: ~s\n" (perm (list 1 2 3) 4) )
(printf "\n" )
(printf "Testing perm_all\n" )
(printf "perm_all (1) 2: ~s\n" (perm_all (list (list 1)) 2) )
(printf "perm_all (1 2) 3: ~s\n" (perm_all (list (list 1 2)) 3) )
(printf "perm_all (1 2 3) 4: ~s\n" (perm_all (list (list 1 2 3) (list 1 3 2)) 4) )
(printf "\n" )
(printf "\n" )
(printf "Testing genTours\n" )
(printf "genTours 1: ~s\n" (genTours 1) )
(printf "genTours 2: ~s\n" (genTours 2) )
(printf "genTours 3: ~s\n" (genTours 3) )
(printf "genTours 4: ~s\n" (genTours 4) )
(printf "\n" )
(printf "Testing dist\n" )
(printf "dist (8 2) (3 1): ~s\n" (dist '(8 2) '(3 1) ))
(printf "\n" )
(printf "Testing nth\n" )
(printf "nth 0 (0 1 2 3 4 5 6 7 8 9): ~s\n" (nth 0 '(0 1 2 3 4 5 6 7 8 9) ))
(printf "nth 3 (0 1 2 3 4 5 6 7 8 9): ~s\n" (nth 3 '(0 1 2 3 4 5 6 7 8 9) ))
(printf "nth 5 (0 1 2 3 4 5 6 7 8 9): ~s\n" (nth 5 '(0 1 2 3 4 5 6 7 8 9) ))
(printf "nth 11 (0 1 2 3 4 5 6 7 8 9): ~s\n" (nth 11 '(0 1 2 3 4 5 6 7 8 9) ))
(printf "nth 0 (): ~s\n" (nth 0 empty ))

(printf "\n" )
(printf "\n" )
(define EXAMPLE_VERTS '((3 1) (8 2) (5 4) (2 6) (6 6) (4 8)))
(printf "Testing score\n" )
(printf "score ~s (1 2 3 4 5 6): ~s\n" EXAMPLE_VERTS (score EXAMPLE_VERTS '(1 2 3 4 5 6)) )

(printf "\n" )
(printf "\n" )
(printf "Testing etsp\n" )
(printf "etsp ~s: ~s\n" EXAMPLE_VERTS (etsp EXAMPLE_VERTS) )
(printf "best score for ~s: ~s" EXAMPLE_VERTS (score EXAMPLE_VERTS (etsp EXAMPLE_VERTS) ) )