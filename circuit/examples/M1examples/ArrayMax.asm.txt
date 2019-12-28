; ArrayMax: find the maximum element of an array
;
; The program is given
;   *  a natural number n, assume n>0
;   *  an n-element array x[0], x[1], ..., x[n-1]
;  It calculates
;   * max = the maximum element of x

; Since n>0, the array x contains at least one element,
; and a maximum element is guaranteed to exist.
;
; program ArrayMax
;   max := x[0]
;   for i := 1 to n-1 step 1
;       if x[i] > max
;         then max := x[i]

; Register usage
;   R1 = constant 1
;   R2 = n
;   R3 = i
;   R4 = max

; Initialise

       lea   R1,1[R0]          ; R1 = constant 1
       load  R2,n[R0]          ; R2 = n
       lea   R3,1[R0]          ; R3 = i = 1
       load  R4,x[R0]          ; R4 = max = x[0]

; Top of loop, determine whether to remain in loop

loop
       cmplt R5,R3,R2          ; R5 = (i<n)
       jumpf R5,done[R0]       ; if i>=n then goto done

; if x[i] > max

       load  R5,x[R3]          ; R5 = x[i]
       cmpgt R6,R5,R4          ; R6 = (x[i]>max)
       jumpf R6,next[R0]       ; if x[i] <= max then goto neg

; then max := x[i]

       add   R4,R5,R0          ; max := x[i]

; Bottom of loop, increment loop index

next   add   R3,R3,R1          ; i = i + 1
       jump  loop[R0]          ; go to top of loop

; Exit from loop

done   store R4,max[R0]        ; max = R4
       trap  R0,R0,R0          ; terminate

; Data area
n        data   6
max      data   0
x        data  18
         data   3
         data  21
         data  -2
         data  40
         data  25
