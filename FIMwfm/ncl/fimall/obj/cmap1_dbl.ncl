function gen_colormap( )
begin
        ;This procedure explicitly sets the red, green, and blue
        ;intensities for each element in a colormap array.
        ;
        return( (/ (/.000,.000,.000/), \   ; 0 (background)
                (/1.000,1.000,1.000/), \   ; 1 (foreground)
                (/.000,.184,1.000/), \     ; 2
                (/.041,.345,1.000/), \     ; 3
                (/.082,.506,1.000/), \     ; 4
                (/.112,.591,.963/), \      ; 5
                (/.141,.675,.925/), \      ; 6 
                (/.071,.783,.914/), \      ; 7 
                (/.000,.890,.902/), \      ; 8
                (/.167,.890,.451/), \      ; 9
                (/.333,.890,.000/), \      ; 10
                (/.500,.945,.000/), \      ; 11
                (/.667,1.000,.000/), \     ; 12
                (/.834,1.000,.000/), \     ; 13
                (/1.000,1.000,.000/), \    ; 14
                (/1.000,.900,.000/), \     ; 15
                (/1.000,.800,.000/), \     ; 16
                (/1.000,.700,.000/), \     ; 17
                (/1.000,.600,.000/), \     ; 18
                (/1.000,.432,.000/), \     ; 19
                (/1.000,.263,.000/), \     ; 20
                (/1.000,.328,.500/), \     ; 21
                (/1.000,.392,1.000/), \    ; 22
                (/1.000,.506,.955/), \     ; 23
                (/1.000,.620,.910/), \     ; 24
                (/1.000,.810,.955/), \     ; 25
                (/1.000,1.000,1.000/), \   ; 26
                (/.500,.500,.500/), \      ; 27
                (/.000,.000,.000/), \      ; 28
                (/.375,.500,.500/), \      ; 29
                (/.749,1.000,1.000/), \    ; 30
                (/.375,.500,.794/), \      ; 31
                (/.000,.000,.588/), \      ; 32
                (/.000,.259,.294/), \      ; 33
                (/.000,.518,.000/), \      ; 34
                (/.490,.338,.079/), \      ; 35
                (/.980,.157,.157/), \      ; 36
                (/.686,.275,.275/), \      ; 37
                (/.392,.392,.392/) /) )    ; 38
end