;Connaissances
(setq *spots* '(Landes_FR CapeTown_RSA GoldCoast_AUS Ohahu_HWI Bali_IDN Reykjanes_ISL))
(setq *desc*
	'(
		"La mer est comme un miroir, lisse et sans vague"
		"Quelques rides ressemblant à des écailles de poisson, mais sans aucune écume"
		"Vaguelettes ne déferlant pas"
		"Très petites vagues. Les crêtes commencent à déferler. Écume d'aspect vitreux. Parfois quelques moutons épars"
		"Petites vagues, de nombreux moutons"
		"Vagues modérées, moutons et embruns"
		"Crêtes d'écume blanches, lames, embruns"
		"Trainées d'écume, lames déferlantes"
		"Tourbillons d'écumes à la crête des lames, trainées d'écume"
		"Lames déferlantes grosses à énormes, visibilité réduite par les embruns"
		"Conditions exceptionnelles : Très grosses lames à longue crête en panache. L'écume produite s'agglomère en larges bancs et est soufflée dans le lit du vent en épaisses trainées blanches. Dans son ensemble, la surface des eaux semble blanche. Le déferlement en rouleaux devient intense et brutal. Visibilité réduite"
		"Conditions exceptionnelles : Lames exceptionnellement hautes (les navires de petit et moyen tonnage peuvent, par instant, être perdus de vue). La mer est complètement recouverte de bancs d'écume blanche élongés dans la direction du vent. Partout, le bord de la crête des lames est soufflé et donne de la mousse. Visibilité réduite"
		"Conditions exceptionnelles : L'air est plein d'écume et d'embruns. La mer est entièrement blanche du fait des bancs d'écume dérivants. Visibilité fortement réduite"
	 )
)
(setq *tempSpot*
	'(
		(Landes_FR  14)
		(CapeTown_RSA 20)
		(GoldCoast_AUS 22)
		(Ohahu_HWI 26)
		(Bali_IDN  29)
		(Reykjanes_ISL 6)
	)
)

(setq *MP*
			'("Shortboard"
				"Big Wave Surfboard"
				"Funboard"
				"Fish Surfboard"
				"Longboard"
			)
)

(setq *TV*
	'(
		(0 0)
		(0 0.1)
		(0.2 0.5)
		(0.6 1.25)
		(1.3 2.5)
		(2.6 4)
		(4.1 6)
		(6.1 9)
	 )
)
(setq *PS*
	'(
		(faible  (Landes_FR Reykjanes_ISL))
		(moyen (CapeTown_RSA GoldCoast_AUS))
		(eleve (Ohahu_HWI Bali_IDN))
	 )
)
(setq *W*
      '(
        "4/3 mm Combinaison Intégrale + bottes "
		"3/2 mm Combinaison Intégrale + bottes "
        "3/2 mm Combinaison Intégrale"
        "Springsuit"
        "Rash Guard"
        "Short et Lycra"
         )
 )

;Base de Faits
(setq FV 21)
(setq Te 25)
(setq S 'Landes_FR)
(setq EB NIL)
(setq PS NIL)
(setq Desc NIL)
(setq TV NIL)
(setq W NIL)



;Base de Règles

;Regles de premier niveau

;Echelle de Beaufort EB en fonction de la force du vent FV (ici x)
;Ici les connaissances sur l'Echelle de Beaufort sont contenues dans la règle elle même (je ne sais pas si le meilleur bail)
(defun getEB(x)
	(cond
		((AND (>= x 0 ) (<= x 1 ))   		(setq EB 0))
		((AND (>= x 2 ) (<= x 5 ))    		(setq EB 1))
		((AND (>= x 6 ) (<= x 11 ))    		(setq EB 2))
		((AND (>= x 12 ) (<= x 19 ))    	(setq EB 3))
		((AND (>= x 20 ) (<= x 28 ))    	(setq EB 4))
		((AND (>= x 29 ) (<= x 38 ))    	(setq EB 5))
		((AND (>= x 39 ) (<= x 49 ))    	(setq EB 6))
		((AND (>= x 50 ) (<= x 61 ))    	(setq EB 7))
		((AND (>= x 62 ) (<= x 74 ))    	(setq EB 8))
		((AND (>= x 75 ) (<= x 88 ))    	(setq EB 9))
		((AND (>= x 89 ) (<= x 102 ))    	(setq EB 10))
		((AND (>= x 103 ) (<= x 118 ))    	(setq EB 11))
		((>= x 119) (setq EB 12))
   	)
)

;Popularité du Spot PS en fonction du spot S (ici x)
(defun getPS(x)
	(dolist (p *PS*)(
		if (member x (cadr p))
			(setq PS (car p))
		)
	)
)
;Calcul de la temperature finale de l'eau

(defun getT (f spot)
		(dolist (x *tempSpot*)
			( if (member spot x)
						(cond
								((AND (>= f 0) (<= f 10))		(setq Te (- car x 0.19) ))
								((AND (>= f 11) (<= f 20))		(setq Te (- car x 0.63) ))
								((AND (>= f 21) (<= f 30))		(setq Te (- car x 1.29) ))
								((AND (>= f 31) (<= f 40))		(setq Te (- car x 2.05) ))
								((AND (>= f 41) (<= f 50))		(setq Te (- car x 3.01) ))
								((AND (>= f 51) (<= f 60))		(setq Te (- car x 4.25) ))
								((> f 60)		(print "Erreur: Trop de vent pour surfer!")))
						)
			)
)

;Wetsuit en fonction de la temperature

(defun getWetsuit(x)
			(cond
				((AND (>= x 11) (<= x 13))     (setq W (nth 0 *W*)))
				((AND (>= x 14) (<= x 16))     (setq W (nth 1 *W*)))
				((AND (>= x 17) (<= x 19))     (setq W (nth 2 *W*)))
				((AND (>= x 20) (<= x 23))     (setq W (nth 3 *W*)))
				((AND (>= x 24) (<= x 28))     (setq W (nth 4 *W*)))
				((> x 28)   (setq W (nth 6 *W*)))
			)
)

;Modèle de planche en fonction de la popularité du spot et Taille de la vague

; (defun getMP(x y)
; 		(cond
; 			((AND
; 						(OR
; 								(AND
; 									(>= x 4.1) (<= x 6))
; 							  (AND
; 									(>= x 7)) (<= x 9)
; 						 )
; 						(OR
; 								(eq y "Faible") (eq y "Moyen")
; 						)
; 				)
; 			(setq MP (assoc (nth 0 *MP*) (nth 1 *MP*)))
; 			)

; 			((AND
; 						(OR
; 								(AND
; 									(>= x 4.1) (<= x 6))
; 								(AND
; 									(>= x 7)) (<= x 9))
; 								(eq y "Eleve")
; 			 )
; 			(setq MP  (nth 1 *MP*))
; 			)

; 			((AND
; 						(OR
; 							(eq x 0)
; 							(AND
; 								(>= x 0)) (<= x 0.1))
; 						(OR
; 							(eq y "Faible") (eq y "Moyen")  (eq y "Eleve"))
; 				)
; 			)
; 			(setq MP (nth 1 *MP*))

; 			((AND
; 						(OR
; 							(AND
; 								(>= x 0.2) (<= x 0.5))
; 							(AND
; 								(>= x 0.6)) (<= x 1.25)
; 						)
; 						 (OR
; 							(eq y "Faible") (eq y "Moyen")
; 						 )
; 				)
; 			(setq MP (list (nth 0 *MP*) (nth 2 *MP*)(nth 3 *MP*)(nth 4 *MP*))))

; 			((AND
; 						(OR
; 							(AND
; 								(>= x 0.2) (<= x 0.5)
; 							)
; 							(AND
; 								(>= x 0.6) (<= x 1.25)
; 							)
; 						(eq y "Eleve")
; 						)
; 				)
; 			(setq MP (assoc (nth 0 *MP*)(nth 3 *MP*)))
; 			)
; 			((AND
; 						(OR
; 							(AND
; 								(>= x 1.3) (<= x 2.5)
; 							)
; 							(AND
; 								(>= x 2.6)) (<= x 4)
; 							)
; 						(OR
; 							(eq y "Faible") (eq y "Moyen")
; 						)
; 				)
; 			(setq MP (list (nth 0 *MP*) (nth 3 *MP*)(nth 4 *MP*)))
; 			)
; 			((AND
; 						(OR
; 							(AND
; 								(>= x 1.3) (<= x 2.5)
; 							)
; 							(AND
; 								(>= x 2.6)) (<= x 4)
; 							)
; 						(eq y "Eleve")
; 				)
; 		 (setq MP (nth 1 *MP*))
; 	 		)
; 		)
; )

;Nombre de personnes en fonction de la taille de la vague et de la popularité du spot
(defun getNP (x y)(
	cond
		((AND
			(OR (<= (car x) 2.5)
				(>= (cadr x) 7)
			)
			(EQ y 'FAIBLE)
		) 
		(setq NP '(0 20))
		)
		((AND
			(AND (> (cadr x) 2.5)
				 (< (car x) 7)
			)
			(EQ y 'FAIBLE)
		) 
		(setq NP '(21 50))
		)
		((AND
			(OR (<= (car x) 0.5)
				(>= (cadr x) 7)
			)
			(EQ y 'MOYEN)
		) 
		(setq NP '(0 20))
		)
		((AND
			(AND (> (cadr x) 0.5)
				 (< (car x) 7)
			)
			(EQ y 'MOYEN)
		) 
		(setq NP '(21 50))
		)
		((AND
			(AND (> (cadr x) 0.2)
				 (< (car x) 7)
			)
			(EQ y 'ELEVE)
		) 
		(setq NP '(50 2000))
		)
		((AND
			(<= (cadr x) 0.2)
			(EQ y 'ELEVE)
		) 
		(setq NP '(0 20))
		)
		((AND
			(>= (car x) 7)
			(EQ y 'ELEVE)
		) 
		(setq NP '(21 50))
		)
	)
)

;Desc en fonction de l'échelle de Beaufort
(defun getDesc(x)
	(setq Desc (nth x *desc*))
)

;TV en fonction de l'échelle de Beaufort
(defun getTV(x)
	(if(< x 8)
		(setq TV (nth x *TV*))
		(print "Erreur: On parle de tsunami là! Fuis pauvre fou!")
     )
)

;Tests
(getEB FV)
(print EB)

(getPS (car (cadr (cadr *PS*))))
(print PS)

(getDesc 4)
(print Desc)

(getDesc 9)
(print Desc)

(getTV 4)
(print TV)

(getTV 9)
(print TV)

(getNP '(3 4) 'FAIBLE)
(print NP)

