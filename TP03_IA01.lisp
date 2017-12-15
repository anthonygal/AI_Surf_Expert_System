;Base de Faits/Connaissances
(setq *spots* '(Landes_FR CapeTown_RSA GoldCoast_AUS Ohahu_HWI Bali_IDN Reykjanes_ISL))
(setq *EB* (1 2 3 4 5 6 7 8 9 10 11 12))
(setq *PS* 
	'(
		(faible  (Landes_FR Reykjanes_ISL))
		(moyen (CapeTown_RSA GoldCoast_AUS))
		(eleve (Ohahu_HWI Bali_IDN))
	 )
)


;Initialisations des variables
(setq FV 21)
(setq Text 26)
(setq S 'Landes_FR)
(setq EB NIL)
(setq PS NIL)





;Base de Règles

;Regles de premier niveau

;Echelle de Beaufort EB en fonction de la force du vent FV (ici x)
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

;Tests
(getEB FV)
(print EB)

(getPS (car (cadr (cadr *PS*))))
(print PS)
