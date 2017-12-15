;Base de Faits/Connaissances
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

;Desc en fonction de l'échelle de Beaufort
(defun getDesc(x)
	(nth (- x 1) *desc*)
)

;Tests
(getEB FV)
(print EB)

(getPS (car (cadr (cadr *PS*))))
(print PS)

(print (getDesc 4))
