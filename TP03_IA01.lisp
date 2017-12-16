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
	(setq *Te*
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
				'(
					"Shortboard"
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
			(2.6 3.9)
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
	(setq FV (read))
    (setq S (read))
    (setq EB NIL)
	(setq PS NIL)
    (setq Te NIL)
	(setq Desc NIL)
    (setq W NIL)
	(setq TV NIL)
	(setq MP NIL)
	(setq NP NIL)
	(setq NS NIL)

	(setq *BF* '(FV Te S EB PS Desc TV W MP NP NS))

;But
	(setq *But* '(Desc NS MP W))

;Base de Règles

	(setq *BR*
		'(
			(R1 (FV) EB getEB)
			(R2 (S) PS getPS)
			(R3 (FV S) Te getTe)
			(R4 (Te) W getW)
			(R5 (EB) Desc getDesc)
			(R6 (EB) TV getTV)
			(R7 (TV PS) MP getMP)
			(R8 (TV PS) NP getNP)
			(R9 (NP TV) NS getNS)
	      )
	)

;Regles de premier niveau

	;R1: Echelle de Beaufort EB en fonction de la force du vent FV (ici x)
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

	;R2: Popularité du Spot PS en fonction du spot S (ici x)
	(defun getPS(x)
		(dolist (p *PS*)(
			if (member x (cadr p))
				(setq PS (car p))
			)
		)
	)
	;R3: Calcul de la temperature finale de l'eau
	(defun getTe (f spot)
			(dolist (x *Te*)
				( if (member spot x)
							(cond
									((AND (>= f 0) (<= f 10))		(setq Te (- (cadr x) 0.19) ))
									((AND (>= f 11) (<= f 20))		(setq Te (- (cadr x) 0.63) ))
									((AND (>= f 21) (<= f 30))		(setq Te (- (cadr x) 1.29) ))
									((AND (>= f 31) (<= f 40))		(setq Te (- (cadr x) 2.05) ))
									((AND (>= f 41) (<= f 50))		(setq Te (- (cadr x) 3.01) ))
									((AND (>= f 51) (<= f 60))		(setq Te (- (cadr x) 4.25) ))
									((> f 60)		(print "Erreur: Trop de vent pour surfer!")))
							)
				)
	)

;Regles de deuxieme niveau

	;R4: Wetsuit en fonction de la temperature
	(defun getW(x)
				(cond
					((<= x 13)     (setq W (nth 0 *W*)))
					((AND (> x 13) (<= x 16))     (setq W (nth 1 *W*)))
					((AND (> x 16) (<= x 19))     (setq W (nth 2 *W*)))
					((AND (> x 19) (<= x 23))     (setq W (nth 3 *W*)))
					((AND (> x 23) (<= x 28))     (setq W (nth 4 *W*)))
					((> x 28)   (setq W (nth 5 *W*)))
				)
	)

	;R5: Desc en fonction de l'échelle de Beaufort
	(defun getDesc(x)
		(setq Desc (nth x *desc*))
	)

	;R6: TV en fonction de l'échelle de Beaufort
	(defun getTV(x)
		(if(< x 8)
			(setq TV (nth x *TV*))
			(print "Erreur: On parle de tsunami là! Fuis pauvre fou!")
	     )
	)

;Regles de troisieme niveau	

	;R7: Modèle de planche en fonction de la popularité du spot et Taille de la vague
	(defun getMP(x y)
	 		(cond
	 			(
				(>= (cadr x) 7)
				(setq MP '("Big Wave Surfboard"))
				)
				(
				(<= (car x) 0.1)
				(setq MP '("Funboard"))
				)
	 			((AND
					(AND (>= (car x) 4)
					 	 (< (cadr x) 7)
					)
				 (OR (EQ y 'FAIBLE) (EQ y 'MOYEN))
				)
				(setq MP '("Shortboard" "Big Wave Surfboard"))
				)
				((AND
					(AND (>= (car x) 4)
					 	 (< (cadr x) 7)
					)
				 (EQ y 'ELEVE)
				)
				(setq MP '("Big Wave Surfboard"))
				)
				((AND
					(AND (> (car x) 0.1)
					 	 (<= (cadr x) 1.25)
					)
				 (OR (EQ y 'FAIBLE) (EQ y 'MOYEN))
				)
				(setq MP '("Funboard" "Shortboard" "Fish Surfboard" "Longboard" ))
				)
				((AND
					(AND (> (car x) 0.1)
					 	 (<= (cadr x) 1.25)
					)
				 (EQ y 'ELEVE)
				)
				(setq MP '("Shortboard" "Fish Surfboard"))
				)
				((AND
					(AND (> (car x) 1.25)
					 	 (< (cadr x) 4)
					)
				 (OR (EQ y 'FAIBLE) (EQ y 'MOYEN))
				)
				(setq MP '("Shortboard" "Fish Surfboard"))
				)
				((AND
					(AND (> (car x) 1.25)
					 	 (< (cadr x) 4)
					)
				 (EQ y 'ELEVE)
				)
				(setq MP '("Shortboard"))
				)
			)
	 )

	;R8: Nombre de personnes en fonction de la taille de la vague (ici x) et de la popularité du spot (ici y)
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

;Regles de troisieme niveau

	;R9: NS en fonction de  NP (ici x) et TV (ici y)
	(defun getNS (x y)(
		cond
			((AND
				(<= (cadr x) 20)
				(<= (cadr y) 1.3)
			)
			(setq NS '(Debutant Intermediaire Expert))
			)
			((AND
				(<= (cadr x) 20)
				(> (cadr y) 1.3)
				(< (car y) 4)
			)
			(setq NS '(Intermediaire Expert))
			)
			((AND
				(> (cadr x) 20)
				(< (car x) 50)
				(< (car y) 4)
			)
			(setq NS '(Intermediaire Expert))
			)
			((AND
				(>= (car x) 50)
				(<= (car y) 1.3)
			)
			(setq NS '(Intermediaire Expert))
			)
			((AND
				(>= (car x) 50)
				(> (cadr y) 1.3)
				(< (car y) 4)
			)
			(setq NS '(Expert))
			)
			((>= (car y) 4)
			(setq NS '(Expert))
			)

		)
	)


;MOTEUR D'INFERENCE

;Algo de chainage avant en largeur d'abord
;	FAIRE{
;	FIN=0
;	Si but est dans BF
;		Afficher reslultat
;		FIN=1
;	Sinon;
;		Trouver regles candidates
;		Appliquer regles / ajout des resultats à la base de fait
;		Retirer les regles de la *BR*
;	}TANT QUE FIN=0


;Verifie si une sous liste de But est dans la BR
(defun ButDansBF?(B)(
  let (ok)(
           progn
               (setq ok T)
               (dolist (x B)
                   (if (EQ (symbol-value x) NIL)
                        (setq ok NIL)
                    )
               )
               ok
           )
)
)

;Renvoie la liste des regles applicables pour une base de fait donnée
(defun ReglesApplicables ()(
		let (res)(
			progn
				(dolist (r *BR*)(
					if (ButDansBF? (cadr r))
						(push r res)
					)
				)
				(reverse res)
		)
	)
)

;Fonction outi qui renvoie la liste des valeur associées aux symboles respectifs à partir de la liste des symboles
(defun symbol-values-from-list (L)
    (let (res)
         (progn
          (dolist (x L)
             (push (symbol-value x) res)
             )
          (reverse res)
         )
    )
)

;Applique la regle R, met a jour la base de fait et retire la regle de la base de regle
(defun AppliquerRegle (R)(
	progn
		(apply (cadddr R) (symbol-values-from-list (cadr R))) ;appel de la fonction get correspondante qui met à jour elle meme la BF par definition des fonctions get et de la BF
		(delete R *BR*)
	)
)

(defun resultat (B)(
	loop(
		if (ButDansBF? B)
			(progn
                (print "RESULTAT TROUVE:")
				(dolist (x B)
					(print (list x '= (symbol-value x)))
				)
				(return)
			)
			(if (NOT (EQ *BR* NIL))
				(dolist (r (ReglesApplicables))
					(progn
						(AppliquerRegle r)
                        (print "ETAT DE LA BASE DE FAITS:")
						(dolist (f *BF*) 
							(print (list f '= (symbol-value f)))	
						) 
						(print "--------------------")
					)
				)
				(progn
					(print "ECHEC, PLUS DE REGLES APPLICABLES")
					(return)
				)
			)
		)
	)
)

;Tests

; (print *BF*)
; (print (symbol-value (car *BF*)))
; (print *but*)
; (print *BR*)

; (getEB FV)
; (print EB)

; (getPS (car (cadr (cadr *PS*))))
; (print PS)

; (getDesc 4)
; (print Desc)

; (getDesc 9)
; (print Desc)

; (getTV 4)
; (print TV)

; (getTV 9)
; (print TV)

; (getNP '(3 4) 'FAIBLE)
; (print NP)

; (getNS '(53 57) '(2 3))
; (print NS)

; (dolist (tv *tv*) (print (getMP tv 'ELEVE)))

(resultat *but*)