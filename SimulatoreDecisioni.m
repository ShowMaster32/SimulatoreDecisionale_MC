(* ::Package:: *)

BeginPackage["SimulatoreDecisioni`"];

(* Funzione principale per avviare il gioco *)
AvviaGioco::usage = "AvviaGioco[] lancia la simulazione interattiva completa.";

Begin["`Private`"];

(* Variabile globale per raccogliere i messaggi di debug *)
debugLog = {};

(* Funzione per aggiungere messaggi al log di debug *)
AggiungiDebug[message_] := AppendTo[debugLog, message];

soluzioneOttimale = <|"BeneficioMassimo" -> 0, "Azioni" -> {}|>;

(* Funzione dedicata per configurare tutti i parametri dinamicamente in base alla difficolt\[AGrave] *)
ConfiguraParametri[] := Module[{},
  Switch[difficolta,
    "Facile", (
      costoRaccolta = 10; beneficioRaccolta = 30;
      costoCostruzione = 35; beneficioCostruzione = 8;
      costoEsplorazione = 15; beneficioEsplorazione = {20, 70};
      rendimentoCostruzione = 8;
    ),
    "Medio", (
      costoRaccolta = 15; beneficioRaccolta = 25;
      costoCostruzione = 40; beneficioCostruzione = 10;
      costoEsplorazione = 20; beneficioEsplorazione = {10, 50};
      rendimentoCostruzione = 10;
    ),
    "Difficile", (
      costoRaccolta = 15; beneficioRaccolta = RandomInteger[{15, 30}];
      costoCostruzione = 50; beneficioCostruzione = 12;
      costoEsplorazione = 25; beneficioEsplorazione = {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]};
      rendimentoCostruzione = 12;
    )
  ];
];

(* Funzione per verificare il risultato *)
If[risorseFinali === soluzioneOttimale[[1]],
  "Risultato Corretto! Hai ottenuto il massimo beneficio.",
  "Risultato Errato. Il massimo beneficio \[EGrave]: " <> 
   ToString[soluzioneOttimale[[1]]]]

(* Funzione per calcolare la strategia ottimale con programmazione dinamica *)
CalcolaStrategiaOttimale[turni_, risorse_, costoRacc_, benefRacc_, costoEspl_, benefEspl_] :=
 Module[{dp, beneficioEsplorazioneRange, t, r, maxBeneficio, azioniOttimali},
  (* Log delle variabili iniziali *)
  Print["Inizio Calcolo: turni = ", turni, ", risorse = ", risorse];

  (* Verifica di validit\[AGrave] dei parametri *)
  If[turni <= 0 || risorse <= 0 || costoRacc <= 0 || costoEspl <= 0,
   Print["Parametri non validi"];
   Return[<|"BeneficioMassimo" -> 0, "Azioni" -> {}|>]
  ];

  (* Inizializzazione della tabella DP *)
  dp = Table[{0, {}}, {turni + 1}, {risorse + 1}];

  (* Iterazione *)
  For[t = 0, t < turni, t++,
   For[r = 0, r <= risorse, r++,
    If[r >= costoRacc,
     maxBeneficio = dp[[t + 1, r + 1, 1]] + benefRacc;
     If[maxBeneficio > dp[[t + 2, r - costoRacc + 1, 1]],
      dp[[t + 2, r - costoRacc + 1]] = {maxBeneficio, 
        Append[dp[[t + 1, r + 1, 2]], "Raccogli"]}
     ];
    ];
    If[r >= costoEspl,
     maxBeneficio = dp[[t + 1, r + 1, 1]] + Max[benefEspl];
     If[maxBeneficio > dp[[t + 2, r - costoEspl + 1, 1]],
      dp[[t + 2, r - costoEspl + 1]] = {maxBeneficio, 
        Append[dp[[t + 1, r + 1, 2]], "Esplora"]}
     ];
    ];
    If[dp[[t + 1, r + 1, 1]] > dp[[t + 2, r + 1, 1]],
     dp[[t + 2, r + 1]] = dp[[t + 1, r + 1]];
    ];
   ]
  ];

  (* Log della matrice DP finale *)
  Print["Matrice DP finale: ", dp];

  (* Estrazione del risultato finale *)
  azioniOttimali = dp[[turni + 1, risorse + 1]];
  Print["Beneficio massimo trovato: ", azioniOttimali[[1]]];
  <|"BeneficioMassimo" -> azioniOttimali[[1]], "Azioni" -> azioniOttimali[[2]]|>
];

(* Funzione per mostrare la soluzione in una finestra pop-up *)
MostraSoluzione[soluzione_] :=
 Module[{},
  If[ListQ[Lookup[soluzione, "Azioni"]] && Lookup[soluzione, "BeneficioMassimo"] =!= 0,
   CreateDialog[{TextCell["Soluzione Ottimale:"], 
     TextCell[
      "Beneficio Totale: " <> ToString[Lookup[soluzione, "BeneficioMassimo"]] <>
       "\nAzioni Ottimali:\n" <> StringJoin[Riffle[Lookup[soluzione, "Azioni"], "\n"]],
      FontWeight -> Bold
     ]}, WindowTitle -> "Soluzione"],
   CreateDialog[{TextCell["Errore: La soluzione ottimale non \[EGrave] valida.", FontWeight -> Bold]}]
  ]
];

(* Funzione per avviare il simulatore *)
AvviaGioco[] := DynamicModule[
   {
    risorseCorrenti = 100, turniRestanti = 10, log = "", 
    azioniRaccolta = 0, azioniCostruzione = 0, azioniEsplorazione = 0, 
    simulazioneIniziata = False, risorseIniziali = 100, turniIniziali = 10, 
    difficolta = "Medio", costoRaccolta, beneficioRaccolta, 
    costoCostruzione, beneficioCostruzione, costoEsplorazione, 
    beneficioEsplorazione, rendimentoCostruzione = 0
    }, 
       
   (* Interfaccia del Simulatore *)
   Column[{
     
     (* Titolo del Simulatore *)
     Style["Simulatore di Ottimizzazione delle Scelte", Bold, 18],
     Style["Per iniziare, personalizzare i parametri e cliccare su 'Inizia Simulazione'", Italic, Darker@Gray],
     
     (* Personalizzazione dei parametri *)
     Row[{
   Tooltip[
    "Numero di Turni: ", 
    "Seleziona il numero totale di turni per la simulazione. Minimo: 5, Massimo: 20."
    ],
   Slider[Dynamic[turniIniziali, (turniIniziali = Round[#]) &], {5, 20}, 
    Enabled -> Dynamic[!simulazioneIniziata]],
   Dynamic[Style[turniIniziali, Bold]], Spacer[10],
   
   Tooltip[
    "Risorse Iniziali: ", 
    "Imposta le risorse iniziali disponibili per la simulazione. Minimo: 50, Massimo: 200."
    ],
   Slider[Dynamic[risorseIniziali, (risorseIniziali = Round[#]) &], {50, 200}, 
    Enabled -> Dynamic[!simulazioneIniziata]],
   Dynamic[Style[risorseIniziali, Bold]], Spacer[10],
   
   Tooltip[
    "Difficolt\[AGrave]: ", 
    "Imposta il livello di difficolt\[AGrave]:\n- Facile: Costi bassi, benefici elevati.\n- Medio: Bilanciato.\n- Difficile: Costi elevati, benefici bassi."
    ],
   PopupMenu[Dynamic[difficolta], {"Facile", "Medio", "Difficile"}, 
    Enabled -> Dynamic[!simulazioneIniziata]]
}]
     Spacer[10],
     
     Row[{
       Style["Parametri Attuali: ", Bold],
       Dynamic[
         Row[{
           "Turni: ", turniIniziali, "   ",
           "Risorse Iniziali: ", risorseIniziali, "   ",
           "Difficolt\[AGrave]: ", difficolta
           }]
         ]
       }],
     Spacer[10],
     
   (* Variabile per il seed numerico *)
	seedNumerico = 42;
	
	(* Interfaccia utente: aggiunta input per il seed *)
Row[{
   Style["Seed: ", Bold],
   InputField[
     Dynamic[
       seedNumerico, 
       (seedNumerico = IntegerPart[#]) &
     ], 
     FieldSize -> 6, 
     Enabled -> Dynamic[!simulazioneIniziata]
   ]
}],

Spacer[10],

(* Calcolo della strategia ottimale dopo aver impostato i parametri con seed*)
Tooltip[
 Button["Inizia Simulazione",
  (
   SeedRandom[seedNumerico]; (* Fissa il seed per la simulazione *)
   risorseCorrenti = risorseIniziali; 
   turniRestanti = turniIniziali; 
   simulazioneIniziata = True; 
   log = "Simulazione iniziata...\n";

   (* Configurazione parametri in base alla difficolt\[AGrave] *)
	ConfiguraParametri[];

   (* Calcolo della strategia ottimale *)
   soluzioneOttimale = CalcolaStrategiaOttimale[
     turniIniziali, risorseIniziali, costoRaccolta, beneficioRaccolta, costoEsplorazione, beneficioEsplorazione
   ];
   
   If[soluzioneOttimale["BeneficioMassimo"] === 0,
    Print["Errore: strategia non valida."],
    Print["Strategia ottimale calcolata correttamente."]
   ];
  ),
  Enabled -> Dynamic[!simulazioneIniziata]
 ], "Avvia il simulatore con un seed specifico e calcola la strategia ottimale."
 ],

     Spacer[10],
	
(* Funzione per controllare lo stato delle risorse e terminare la partita se necessario *)
ControllaRisorse[] := Module[{},
  If[risorseCorrenti <= 0,
    CreateDialog[{
      TextCell["Hai perso! Non puoi continuare il gioco.", FontWeight -> Bold, FontColor -> Red],
      TextCell["Clicca 'Pulisci e Ricomincia' per iniziare una nuova partita."]
    }];
    simulazioneIniziata = False;
  ];
];
     
(* Pulsanti per le azioni di gioco *)
Row[{
	   (* Button: Raccogli Risorse *)
	   Tooltip[
	    Button["Raccogli Risorse", 
	     If[turniRestanti > 0 && risorseCorrenti >= costoRaccolta,
	      risorseCorrenti -= costoRaccolta;
	      risorseCorrenti += beneficioRaccolta;
	      turniRestanti--; azioniRaccolta++;
	      
	      (* Aggiungi al log degli eventi *)
	      log = Column[{
	        Row[{
	          Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
	          Style["Raccogli Risorse", Blue],
	          "   -   Risorse Spese: ", Style[ToString[costoRaccolta], Italic],
	          "   |   Guadagno: ", Style[ToString[beneficioRaccolta], Bold],
	          "   |   Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
	        }],
	        log
	      }];
	      
	    (* Controllo delle risorse *)
	    ControllaRisorse[];
	
	      (* Aggiungi informazioni di debug *)
	      AggiungiDebug["DEBUG: Turno " <> ToString[turniIniziali - turniRestanti] <> 
	        ": Raccogli Risorse - Risorse Spese: " <> ToString[costoRaccolta] <> 
	        ", Guadagno: " <> ToString[beneficioRaccolta] <> 
	        ", Risorse Totali: " <> ToString[risorseCorrenti]];
	      
	      If[turniRestanti == 0,
	       log = Column[{
	         Spacer[10],
	         Style["--- PARTITA CONCLUSA! ---", Bold, Darker[Green]],
	         "Turni Totali: " <> ToString[turniIniziali],
	         "Risorse Finali: " <> ToString[risorseCorrenti],
	         "Azioni Raccogli Risorse: " <> ToString[azioniRaccolta],
	         "Azioni Costruisci Struttura: " <> ToString[azioniCostruzione],
	         "Azioni Esplora: " <> ToString[azioniEsplorazione],
	         Spacer[10],
	         Style["Premi 'Pulisci e Ricomincia' per iniziare una nuova partita!", Italic, Darker[Red]],
	         Spacer[10], log
	       }]
	      ]
	     ], Enabled -> Dynamic[turniRestanti > 0 && simulazioneIniziata]
	    ], Dynamic[
		  "Raccogli Risorse: Costo " <> ToString[costoRaccolta] <> 
		  " risorse\nGuadagno: " <> ToString[beneficioRaccolta] <> 
		  "\nRisorse Totali: " <> ToString[risorseCorrenti]
		]
	   ],
	   
	   Spacer[20],
	
	   (* Button: Costruisci Struttura *)
	   Tooltip[
	    Button["Costruisci Struttura", 
	     If[turniRestanti > 0 && risorseCorrenti >= costoCostruzione,
	      Module[{incrementoRendimento},
	       incrementoRendimento = Switch[difficolta, "Facile", 8, "Medio", 10, "Difficile", 12];
	       risorseCorrenti -= costoCostruzione;
	       rendimentoCostruzione += incrementoRendimento;
	       turniRestanti--; azioniCostruzione++;
	       
	       (* Aggiungi al log degli eventi *)
	       log = Column[{
		      Row[{
		        Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
		        Style["Costruisci Struttura", Darker[Green]],
		        "   -   Risorse Spese: ", Style[ToString[costoCostruzione], Italic],
		        "   |   Guadagno Passivo: ", Style[ToString[incrementoRendimento], Bold],
		        "   |   Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
		      }],
		      log
			}];
				
		    (* Controllo delle risorse *)
		    ControllaRisorse[];
	
	       (* Aggiungi informazioni di debug *)
	       AggiungiDebug["DEBUG: Turno " <> ToString[turniIniziali - turniRestanti] <> 
	         ": Costruisci Struttura - Risorse Spese: " <> ToString[costoCostruzione] <> 
	         ", Guadagno Passivo: " <> ToString[incrementoRendimento] <> 
	         ", Risorse Totali: " <> ToString[risorseCorrenti]];
	       
	       If[turniRestanti == 0,
	        log = Column[{
	          Spacer[10],
	          Style["--- PARTITA CONCLUSA! ---", Bold, Darker[Green]],
	          "Turni Totali: " <> ToString[turniIniziali],
	          "Risorse Finali: " <> ToString[risorseCorrenti],
	          "Azioni Raccogli Risorse: " <> ToString[azioniRaccolta],
	          "Azioni Costruisci Struttura: " <> ToString[azioniCostruzione],
	          "Azioni Esplora: " <> ToString[azioniEsplorazione],
	          Spacer[10],
	          Style["Grazie per aver giocato!", Italic, Darker[Gray]],
	          Spacer[10], log
	        }]
	       ]
	      ]
	     ], Enabled -> Dynamic[turniRestanti > 0 && simulazioneIniziata]
	    ], Dynamic["Costruisci struttura: Costo " <> ToString[costoCostruzione] <> 
    " risorse\nGuadagno passivo: " <> ToString[rendimentoCostruzione] <> " per turno."
	   ]
	   ],
	
	   Spacer[20],
	
	   (* Button: Esplora *)
	   Tooltip[
	    Button["Esplora", 
	     If[turniRestanti > 0 && risorseCorrenti >= costoEsplorazione,
	      Module[{guadagnoEsplorazione},
	       guadagnoEsplorazione = RandomInteger[beneficioEsplorazione];
	       risorseCorrenti -= costoEsplorazione;
	       risorseCorrenti += guadagnoEsplorazione;
	       turniRestanti--; azioniEsplorazione++;
	       
	       (* Aggiungi al log degli eventi *)
	       log = Column[{
		      Row[{
		        Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
		        Style["Esplora", Darker[Red]],
		        "   -   Risorse Spese: ", Style[ToString[costoEsplorazione], Italic],
		        "   |   Guadagno Variabile: ", Style[ToString[guadagnoEsplorazione], Bold],
		        "   |   Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
		      }],
		      log
			}];
			
		    (* Controllo delle risorse *)
		    ControllaRisorse[];
	
	       (* Aggiungi informazioni di debug *)
	       AggiungiDebug["DEBUG: Turno " <> ToString[turniIniziali - turniRestanti] <> 
	         ": Esplora - Risorse Spese: " <> ToString[costoEsplorazione] <> 
	         ", Guadagno Variabile: " <> ToString[guadagnoEsplorazione] <> 
	         ", Risorse Totali: " <> ToString[risorseCorrenti]];
	       
	       If[turniRestanti == 0,
	        log = Column[{
	          Spacer[10],
	          Style["--- PARTITA CONCLUSA! ---", Bold, Darker[Green]],
	          "Turni Totali: " <> ToString[turniIniziali],
	          "Risorse Finali: " <> ToString[risorseCorrenti],
	          "Azioni Raccogli Risorse: " <> ToString[azioniRaccolta],
	          "Azioni Costruisci Struttura: " <> ToString[azioniCostruzione],
	          "Azioni Esplora: " <> ToString[azioniEsplorazione],
	          Spacer[10],
	          Style["Grazie per aver giocato!", Italic, Darker[Gray]],
	          Spacer[10], log
	        }]
	       ]
	      ]
	     ], Enabled -> Dynamic[turniRestanti > 0 && simulazioneIniziata]
	    ], Dynamic["Costruisci struttura: Costo " <> ToString[costoCostruzione] <> 
   " risorse\nGuadagno passivo: " <> ToString[rendimentoCostruzione] <> " per turno."
   ]
   ],
	
	   Spacer[20],
	
	(* Pulsante: Ricevi un consiglio *)
Tooltip[
 Button["Ricevi un consiglio",
  Module[{turnoCorrente, azioneSuggerita, spiegazione, roiCostruzione, guadagnoEsplorazione, rischioEsplorazione},
   turnoCorrente = turniIniziali - turniRestanti + 1;

   If[risorseCorrenti <= 0,
    (* Partita persa se risorse sono a 0 o negative *)
    CreateDialog[{
      TextCell["Hai perso! Non puoi continuare il gioco.", FontWeight -> Bold, FontColor -> Red],
      TextCell["Clicca 'Pulisci e Ricomincia' per iniziare una nuova partita."]
    }];
    simulazioneIniziata = False;
    Return[];
   ];

   If[turniRestanti > 0,
    (* Calcolo del ROI per costruzione *)
    roiCostruzione = If[rendimentoCostruzione > 0, 
      Ceiling[costoCostruzione / rendimentoCostruzione], Infinity];

    (* Guadagno medio per esplorazione *)
    guadagnoEsplorazione = Mean[beneficioEsplorazione];

    (* Rischio di esplorazione: risorse negative dopo esplorazione *)
    rischioEsplorazione = risorseCorrenti - costoEsplorazione;

    (* Logica del consiglio *)
    Which[
     (* Caso 1: Costruire se ROI \[EGrave] vantaggioso e ci sono risorse sufficienti *)
     risorseCorrenti >= costoCostruzione && roiCostruzione <= turniRestanti && turnoCorrente < turniIniziali / 2,
     azioneSuggerita = Style["Costruisci", Blue];
     spiegazione = "Costruire \[EGrave] vantaggioso: ROI in " <> ToString[roiCostruzione] <> 
       " turni. Guadagno passivo massimo.",

     (* Caso 2: Esplorare se garantisce un guadagno netto positivo e risorse non rischiano il negativo *)
     risorseCorrenti >= costoEsplorazione && guadagnoEsplorazione > costoEsplorazione && rischioEsplorazione > 0,
     azioneSuggerita = Style["Esplora", Blue];
     spiegazione = "Esplorare \[EGrave] conveniente: guadagno netto positivo dalle risorse extra.",

     (* Caso 3: Raccogliere risorse se siamo nei turni finali o altre opzioni non sono possibili *)
     risorseCorrenti >= costoRaccolta && turnoCorrente >= turniIniziali / 2,
     azioneSuggerita = Style["Raccogli Risorse", Blue];
     spiegazione = "Raccogli risorse per prepararti agli ultimi turni.",

     (* Caso 4: Nessuna azione possibile *)
     True,
     azioneSuggerita = Style["Nessuna azione consigliata", Blue];
     spiegazione = "Risorse insufficienti per qualsiasi azione."
    ];
   ,
   (* Partita terminata *)
   azioneSuggerita = Style["Nessuna azione suggerita", Blue];
   spiegazione = "Partita terminata o parametri non validi.";
   ];

   (* Mostra il consiglio *)
   CreateDialog[{
     TextCell["Consiglio per il turno " <> ToString[turnoCorrente] <> ":", FontWeight -> Bold],
     TextCell[azioneSuggerita],
     TextCell[spiegazione, FontWeight -> Bold]
   }];
  ],
  Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
 ],
 "Ricevi un consiglio basato sulla strategia ottimale per il turno corrente."
]

 }]

     Spacer[10],
     
Row[{
 Tooltip[
  Button["Mostra Risultati", 
   log = Column[{
     "Statistiche finali:",
     "Turni Rimanenti: " <> ToString[turniRestanti],
     "Risorse Rimanenti: " <> ToString[risorseCorrenti],
     Style["", Bold],
     log
    }];
   Enabled -> Dynamic[simulazioneIniziata]
  ], "Mostra le statistiche finali della simulazione."
 ],
 
 Spacer[10],
 
Tooltip[
 Button["Mostra Soluzione", 
  Module[{beneficio, azioni},
   beneficio = Lookup[soluzioneOttimale, "BeneficioMassimo", Null];
   azioni = Lookup[soluzioneOttimale, "Azioni", {}];
   If[beneficio =!= Null && Length[azioni] > 0,
    CreateDialog[{TextCell["Soluzione Ottimale:"], 
      TextCell["Beneficio Totale: " <> ToString[beneficio] <> 
        "\nAzioni Ottimali:\n" <> StringJoin[Riffle[azioni, "\n"]], FontWeight -> Bold]}],
    CreateDialog[{TextCell["Errore: Soluzione non valida o non calcolata.", FontWeight -> Bold]}]
   ]
  ]
 ], "Mostra la soluzione ottimale calcolata per il problema attuale."],
 
 Spacer[10],
 
	Tooltip[
	 Button["Verifica Risultato", 
	  Module[{beneficio},
	   beneficio = Lookup[soluzioneOttimale, "BeneficioMassimo", Null];
	   If[beneficio =!= Null,
	    CreateDialog[{TextCell[
	      If[risorseCorrenti == beneficio,
	       "Risultato Corretto! Hai ottenuto il massimo beneficio.",
	       "Risultato Errato. Il massimo beneficio \[EGrave]: " <> ToString[beneficio]
	      ], FontWeight -> Bold]}],
	    CreateDialog[{TextCell["Errore: Soluzione non valida o non calcolata.", FontWeight -> Bold]}]
	   ]
	  ]
	 ], "Verifica se il risultato raggiunto corrisponde alla soluzione ottimale."]
	}],
  
 Spacer[10],
 
     Row[{
       Button["Mostra Debug Log", 
        CreateDialog[{TextCell["Debug Log:"], 
          Pane[Dynamic[StringJoin[Riffle[debugLog, "\n"]]], {400, 300}, Scrollbars -> True]}, 
         WindowTitle -> "Debug Log"]]
     }],
     Spacer[10],
     (* Pulsante per pulire l'interfaccia *)
     Tooltip[
      Button["Pulisci e Ricomincia", 
       (
        risorseCorrenti = 100; 
        turniRestanti = 10; 
        simulazioneIniziata = False; 
        log = "";
        ), Enabled -> True
       ], "Reimposta l'interfaccia per iniziare un nuovo esercizio."
       ],
     Spacer[10],
     
     (* Log degli eventi *)
     Style["Log degli Eventi:", Bold],
     Pane[Dynamic[log], {800, 200}, Scrollbars -> True]
     }]
   ];

End[];
EndPackage[];
