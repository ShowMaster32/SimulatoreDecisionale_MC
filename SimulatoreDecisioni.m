(* ::Package:: *)

BeginPackage["SimulatoreDecisioni`"];

(* Funzione principale per avviare il gioco *)
AvviaGioco::usage = "AvviaGioco[] lancia la simulazione interattiva completa.";

Begin["`Private`"];

(* Variabile globale per raccogliere i messaggi di debug *)
debugLog = {};

(* Funzione per aggiungere messaggi al log di debug *)
AggiungiDebug[message_] := AppendTo[debugLog, message];

(* Funzione per verificare il risultato *)
VerificaRisultato[risorseFinali_, soluzioneOttimale_] := 
 If[risorseFinali == soluzioneOttimale[[1]],
  "Risultato Corretto! Hai ottenuto il massimo beneficio.",
  "Risultato Errato. Il massimo beneficio \[EGrave]: " <> 
   ToString[soluzioneOttimale[[1]]]]

(* Funzione per calcolare la strategia ottimale con programmazione dinamica *)
CalcolaStrategiaOttimale[turni_, risorse_, costoRacc_, benefRacc_, costoEspl_, benefEspl_] :=
 Module[{dp, beneficioEsplorazioneRange, maxRisorse, t, r},

  (* Controllo immediato sui parametri *)
  AggiungiDebug["DEBUG: Entrato nel Module"];
  AggiungiDebug["DEBUG: Parametri: " <> ToString[{turni, risorse, costoRacc, benefRacc, costoEspl, benefEspl}]];

  (* Gamma dei benefici esplorazione *)
  beneficioEsplorazioneRange = If[ListQ[benefEspl], Range @@ benefEspl, {0}];
  AggiungiDebug["DEBUG: Beneficio Esplorazione Range: " <> ToString[beneficioEsplorazioneRange]];

  (* Controllo parametri validi *)
  If[turni <= 0 || risorse <= 0, 
   AggiungiDebug["Errore: Turni o Risorse non validi."];
   Return["Errore: Turni o Risorse non validi."]
  ];

  (* Tabella di programmazione dinamica *)
  maxRisorse = risorse + 1;
  dp = Table[{0, ""}, {t, 0, turni}, {r, 0, risorse}];
  dp[[1, All]] = {0, ""};
  AggiungiDebug["DEBUG: Tabella DP inizializzata"];

  (* DEBUG: Controllo sulla tabella inizializzata *)
  Print["DEBUG: Tabella DP inizializzata - Primo elemento: ", dp[[1, 1]]];

  (* Iterazione su turni e risorse *)
  For[t = 1, t <= turni, t++,
   For[r = 0, r <= risorse, r++,
	AggiungiDebug["DEBUG: Turno: " <> ToString[t] <> ", Risorse: " <> ToString[r]];
	
    (* Debug all'inizio dei cicli *)
    Print["DEBUG: Entrando nel ciclo - Turno: ", t, ", Risorse: ", r];

    (* Azione: Raccogli Risorse *)
    If[r >= costoRacc,
     dp[[t + 1, r + 1]] = 
      Max[dp[[t + 1, r + 1]], 
       {dp[[t, r - costoRacc + 1]][[1]] + benefRacc, 
        dp[[t, r - costoRacc + 1]][[2]] <> "Raccogli Risorse\n"}]
        AggiungiDebug["DEBUG: Raccogli Risorse eseguita, Valore DP aggiornato: " <> 
       ToString[dp[[t + 1, r + 1]]]];
    ];

    (* Azione: Esplora *)
    If[r >= costoEspl,
     dp[[t + 1, r + 1]] = 
      Max[dp[[t + 1, r + 1]], 
       {dp[[t, r - costoEspl + 1]][[1]] + Max[beneficioEsplorazioneRange], 
        dp[[t, r - costoEspl + 1]][[2]] <> "Esplora\n"}]
        AggiungiDebug["DEBUG: Esplora eseguita, Valore DP aggiornato: " <> 
       ToString[dp[[t + 1, r + 1]]]];
    ];
   ]
  ];

  (* Debug finale *)
  AggiungiDebug["DEBUG: Valore finale DP[turni + 1, risorse + 1]: " <> 
    ToString[dp[[turni + 1, risorse + 1]]]];  

  (* Recupero della soluzione ottimale *)
  {dp[[turni + 1, risorse + 1]][[1]], dp[[turni + 1, risorse + 1]][[2]]}
]

(* Funzione per mostrare la soluzione in una finestra pop-up *)
MostraSoluzione[soluzione_] :=
 CreateDialog[{TextCell["Soluzione Ottimale:"], 
    TextCell[soluzione, FontWeight -> Bold]}, 
   WindowTitle -> "Soluzione"];

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

(* Pulsante per avviare la simulazione con seed *)
Tooltip[
 Button["Inizia Simulazione",
 (
   SeedRandom[seedNumerico]; (* Fissa il seed per la simulazione *)
   risorseCorrenti = risorseIniziali; 
   turniRestanti = turniIniziali; 
   simulazioneIniziata = True; 
   log = "Simulazione iniziata...\n";

   (* Configurazione parametri in base alla difficolt\[AGrave] *)
   Switch[difficolta,
  "Facile", (
    costoRaccolta = 10; beneficioRaccolta = 30; 
    costoCostruzione = 35; rendimentoCostruzione = 8;
    costoEsplorazione = 15; beneficioEsplorazione = {20, 70}
  ),
  "Medio", (
    costoRaccolta = 15; beneficioRaccolta = 25; 
    costoCostruzione = 40; rendimentoCostruzione = 10;
    costoEsplorazione = 20; beneficioEsplorazione = {10, 50}
  ),
  "Difficile", (
    costoRaccolta = 15; beneficioRaccolta = RandomInteger[{15, 30}]; 
    costoCostruzione = 50; rendimentoCostruzione = 12;
    costoEsplorazione = 25; 
    beneficioEsplorazione = {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]}
  )
];

   (* Calcolo della strategia ottimale dopo aver impostato i parametri *)
   strategiaOttimale = 
     CalcolaStrategiaOttimale[turniRestanti, risorseCorrenti, 
       costoRaccolta, beneficioRaccolta, costoCostruzione, 
       beneficioCostruzione, rendimentoCostruzione, costoEsplorazione, 
       beneficioEsplorazione];
 ), Enabled -> Dynamic[!simulazioneIniziata]
], "Avvia il simulatore con un seed specifico."],
     Spacer[10],
     
	(* Pulsanti per le azioni di gioco *)
	(* Valori predefiniti per le variabili *)
	costoRaccolta = 15;
	beneficioRaccolta = 25;
	costoCostruzione = 40;
	beneficioCostruzione = 50;
	costoEsplorazione = 20;
	beneficioEsplorazione = {10, 50};
	
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
	         Style["Premi 'Pulisci' per iniziare una nuova partita!", Italic, Darker[Red]],
	         Spacer[10], log
	       }]
	      ]
	     ], Enabled -> Dynamic[turniRestanti > 0 && simulazioneIniziata]
	    ], "Raccogli risorse: Costo " <> ToString[costoRaccolta] <> " risorse."
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
	         Spacer[10], log
	       }];
	
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
	    ], "Costruisci struttura: Costo " <> ToString[costoCostruzione] <> " risorse."
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
	         Spacer[10], log
	       }];
	
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
	    ], "Esplora nuove aree: Costo " <> ToString[costoEsplorazione] <> 
	     " risorse\nGuadagno variabile: da " <> ToString[beneficioEsplorazione[[1]]] <> 
	     " a " <> ToString[beneficioEsplorazione[[2]]] <> " risorse."
	   ]
	}],

     Spacer[10],
     
     (* Pulsante per mostrare risultati *)
     Tooltip[
      Button["Mostra Risultati", 
       log = Column[{
   "Statistiche finali:",
   "Turni Rimanenti: " <> ToString[turniRestanti],
   "Risorse Rimanenti: " <> ToString[risorseCorrenti],
   Style["", Bold], (* Aggiunge una linea vuota *)
   log
}];
       Enabled -> Dynamic[simulazioneIniziata]
       ], "Mostra le statistiche finali della simulazione."],
     Spacer[10],
     
     (* Pulsante per mostrare la soluzione *)
Tooltip[
 Button["Mostra Soluzione", 
  If[ListQ[strategiaOttimale] && Length[strategiaOttimale] >= 2, 
   MostraSoluzione[
    "Beneficio Totale: " <> ToString[strategiaOttimale[[1]]] <> 
    "\nAzioni Ottimali:\n" <> strategiaOttimale[[2]]
   ],
   CreateDialog[{TextCell["Errore: La strategia ottimale non \[EGrave] valida.", FontWeight -> Bold]}]
  ], Enabled -> Dynamic[simulazioneIniziata]],
 "Mostra la soluzione ottimale calcolata per il problema attuale."],
   
   (* Pulsante per verificare il risultato *)
Tooltip[
 Button["Verifica Risultato", 
   Module[{feedback},
    feedback = VerificaRisultato[risorseCorrenti, strategiaOttimale];
    CreateDialog[{TextCell[feedback, FontWeight -> Bold]}]
   ],
   Enabled -> Dynamic[simulazioneIniziata]
 ], "Verifica se il risultato raggiunto corrisponde alla soluzione ottimale."],
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
      Button["Pulisci", 
       (
        risorseCorrenti = 100; 
        turniRestanti = 10; 
        simulazioneIniziata = False; 
        log = "";
        ), Enabled -> True
       ], "Reimposta l'interfaccia per iniziare un nuovo esercizio."],
     Spacer[10],
     
     (* Log degli eventi *)
     Style["Log degli Eventi:", Bold],
     Pane[Dynamic[log], {800, 200}, Scrollbars -> True]
     }]
   ];

End[];
EndPackage[];

