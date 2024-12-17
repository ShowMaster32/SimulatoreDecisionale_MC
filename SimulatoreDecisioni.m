(* ::Package:: *)

BeginPackage["SimulatoreDecisioni`"];

(* Funzione principale per avviare il gioco *)
AvviaGioco::usage = "AvviaGioco[] lancia la simulazione interattiva completa.";

Begin["`Private`"];

(* Funzione per calcolare la strategia ottimale con programmazione dinamica *)
CalcolaStrategiaOttimale[turni_, risorse_, costoRacc_, benefRacc_, costoCostr_, benefCostr_, rendimentoCostr_, costoEspl_, benefEspl_] :=
 Module[{dp, azioniOttimali, beneficioEsplorazioneRange},
  
  (* Gamma dei benefici esplorazione *)
  beneficioEsplorazioneRange = Range[Sequence @@ benefEspl];
  
  (* Tabella di programmazione dinamica *)
  dp = Table[{-Infinity, ""}, {t, 0, turni}, {r, 0, risorse}];
  dp[[1, All]] = {0, ""}; (* Stato base: con 0 turni rimasti, il guadagno \[EGrave] 0 *)
  
  (* Iterazione sui turni e risorse *)
  For[t = 1, t <= turni, t++,
   For[r = 0, r <= risorse, r++,
    
    (* Azione: Raccogli Risorse *)
    If[r >= costoRacc,
     dp[[t, r]] = Max[dp[[t, r]], 
       {dp[[t - 1, r - costoRacc]][[1]] + benefRacc, 
        StringJoin[dp[[t - 1, r - costoRacc]][[2]], "Raccogli Risorse\n"]}];
     ];
    
    (* Azione: Costruisci Struttura *)
    If[r >= costoCostr,
     dp[[t, r]] = Max[dp[[t, r]], 
       {dp[[t - 1, r - costoCostr]][[1]] + benefCostr + rendimentoCostr * (turni - t), 
        StringJoin[dp[[t - 1, r - costoCostr]][[2]], "Costruisci Struttura\n"]}];
     ];
    
    (* Azione: Esplora *)
    If[r >= costoEspl,
     dp[[t, r]] = Max[dp[[t, r]], 
       {dp[[t - 1, r - costoEspl]][[1]] + Max[beneficioEsplorazioneRange], 
        StringJoin[dp[[t - 1, r - costoEspl]][[2]], "Esplora\n"]}];
     ];
    ]
   ];
  
  (* Recupero della strategia ottimale *)
  azioniOttimali = dp[[turni, risorse]][[2]];
  {dp[[turni, risorse]][[1]], azioniOttimali}
  ];

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
    beneficioEsplorazione, rendimentoCostruzione = 0, 
    strategiaOttimale = ""
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
     
     (* Pulsante per avviare la simulazione *)
     Tooltip[
      Button["Inizia Simulazione", 
       (
        risorseCorrenti = risorseIniziali; 
        turniRestanti = turniIniziali; 
        simulazioneIniziata = True; 
        log = "Simulazione iniziata...\n";
		Switch[difficolta,
		  "Facile", (costoRaccolta = 10; beneficioRaccolta = 30; 
		    costoCostruzione = 35; rendimentoCostruzione = 8;
		    costoEsplorazione = 15; beneficioEsplorazione = {20, 70}),
		  "Medio", (costoRaccolta = 15; beneficioRaccolta = 25; 
		    costoCostruzione = 40; rendimentoCostruzione = 10;
		    costoEsplorazione = 20; beneficioEsplorazione = {10, 50}),
		  "Difficile", (costoRaccolta = 20; beneficioRaccolta = 20; 
		    costoCostruzione = 50; rendimentoCostruzione = 12;
		    costoEsplorazione = 25; beneficioEsplorazione = {5, 30})
		];
        ), Enabled -> Dynamic[!simulazioneIniziata]
       ], "Avvia il simulatore con i parametri selezionati."],
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
   (* Aggiorna risorse e log *)
   risorseCorrenti -= costoRaccolta; 
   risorseCorrenti += beneficioRaccolta;
   turniRestanti--; azioniRaccolta++;
   log = Column[{
      Row[{
        Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
        Style["Raccogli Risorse", Blue],
        " - Risorse Spese: ", Style[ToString[costoRaccolta], Italic],
        ", Guadagno: ", Style[ToString[beneficioRaccolta], Bold],
        ", Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
      }],
         Style["", Bold], (* Linea vuota tra un turno e l'altro *)
      log
    }]
  ],
  Enabled -> Dynamic[simulazioneIniziata]
 ],
 Dynamic[
  Module[{cR, bR},
   Switch[difficolta,
    "Facile", (cR = 10; bR = 30),
    "Medio", (cR = 15; bR = 25),
    "Difficile", (cR = 20; bR = 20)];
   "Raccogli risorse:\n - Costo: " <> ToString[cR] <> 
   " risorse\n - Guadagno: " <> ToString[bR] <> " risorse."
  ]
 ]
]

   Spacer[10],

   (* Button: Costruisci Struttura *)
Tooltip[
 Button["Costruisci Struttura", 
  If[turniRestanti > 0 && risorseCorrenti >= costoCostruzione,
   (* Aggiorna risorse e log *)
   risorseCorrenti -= costoCostruzione; 
   rendimentoCostruzione += Switch[difficolta, "Facile", 8, "Medio", 10, "Difficile", 12]; 
   turniRestanti--; azioniCostruzione++;
   log = Column[{
      Row[{
        Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
        Style["Costruisci Struttura", Darker[Green]],
        " - Risorse Spese: ", Style[ToString[costoCostruzione], Italic],
        ", Guadagno Passivo: ", Style[ToString[rendimentoCostruzione], Bold],
        ", Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
      }],
      Style["", Bold], (* Aggiunge una nuova linea *)
      log
    }]
  ],
  Enabled -> Dynamic[simulazioneIniziata]
 ],
 Dynamic[
  Module[{cC, rC},
   Switch[difficolta,
    "Facile", (cC = 35; rC = 8),
    "Medio", (cC = 40; rC = 10),
    "Difficile", (cC = 50; rC = 12)];
   "Costruisci struttura:\n - Costo: " <> ToString[cC] <> 
   " risorse\n - Guadagno passivo per turno: +" <> ToString[rC] <> " risorse."
  ]
 ]
]
   Spacer[10],

   (* Button: Esplora *)
   Tooltip[
 Button["Esplora", 
  If[turniRestanti > 0 && risorseCorrenti >= costoEsplorazione,
   Module[{guadagnoEsplorazione},
    (* Calcola il guadagno *)
    guadagnoEsplorazione = RandomInteger[
      Switch[difficolta, 
       "Facile", {20, 70}, 
       "Medio", {10, 50}, 
       "Difficile", {5, 30}
      ]
    ];
    (* Aggiorna risorse e log *)
    risorseCorrenti -= costoEsplorazione; 
    risorseCorrenti += guadagnoEsplorazione;
    turniRestanti--; azioniEsplorazione++;

    log = Column[{
       Row[{
         Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
         Style["Esplora", Darker[Red]],
         " - Risorse Spese: ", Style[ToString[costoEsplorazione], Italic],
         ", Guadagno Variabile: ", Style[ToString[guadagnoEsplorazione], Bold],
         ", Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
       }],
          Style["", Bold], (* Linea vuota tra un turno e l'altro *)
       log
    }]
   ]
  ],
  Enabled -> Dynamic[simulazioneIniziata]
 ],
 Dynamic[
  Module[{cE, bMin, bMax},
   Switch[difficolta,
    "Facile", (cE = 15; bMin = 20; bMax = 70),
    "Medio", (cE = 20; bMin = 10; bMax = 50),
    "Difficile", (cE = 25; bMin = 5; bMax = 30)];
   "Esplora nuove aree:\n - Costo: " <> ToString[cE] <> 
   " risorse\n - Guadagno variabile: da " <> ToString[bMin] <> 
   " a " <> ToString[bMax] <> " risorse."
  ]
 ]
]
}]

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
       MostraSoluzione["Soluzione ottimale ancora da implementare."], 
       Enabled -> Dynamic[simulazioneIniziata]
       ], "Mostra la soluzione della simulazione."],
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

