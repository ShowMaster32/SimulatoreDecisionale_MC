(* ::Package:: *)

BeginPackage["SimulatoreDecisioni`"];

(* Funzione principale per avviare il gioco *)
AvviaGioco::usage = "AvviaGioco[] lancia la simulazione interattiva completa.";

Begin["`Private`"];

(* Funzione per calcolare la strategia ottimale con programmazione dinamica *)
CalcolaStrategiaOttimale[turni_, risorse_, costoRacc_, benefRacc_, costoCostr_, benefCostr_, rendimentoCostr_, costoEspl_, benefEspl_] :=
 Module[{dp, azioniOttimali, beneficioEsplorazioneRange},
  
  beneficioEsplorazioneRange = Range[Sequence @@ benefEspl];
  dp = Table[{-Infinity, ""}, {t, 0, turni}, {r, 0, risorse}];
  dp[[1, All]] = {0, ""}; (* Stato base: con 0 turni rimasti, il guadagno \[EGrave] 0 *)
  
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
    risorseOttimali = 0, strategiaOttimale = "", esercizioSeed = 0, 
    rispostaUtente = "", verificaFeedback = ""
    },
   Column[{
     Style["Simulatore di Ottimizzazione delle Scelte", Bold, 18],
     Style["Per iniziare, personalizzare i parametri e cliccare su 'Inizia Simulazione'", Italic, Darker@Gray],
     
     (* Personalizzazione dei parametri *)
     Row[{
       "Numero di Turni: ", 
       Slider[Dynamic[turniIniziali, (turniIniziali = Round[#]) &], {5, 20}, 
        Enabled -> Dynamic[!simulazioneIniziata]],
       Dynamic[Style[turniIniziali, Bold]], Spacer[10],
       "Risorse Iniziali: ", 
       Slider[Dynamic[risorseIniziali, (risorseIniziali = Round[#]) &], {50, 200}, 
        Enabled -> Dynamic[!simulazioneIniziata]],
       Dynamic[Style[risorseIniziali, Bold]], Spacer[10],
       Tooltip[
        "Difficolt\[AGrave]: ", 
        "Facile: guadagni elevati e costi ridotti.\nMedio: bilanciato.\nDifficile: guadagni bassi e costi elevati."
        ],
       PopupMenu[Dynamic[difficolta], {"Facile", "Medio", "Difficile"}, 
        Enabled -> Dynamic[!simulazioneIniziata]]
       }],
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
     
     (* Button per avviare la simulazione *)
     Spacer[10],
     Tooltip[
      Button["Inizia Simulazione", 
       (
        risorseCorrenti = risorseIniziali; 
        turniRestanti = turniIniziali; 
        azioniRaccolta = 0; azioniCostruzione = 0; azioniEsplorazione = 0;
        rendimentoCostruzione = 0; 
        log = "";
        (* Imposta parametri in base alla difficolt\[AGrave] *)
        Switch[difficolta,
         "Facile", (costoRaccolta = 10; beneficioRaccolta = 30; 
           costoCostruzione = 35; beneficioCostruzione = 60; 
           rendimentoCostruzione = 14; 
           costoEsplorazione = 15; beneficioEsplorazione = {20, 70}),
         "Medio", (costoRaccolta = 15; beneficioRaccolta = 25; 
           costoCostruzione = 40; beneficioCostruzione = 50; 
           rendimentoCostruzione = 14; 
           costoEsplorazione = 20; beneficioEsplorazione = {10, 50}),
         "Difficile", (costoRaccolta = 20; beneficioRaccolta = 20; 
           costoCostruzione = 50; beneficioCostruzione = 40; 
           rendimentoCostruzione = 14; 
           costoEsplorazione = 25; beneficioEsplorazione = {5, 30})
         ];
        simulazioneIniziata = True;
        ), Enabled -> Dynamic[!simulazioneIniziata]
       ],
      "Clicca per iniziare la simulazione con i parametri selezionati."
      ],
     Tooltip[
      Button["Ricomincia", 
       (
        simulazioneIniziata = False;
        risorseCorrenti = 100;
        turniRestanti = 10;
        log = "";
        rendimentoCostruzione = 0;
        ), Enabled -> Dynamic[simulazioneIniziata]
       ],
      "Clicca per ricominciare la simulazione."
      ],
     
     (* Button di gioco *)
     Spacer[10],
     Row[{
       Tooltip[
        Button["Raccogli Risorse", 
         If[turniRestanti > 0 && risorseCorrenti >= costoRaccolta, 
          turniRestanti--;
          risorseCorrenti -= costoRaccolta;
          risorseCorrenti += beneficioRaccolta;
          azioniRaccolta++;
          log = Row[{Style["Turno: ", Bold], ToString[turniIniziali - turniRestanti], "\n", 
            Style["Azione: Raccogli Risorse\n", Bold], 
            "Risorse spese: ", ToString[costoRaccolta], "\n",
            "Risorse guadagnate: ", ToString[beneficioRaccolta], "\n", log}];
          , 
          log = Row[{Style["Azione non disponibile.\n", Red], log}];
          ], Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
         ],
        Row[{"Costo: ", ToString[costoRaccolta], " | Guadagno: ", ToString[beneficioRaccolta]}]
        ],
       Tooltip[
        Button["Costruisci Struttura", 
         If[turniRestanti > 0 && risorseCorrenti >= costoCostruzione, 
          turniRestanti--; 
          risorseCorrenti -= costoCostruzione; 
          risorseCorrenti += beneficioCostruzione; 
          azioniCostruzione++; 
          rendimentoCostruzione += 14; 
          log = Row[{Style["Turno: ", Bold], ToString[turniIniziali - turniRestanti], "\n", 
            Style["Azione: Costruisci Struttura\n", Bold], 
            "Risorse spese: ", ToString[costoCostruzione], "\n",
            "Rendimento incrementale: +", ToString[14], 
            " per turno.\n", log}];
          , 
          log = Row[{Style["Azione non disponibile.\n", Red], log}];
          ], Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
         ],
        Row[{"Costo: ", ToString[costoCostruzione], " | Guadagno: ", ToString[beneficioCostruzione]}]
        ],
       Tooltip[
        Button["Esplora", 
         If[turniRestanti > 0 && risorseCorrenti >= costoEsplorazione, 
          turniRestanti--;
          risorseCorrenti -= costoEsplorazione;
          Module[{guadagno = RandomInteger[beneficioEsplorazione]},
           risorseCorrenti += guadagno;
           log = Row[{Style["Turno: ", Bold], ToString[turniIniziali - turniRestanti], "\n", 
             Style["Azione: Esplora\n", Bold], 
             "Risorse spese: ", ToString[costoEsplorazione], "\n",
             "Guadagno variabile: ", ToString[guadagno], "\n", log}];
           ];
          azioniEsplorazione++;
          , 
          log = Row[{Style["Azione non disponibile.\n", Red], log}];
          ], Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
         ],
        Row[{"Costo: ", ToString[costoEsplorazione], " | Guadagno: Variabile"}]
        ]
       }],
     
     (* Button di gioco *)
     Spacer[10],
     Tooltip[
      Button["Mostra Risultati", 
       (
        log = Row[{Style["\nStatistiche Finali:\n", Bold], 
          "Risorse Finali: ", ToString[risorseCorrenti], "\n",
          "Turni Rimanenti: ", ToString[turniRestanti], "\n",
          "Azioni Eseguite:\n",
          "  Raccolta: ", ToString[azioniRaccolta], "\n",
          "  Costruzioni: ", ToString[azioniCostruzione], "\n",
          "  Esplorazioni: ", ToString[azioniEsplorazione], "\n",
          "Guadagno attuale per turno: ", ToString[rendimentoCostruzione], "\n", log}];
        ), Enabled -> Dynamic[simulazioneIniziata && turniRestanti < turniIniziali]
       ],
      "Clicca per visualizzare le statistiche finali della simulazione."
      ],
     Tooltip[
      Button["Mostra Strategia Ottimale", 
       (
        strategiaOttimale = 
         CalcolaStrategiaOttimale[turniIniziali, risorseIniziali, 
           costoRaccolta, beneficioRaccolta, costoCostruzione, 
           beneficioCostruzione, rendimentoCostruzione, costoEsplorazione, 
           beneficioEsplorazione];
        log = Row[{Style["\nStrategia Ottimale:\n", Bold], strategiaOttimale[[2]], 
          "\nMassimo rendimento previsto: ", ToString[strategiaOttimale[[1]]], "\n", log}];
        ), Enabled -> Dynamic[simulazioneIniziata && turniRestanti == 0]
       ],
      "Calcola e visualizza la sequenza di azioni ottimale per massimizzare le risorse."
      ],
     
     (* Log degli eventi *)
     Style["Log degli Eventi:", Bold],
     Pane[Dynamic[log], {400, 200}, Scrollbars -> True]
     }]
   ];

End[];
EndPackage[];

