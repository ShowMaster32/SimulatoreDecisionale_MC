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
        "Facile: guadagni elevati e costi ridotti.\nMedio: bilanciato.\nDifficile: guadagni bassi e costi elevati."]
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
           costoCostruzione = 35; beneficioCostruzione = 60; rendimentoCostruzione = 14;
           costoEsplorazione = 15; beneficioEsplorazione = {20, 70}),
         "Medio", (costoRaccolta = 15; beneficioRaccolta = 25; 
           costoCostruzione = 40; beneficioCostruzione = 50; rendimentoCostruzione = 14;
           costoEsplorazione = 20; beneficioEsplorazione = {10, 50}),
         "Difficile", (costoRaccolta = 20; beneficioRaccolta = 20; 
           costoCostruzione = 50; beneficioCostruzione = 40; rendimentoCostruzione = 14;
           costoEsplorazione = 25; beneficioEsplorazione = {5, 30})];
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
  Tooltip[
   Button["Raccogli Risorse", 
    If[turniRestanti > 0 && risorseCorrenti >= costoRaccolta,
     risorseCorrenti -= costoRaccolta; 
     risorseCorrenti += beneficioRaccolta;
     turniRestanti--; 
     azioniRaccolta++;
     log = StringJoin["Turno ", ToString[turniIniziali - turniRestanti], 
       ": Raccogli Risorse\n", log]], 
    Enabled -> Dynamic[simulazioneIniziata]
    ], 
   Dynamic[
    "Raccogli risorse:\n - Costo: " <> ToString[costoRaccolta] <>
     " risorse\n - Guadagno: " <> ToString[beneficioRaccolta] <> " risorse."
    ]
   ],
   
  Tooltip[
   Button["Costruisci Struttura", 
    If[turniRestanti > 0 && risorseCorrenti >= costoCostruzione,
     risorseCorrenti -= costoCostruzione; 
     risorseCorrenti += beneficioCostruzione; 
     rendimentoCostruzione += 14; 
     turniRestanti--; 
     azioniCostruzione++;
     log = StringJoin["Turno ", ToString[turniIniziali - turniRestanti], 
       ": Costruisci Struttura\n", log]], 
    Enabled -> Dynamic[simulazioneIniziata]
    ], 
   Dynamic[
    "Costruisci una struttura:\n - Costo: " <> ToString[costoCostruzione] <>
     " risorse\n - Guadagno immediato: " <> ToString[beneficioCostruzione] <>
     " risorse\n - Guadagno per turno: +14 risorse."
    ]
   ],

  Tooltip[
   Button["Esplora", 
    If[turniRestanti > 0 && risorseCorrenti >= costoEsplorazione,
     risorseCorrenti -= costoEsplorazione; 
     risorseCorrenti += RandomInteger[beneficioEsplorazione];
     turniRestanti--; 
     azioniEsplorazione++;
     log = StringJoin["Turno ", ToString[turniIniziali - turniRestanti], 
       ": Esplora\n", log]], 
    Enabled -> Dynamic[simulazioneIniziata]
    ], 
   Dynamic[
    "Esplora nuove aree:\n - Costo: " <> ToString[costoEsplorazione] <>
     " risorse\n - Guadagno variabile: da " <> 
     ToString[beneficioEsplorazione[[1]]] <> " a " <> 
     ToString[beneficioEsplorazione[[2]]] <> " risorse."
    ]
   ]
}]

     Spacer[10],
     
     (* Pulsante per mostrare risultati *)
     Tooltip[
      Button["Mostra Risultati", 
       log = StringJoin["Statistiche finali:\nTurni Rimanenti: ", 
         ToString[turniRestanti], "\nRisorse Rimanenti: ", 
         ToString[risorseCorrenti], "\n", log], 
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
     Pane[Dynamic[log], {400, 200}, Scrollbars -> True]
     }]
   ];

End[];
EndPackage[];

