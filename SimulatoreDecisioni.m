(* ::Package:: *)

BeginPackage["SimulatoreDecisioni`"];

(* Funzione principale per avviare il gioco *)
AvviaGioco::usage = "AvviaGioco[] lancia la simulazione interattiva completa.";

Begin["`Private`"];

(* Funzione per verificare il risultato *)
VerificaRisultato[risorseFinali_, soluzioneOttimale_] := 
 If[risorseFinali == soluzioneOttimale[[1]],
  "Risultato Corretto! Hai ottenuto il massimo beneficio.",
  "Risultato Errato. Il massimo beneficio \[EGrave]: " <> 
   ToString[soluzioneOttimale[[1]]]]

(* Funzione per calcolare la strategia ottimale con programmazione dinamica *)
CalcolaStrategiaOttimale[turni_, risorse_, costoRacc_, benefRacc_, costoCostr_, benefCostr_, rendimentoCostr_, costoEspl_, benefEspl_] :=
 Module[{dp, azioniOttimali, beneficioEsplorazioneRange},

  (* Gamma dei benefici esplorazione *)
  beneficioEsplorazioneRange = If[ListQ[benefEspl], Range @@ benefEspl, {0}];

  (* Controllo parametri validi *)
  If[turni <= 0 || risorse <= 0, Return["Errore: Turni o Risorse non validi."]];

  (* Tabella di programmazione dinamica *)
  dp = Table[{0, ""}, {t, 0, turni}, {r, 0, risorse}];
  dp[[1, All]] = {0, ""}; (* Stato base *)

  (* Iterazione sui turni e risorse *)
  For[t = 1, t <= turni, t++,
   For[r = 0, r <= risorse, r++,

    (* Azione: Raccogli Risorse *)
    If[r >= costoRacc && t - 1 >= 1 && ListQ[dp[[t - 1, r - costoRacc]]],
     dp[[t, r]] = Max[dp[[t, r]],
       {dp[[t - 1, r - costoRacc]][[1]] + benefRacc,
        StringJoin[If[StringQ[dp[[t - 1, r - costoRacc]][[2]]], 
                      dp[[t - 1, r - costoRacc]][[2]], ""], "Raccogli Risorse\n"]}]];

    (* Azione: Esplora *)
    If[r >= costoEspl && t - 1 >= 1 && ListQ[dp[[t - 1, r - costoEspl]]],
     dp[[t, r]] = Max[dp[[t, r]],
       {dp[[t - 1, r - costoEspl]][[1]] + Max[beneficioEsplorazioneRange],
        StringJoin[If[StringQ[dp[[t - 1, r - costoEspl]][[2]]], 
                      dp[[t - 1, r - costoEspl]][[2]], ""], "Esplora\n"]}]];
   ]
  ];

  (* Recupero della strategia ottimale *)
  azioniOttimali = dp[[turni, risorse]][[2]];
  {dp[[turni, risorse]][[1]], azioniOttimali}
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
      log = Column[{
         Row[{
           Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
           Style["Raccogli Risorse", Blue],
           " - Risorse Spese: ", Style[ToString[costoRaccolta], Italic],
           ", Guadagno: ", Style[ToString[beneficioRaccolta], Bold],
           ", Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
         }],
         Style["", Bold], log}]
     ], Enabled -> Dynamic[simulazioneIniziata]],
    "Raccogli risorse: Costo " <> ToString[costoRaccolta] <> " risorse."
   ],
   
   Spacer[10],
   
   (* Button: Costruisci Struttura *)
   Tooltip[
    Button["Costruisci Struttura", 
     If[turniRestanti > 0 && risorseCorrenti >= costoCostruzione,
      Module[{incrementoRendimento},
       incrementoRendimento = Switch[difficolta, "Facile", 8, "Medio", 10, "Difficile", 12];
       risorseCorrenti -= costoCostruzione; 
       rendimentoCostruzione += incrementoRendimento;
       turniRestanti--; azioniCostruzione++;
       log = Column[{
          Row[{
            Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
            Style["Costruisci Struttura", Darker[Green]],
            " - Risorse Spese: ", Style[ToString[costoCostruzione], Italic],
            ", Guadagno Passivo: ", Style[ToString[incrementoRendimento], Bold],
            ", Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
          }],
          Style["", Bold], log}]
      ]], Enabled -> Dynamic[simulazioneIniziata]],
    "Costruisci struttura: Costo " <> ToString[costoCostruzione] <> " risorse."
   ],
   
   Spacer[10],

   (* Button: Esplora *)
Tooltip[
 Button["Esplora", 
  If[turniRestanti > 0 && risorseCorrenti >= costoEsplorazione,
   Module[{guadagnoEsplorazione, intervallo},
    intervallo = Switch[difficolta, 
      "Facile", {20, 70}, 
      "Medio", {10, 50}, 
      "Difficile", {-30, 80},
      _, {0, 0}]; (* Default fallback se il livello di difficolt\[AGrave] \[EGrave] invalido *)

    (* Verifica se l'intervallo \[EGrave] valido *)
    If[Length[intervallo] == 2 && intervallo[[1]] <= intervallo[[2]],
     guadagnoEsplorazione = RandomInteger[intervallo],
     guadagnoEsplorazione = 0]; (* Guadagno zero in caso di errore *)

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
       Style["", Bold], log}]
   ]], Enabled -> Dynamic[simulazioneIniziata]],

 Dynamic[
  Module[{cE, bMin, bMax},
   Switch[difficolta,
    "Facile", (cE = 15; bMin = 20; bMax = 70),
    "Medio", (cE = 20; bMin = 10; bMax = 50),
    "Difficile", (cE = 25; bMin = -30; bMax = 80)];
   "Esplora nuove aree:\n - Costo: " <> ToString[cE] <> 
   " risorse\n - Guadagno variabile: da " <> ToString[bMin] <> 
   " a " <> ToString[bMax] <> " risorse."
  ]]
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

