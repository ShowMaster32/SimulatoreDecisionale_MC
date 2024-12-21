(* ::Package:: *)

(* ::Package:: *)
(**)


BeginPackage["SimulatoreDecisioni`"];

(* Funzione principale per avviare il gioco *)
(* Questa funzione avvia l'interfaccia interattiva del simulatore, permettendo all'utente di personalizzare i parametri e avviare la simulazione. *)
AvviaGioco::usage = "AvviaGioco[] lancia la simulazione interattiva completa.";

Begin["`Private`"];

(* Variabile globale per raccogliere i messaggi di debug *)
(* Questa variabile \[EGrave] una lista utilizzata per registrare i messaggi di debug generati durante l'esecuzione del simulatore. *)
debugLog = {};

(* Funzione per aggiungere messaggi al log di debug *)
(* 
   AggiungiDebug[message_]
   Parametri:
   - message: Stringa contenente il messaggio da aggiungere al log.
   Funzione:
   Aggiunge il messaggio fornito alla lista globale `debugLog`.
*)
AggiungiDebug[message_] := AppendTo[debugLog, message];

(* Variabile globale che rappresenta la soluzione ottimale *)
(* 
   soluzioneOttimale \[EGrave] un'associazione che contiene:
   - "BeneficioMassimo": Il massimo beneficio ottenibile.
   - "Azioni": Una lista delle azioni ottimali per raggiungere il beneficio massimo.
   Inizialmente viene impostata a valori di default.
*)
soluzioneOttimale = <|"BeneficioMassimo" -> 0, "Azioni" -> {}|>;

(* Funzione dedicata per configurare tutti i parametri dinamicamente in base alla difficolt\[AGrave] *)
(* 
   ConfiguraParametri[]
   Funzione:
   Configura i costi e benefici delle azioni (raccolta, costruzione, esplorazione) in base al livello di difficolt\[AGrave] selezionato.
   - "Facile": Costi bassi, benefici elevati.
   - "Medio": Bilanciamento intermedio tra costi e benefici.
   - "Difficile": Costi alti, benefici variabili.
   Logica:
   Utilizza uno switch per assegnare i parametri specifici.
   Inoltre, aggiunge un messaggio di debug con i valori configurati.
*)
ConfiguraParametri[] := Module[{},
  Switch[difficolta,
    "Facile", (
      (* Configurazione per difficolt\[AGrave] "Facile" *)
      costoRaccolta = 10; (* Costo per l'azione di raccolta risorse *)
      beneficioRaccolta = 30; (* Beneficio ottenuto dalla raccolta *)
      costoCostruzione = 35; (* Costo per costruire una struttura *)
      beneficioCostruzione = 8; (* Guadagno passivo derivante dalla struttura *)
      costoEsplorazione = 15; (* Costo per esplorare *)
      beneficioEsplorazione = {20, 70}; (* Beneficio variabile dall'esplorazione *)
      rendimentoCostruzione = 8; (* Guadagno per turno derivante dalla struttura *)
    ),
    "Medio", (
      (* Configurazione per difficolt\[AGrave] "Medio" *)
      costoRaccolta = 15; 
      beneficioRaccolta = 25;
      costoCostruzione = 40; 
      beneficioCostruzione = 10;
      costoEsplorazione = 20; 
      beneficioEsplorazione = {10, 50};
      rendimentoCostruzione = 10;
    ),
    "Difficile", (
    (* Configurazione per difficolt\[AGrave] "Difficile" *)
      costoRaccolta = 15; 
      beneficioRaccolta = RandomInteger[{15, 30}];
      costoCostruzione = 50; 
      beneficioCostruzione = 12;
      costoEsplorazione = 25; 
      beneficioEsplorazione = {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]};
      rendimentoCostruzione = 12;
    )
  ];
  (* Registra i parametri configurati nel log di debug *)
  AggiungiDebug[
   "Parametri configurati: costoRaccolta = " <> ToString[costoRaccolta] <>
    ", beneficioRaccolta = " <> ToString[beneficioRaccolta] <>
    ", costoCostruzione = " <> ToString[costoCostruzione] <>
    ", beneficioCostruzione = " <> ToString[beneficioCostruzione] <>
    ", costoEsplorazione = " <> ToString[costoEsplorazione] <>
    ", beneficioEsplorazione = " <> ToString[beneficioEsplorazione] <> "\n"
  ];
];


(* Funzione per estrarre le azioni utente dal log *)
(* 
   EstraiMosseDaLog[log_]
   Parametri:
   - log: Una stringa contenente il log completo delle azioni effettuate dall'utente.
   Funzione:
   - Estrae le mosse effettuate dall'utente dal log.
   - Ogni azione \[EGrave] associata al turno in cui \[EGrave] stata eseguita.
   Ritorna:
   - Una lista di associazioni con chiave "Turno" e "Azione".
*)
EstraiMosseDaLog[log_] := 
 Module[{righe, mosse = {}, turno, azione},
  righe = StringSplit[log, "\n"]; (* Divide il log in righe *)
  Do[
   If[StringMatchQ[righe[[i]], "Turno *: *"], 
    turno = StringCases[righe[[i]], "Turno " ~~ DigitCharacter .. ~~ ":"]; (* Estrae il numero del turno *)
    azione = StringCases[righe[[i]], ": " ~~ WordCharacter ..]; (* Estrae il nome dell'azione *)
    AppendTo[mosse, <|"Turno" -> ToExpression[First[turno]], "Azione" -> First[azione]|>]; (* Aggiunge l'azione alla lista *)
   ],
   {i, Length[righe]}
  ];
  mosse
];

(* Confronta le azioni utente estratte dal log con la simulazione ottimale *)
ConfrontaMosseDaLog[log_, soluzioneSimulata_] := 
 Module[{mosseUtente, differenze = {}, azioniSimulate, i},
  mosseUtente = EstraiMosseDaLog[log];
  azioniSimulate = Lookup[soluzioneSimulata, "Azioni"];
  For[i = 1, i <= Length[mosseUtente], i++,
   If[mosseUtente[[i, "Azione"]] =!= azioniSimulate[[i]],
    AppendTo[differenze, 
     "Turno " <> ToString[mosseUtente[[i, "Turno"]]] <> 
      ": Utente ha scelto " <> mosseUtente[[i, "Azione"]] <> 
      ", ma la soluzione ottimale era " <> azioniSimulate[[i]]
    ];
   ];
  ];
  differenze
];

(* Funzione per calcolare la strategia ottimale con programmazione dinamica *)
(* 
   CalcolaStrategiaOttimale[turni_, risorse_, costoRacc_, benefRacc_, costoEspl_, benefEspl_]
   Parametri:
   - turni: Numero di turni disponibili per la simulazione.
   - risorse: Quantit\[AGrave] iniziale di risorse disponibili.
   - costoRacc_: Costo di una raccolta.
   - benefRacc_: Beneficio derivante da una raccolta.
   - costoEspl_: Costo di un'esplorazione.
   - benefEspl_: Benefici derivanti dall'esplorazione (range).
   Funzione:
   - Calcola la strategia ottimale per massimizzare il beneficio.
   Ritorna:
   - Un'associazione con "BeneficioMassimo" e "Azioni" ottimali.
*)
CalcolaStrategiaOttimale[turni_, risorse_, costoRacc_, benefRacc_, costoEspl_, benefEspl_] :=
 Module[{dp, beneficioEsplorazioneRange, t, r, maxBeneficio, azioniOttimali},
  (* Log delle variabili iniziali *)
  AggiungiDebug["Inizio Calcolo: turni = ", turni, ", risorse = ", risorse];

  (* Verifica dei parametri *)
  If[turni <= 0 || risorse <= 0 || costoRacc <= 0 || costoEspl <= 0,
   AggiungiDebug["Parametri non validi"];
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
  AggiungiDebug["Matrice DP finale: ", dp];

  (* Estrazione del risultato finale *)
  azioniOttimali = dp[[turni + 1, risorse + 1]];
  AggiungiDebug["Beneficio massimo trovato: ", azioniOttimali[[1]]];
  <|"BeneficioMassimo" -> azioniOttimali[[1]], "Azioni" -> azioniOttimali[[2]]|>
];

(* Funzione per mostrare la soluzione in una finestra pop-up *)
(* 
   MostraSoluzione[soluzione_]
   Parametri:
   - soluzione: Associazione con "BeneficioMassimo" e "Azioni".
   Funzione:
   - Mostra una finestra di dialogo con il beneficio totale e le azioni ottimali.
*)
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

(* Funzione per controllare lo stato delle risorse e terminare la partita se necessario *)
(* 
   ControllaRisorse[costo_, azione_, risorseCorrenti_, log_]
   Parametri:
   - costo: Costo dell'azione corrente.
   - azione: Nome dell'azione.
   - risorseCorrenti: Quantit\[AGrave] di risorse disponibili.
   - log: Variabile per registrare i messaggi.
   Funzione:
   - Controlla se le risorse sono sufficienti per l'azione.
   - Aggiorna il log o termina la simulazione in caso di insufficienza.
   Ritorna:
   - True se l'azione pu\[OGrave] essere eseguita; False altrimenti.
*)
ControllaRisorse[costo_, azione_, risorseCorrenti_, log_] := 
 Module[{},
  AggiungiDebug[
   "Chiamata ControllaRisorse per azione: " <> azione <> 
    " | Costo: " <> ToString[costo] <> 
    " | Risorse Correnti: " <> ToString[risorseCorrenti]
  ];
  
  If[risorseCorrenti < costo,
   AggiungiDebug[
    "Partita persa: nessuna azione possibile con risorse correnti (" <> 
    ToString[risorseCorrenti] <> 
    " disponibili.)"
   ];
   
   (* Aggiorna il log con messaggio ben visibile *)
	log = Column[{
      Row[{
        Style["Turno terminato: ", Bold, Red],
        Style["Partita persa!", Italic, Darker[Red]],
        Style["Risorse insufficienti: " <> ToString[risorseCorrenti], Bold]
      }], log
    }];
   
   (* Mostra dialog di notifica *)
   CreateDialog[
    Column[{
      Style["Partita Terminata!", Bold, 16, Red],
      Style["Non puoi pi\[UGrave] eseguire alcuna azione con le risorse disponibili.", Italic],
      Style["Premi 'Reset Esercizio' per avviare una nuova simulazione.", Bold, Darker[Gray]]
    }],
    WindowTitle -> "Fine Partita"
   ];
   (* Segnala che la simulazione \[EGrave] terminata *)
   simulazioneIniziata = False;
   Return[False]; (* Blocco esecuzione *)
  ];
  
  AggiungiDebug["Risorse sufficienti per eseguire: " <> azione];
  True
 ];



(* Funzione per avviare il simulatore *)
(* 
   AvviaGioco[]
   Funzione principale che inizializza e gestisce l'interfaccia grafica dinamica del simulatore.
   - Fornisce i controlli per personalizzare i parametri di simulazione.
   - Visualizza il titolo e l'interfaccia per avviare la simulazione.
   - Permette di impostare il numero di turni, le risorse iniziali e la difficolt\[AGrave].
   Variabili locali:
   - risorseCorrenti: risorse disponibili all'inizio, impostata di default a 100.
   - turniRestanti: turni residui durante la simulazione, impostati di default a 10.
   - log: registro dinamico delle azioni svolte e degli eventi.
   - simulazioneIniziata: flag booleano che indica se la simulazione \[EGrave] attiva.
   - difficolta: livello di difficolt\[AGrave] selezionato (Facile, Medio, Difficile).
   - seedNumerico: valore del seed per generare numeri casuali, impostato di default a 42.
*)
AvviaGioco[] := DynamicModule[
   {
    risorseCorrenti = 100, turniRestanti = 10, log = "", 
    azioniRaccolta = 0, azioniCostruzione = 0, azioniEsplorazione = 0, 
    simulazioneIniziata = False, risorseIniziali = 100, turniIniziali = 10, 
    difficolta = "Medio", costoRaccolta, beneficioRaccolta, 
    costoCostruzione, beneficioCostruzione, costoEsplorazione, 
    beneficioEsplorazione, rendimentoCostruzione = 0, 
    strategiaOttimale = "", seedNumerico = 42
    },
   
   (* Interfaccia del Simulatore *)
   (* 
      La sezione seguente gestisce la creazione dell'interfaccia grafica.
      Comprende titolo, personalizzazione dei parametri, pulsanti per avviare la simulazione,
      e opzioni per visualizzare risultati e soluzioni.
   *)
   Column[{
     
     (* Titolo del Simulatore *)
     (* Visualizza il titolo e una breve descrizione iniziale. *)
     Style["Simulatore di Ottimizzazione delle Scelte", Bold, 18],
     Style["Per iniziare, personalizzare i parametri e cliccare su 'Genera Esercizio'", Italic, Darker@Gray],
     
     (* Personalizzazione dei parametri *)
     (* 
        Controlli per configurare:
        - Numero di turni (slider tra 5 e 20).
        - Risorse iniziali (slider tra 50 e 200).
        - Livello di difficolt\[AGrave] (popup con tre opzioni: Facile, Medio, Difficile).
     *)
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
        "Facile: guadagni elevati e costi ridotti.\nMedio: bilanciato.\nDifficile: guadagni bassi e costi elevati."],
       PopupMenu[Dynamic[difficolta], {"Facile", "Medio", "Difficile"}, 
        Enabled -> Dynamic[!simulazioneIniziata]]
       }],
     Spacer[10],
     
     (* Log dei parametri aggiornati *)
     (* Mostra dinamicamente i parametri correnti selezionati dall'utente. *)
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
     (* 
        Controllo per impostare il seed (valore compreso tra 0 e 99).
        - Tooltip spiega come utilizzare il controllo.
        - InputField convalidato per garantire l'inserimento di valori accettabili.
     *)
     seedNumerico = 42; (* Valore predefinito valido *)

     Row[{
       Tooltip[
         Style["Seed (0-99): ", Bold],
         "Inserisci un numero intero compreso tra 0 e 99 per impostare il seed del generatore casuale. Se il valore non \[EGrave] valido, verr\[AGrave] impostato a 42 come default."
       ],
       InputField[
         Dynamic[
           seedNumerico, 
           (* Validazione dell'input *)
           (If[
             IntegerQ[#] && 0 <= # <= 99, (* Controlla che sia un intero tra 0 e 99 *)
             seedNumerico = #, (* Assegna il valore se valido *)
             (
               seedNumerico = 42; (* Reimposta al valore di default *)
               CreateDialog[
                 Column[{
                   TextCell["Errore: Il seed deve essere un numero intero tra 0 e 99.", FontWeight -> Bold, FontColor -> Red],
                   TextCell["Il valore \[EGrave] stato impostato a 42 di default."]
                 }],
                 WindowTitle -> "Seed non valido"
               ]
             )
           ]) &
         ], 
         FieldSize -> 6, 
         Enabled -> Dynamic[!simulazioneIniziata]
       ]
     }],

     Spacer[10],

     (* Bottone per avviare la simulazione *)
     (* 
        Pulsante che avvia la simulazione e calcola la strategia ottimale.
        - Fissa il seed specificato.
        - Configura i parametri in base alla difficolt\[AGrave].
        - Calcola la strategia ottimale e registra il risultato.
     *)
     Tooltip[
      Button["Genera Esercizio",
       (
        SeedRandom[seedNumerico]; (* Fissa il seed per la simulazione *)
        risorseCorrenti = risorseIniziali; 
        turniRestanti = turniIniziali; 
        simulazioneIniziata = True; 
        log = "Simulazione iniziata...\n";

        (* Configura i parametri in base alla difficolt\[AGrave] *)
        ConfiguraParametri[];

        (* Calcolo della strategia ottimale *)
        soluzioneOttimale = CalcolaStrategiaOttimale[
          turniIniziali, risorseIniziali, costoRaccolta, beneficioRaccolta, costoEsplorazione, beneficioEsplorazione
        ];

        (* Log del risultato della strategia *)
        If[soluzioneOttimale["BeneficioMassimo"] === 0,
         AggiungiDebug["Errore: strategia non valida."],
         AggiungiDebug["Strategia ottimale calcolata correttamente. Beneficio massimo: " <> 
           ToString[soluzioneOttimale["BeneficioMassimo"]]]
        ];
       ),
       Enabled -> Dynamic[!simulazioneIniziata]
      ], "Avvia il simulatore con un seed specifico e calcola la strategia ottimale."
     ],
     Spacer[10],
     
(* Pulsanti per le azioni di gioco *)
(* 
   La sezione seguente contiene i pulsanti di azione principali del simulatore, 
   inclusi "Raccogli Risorse" e "Costruisci Struttura". Ogni pulsante attiva 
   una specifica logica basata sullo stato corrente delle risorse e dei turni rimanenti.
*)
Row[{

  (* Pulsante: Raccogli Risorse *)
  Tooltip[
    Button["Raccogli Risorse", 
      Module[
        {
          costoCostruzioneEffettivo, incrementoRendimentoEffettivo,
          costoRaccoltaEffettivo, beneficioRaccoltaEffettivo,
          costoEsplorazioneEffettivo, beneficioEsplorazioneEffettivo
        },
        (* Calcolo dinamico dei parametri basati sulla difficolt\[AGrave] *)
        costoCostruzioneEffettivo = Switch[difficolta, 
          "Facile", 35, 
          "Medio", 40, 
          "Difficile", 50
        ];
        incrementoRendimentoEffettivo = Switch[difficolta, 
          "Facile", 8, 
          "Medio", 10, 
          "Difficile", 12
        ];
        costoRaccoltaEffettivo = Switch[difficolta, 
          "Facile", 10, 
          "Medio", 15, 
          "Difficile", 20
        ];
        beneficioRaccoltaEffettivo = Switch[difficolta, 
          "Facile", 30, 
          "Medio", 25, 
          "Difficile", RandomInteger[{15, 30}]
        ];
        costoEsplorazioneEffettivo = Switch[difficolta, 
          "Facile", 15, 
          "Medio", 20, 
          "Difficile", 25
        ];
        beneficioEsplorazioneEffettivo = RandomInteger[
          Switch[difficolta, 
            "Facile", {20, 70}, 
            "Medio", {10, 50}, 
            "Difficile", {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]}]
        ];

        (* Controllo risorse per eseguire l'azione *)
        If[ControllaRisorse[costoRaccoltaEffettivo, "Raccogli Risorse", risorseCorrenti, log],
          (* Aggiorna risorse e registro *)
          risorseCorrenti -= costoRaccoltaEffettivo; 
          risorseCorrenti += beneficioRaccoltaEffettivo; 
          turniRestanti--; 
          azioniRaccolta++;
          
          (* Aggiorna il log con le informazioni dell'azione corrente *)
          log = Column[{
            Row[{
              Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
              Style["Raccogli Risorse", Blue],
              "   -   Risorse Spese: ", Style[ToString[costoRaccoltaEffettivo], Italic],
              "   |   Guadagno: ", Style[ToString[beneficioRaccoltaEffettivo], Bold],
              "   |   Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
            }], log
          }],
          Return[] (* Termina l'azione se le risorse non sono sufficienti *)
        ];
        
        (* Controllo globale: fine partita se le risorse sono insufficienti per altre azioni *)
        ControllaRisorse[
          Min[costoRaccoltaEffettivo, costoCostruzioneEffettivo, costoEsplorazioneEffettivo],
          "Fine Partita", risorseCorrenti, log
        ];

        (* Log finale se i turni sono terminati *)
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
            Style["Premi 'Reset Esercizio' per iniziare una nuova partita!", Italic, Darker[Red]],
            Spacer[10], log
          }];
        ];
      ], 
      Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
    ], 
    Dynamic[
      "Raccogli Risorse: Costo " <> 
      ToString[
        Switch[difficolta, 
          "Facile", 10, 
          "Medio", 15, 
          "Difficile", 20]
      ] <> 
      " risorse\nGuadagno: " <> 
      ToString[
        Switch[difficolta, 
          "Facile", 30, 
          "Medio", 25, 
          "Difficile", RandomInteger[{15, 30}]
        ]
      ]
    ]
  ],
	
Spacer[10],
	  
(* Pulsante: Costruisci Struttura *)
(* 
   Questo pulsante consente al giocatore di investire risorse per costruire una struttura. 
   La struttura fornisce un guadagno passivo per i turni successivi.
   Logica:
   - Legge i valori dinamici di costo e rendimento in base alla difficolt\[AGrave] selezionata.
   - Controlla se il giocatore ha sufficienti risorse per costruire.
   - Aggiorna il numero di turni e le risorse.
   - Registra l'azione nel log.
*)
Tooltip[
 Button["Costruisci Struttura", 
  Module[
   {
    (* Calcola i valori dinamici in base alla difficolt\[AGrave] selezionata *)
    costoCostruzioneEffettivo, incrementoRendimentoEffettivo,
    costoRaccoltaEffettivo, beneficioRaccoltaEffettivo,
    costoEsplorazioneEffettivo, beneficioEsplorazioneEffettivo
   },
    (* Calcola i valori dinamici in base alla difficolt\[AGrave] selezionata *)
    costoCostruzioneEffettivo = Switch[difficolta, 
     "Facile", 35, 
     "Medio", 40, 
     "Difficile", 50
   ];
   incrementoRendimentoEffettivo = Switch[difficolta, 
     "Facile", 8, 
     "Medio", 10, 
     "Difficile", 12
   ];
    costoRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 10, 
     "Medio", 15, 
     "Difficile", 20
   ];
   beneficioRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 30, 
     "Medio", 25, 
     "Difficile", RandomInteger[{15, 30}]
   ];
   costoEsplorazioneEffettivo = Switch[difficolta, 
     "Facile", 15, 
     "Medio", 20, 
     "Difficile", 25
   ];
   beneficioEsplorazioneEffettivo = RandomInteger[
     Switch[difficolta, 
       "Facile", {20, 70}, 
       "Medio", {10, 50}, 
       "Difficile", {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]}]
   ];

      (* Controlla se le risorse sono sufficienti per costruire la struttura *)
      If[ControllaRisorse[costoCostruzioneEffettivo, "Costruisci Struttura", risorseCorrenti, log],
        (* Aggiorna le risorse e incrementa il rendimento passivo *)
        risorseCorrenti -= costoCostruzioneEffettivo;
        rendimentoCostruzione += incrementoRendimentoEffettivo;
        turniRestanti--; 
        azioniCostruzione++;
        
        (* Registra l'azione nel log *)
        log = Column[{
          Row[{
            Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
            Style["Costruisci Struttura", Darker[Green]],
            "   -   Risorse Spese: ", Style[ToString[costoCostruzioneEffettivo], Italic],
            "   |   Guadagno Passivo: ", Style[ToString[incrementoRendimentoEffettivo], Bold],
            "   |   Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
          }], log
        }],
        Return[] (* Termina l'azione se le risorse sono insufficienti *)
      ];

      (* Controlla globalmente se le risorse sono insufficienti per continuare *)
      ControllaRisorse[
        Min[costoRaccoltaEffettivo, costoCostruzioneEffettivo, costoEsplorazioneEffettivo],
        "Fine Partita", risorseCorrenti, log
      ];

      (* Log conclusivo se i turni sono terminati *)
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
        }];
      ];
    ],
    Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
  ],
  (* Descrizione dinamica del pulsante *)
  Dynamic["Costruisci struttura: Costo " <> ToString[
    Switch[difficolta, 
      "Facile", 35, 
      "Medio", 40, 
      "Difficile", 50
    ]] <> 
    " risorse\nGuadagno passivo: " <> 
    ToString[
      Switch[difficolta, 
        "Facile", 8, 
        "Medio", 10, 
        "Difficile", 12
      ]] <> " per turno."
  ]
],

Spacer[10],
	
(* Pulsante: Esplora *)
(* 
   Questo pulsante consente al giocatore di esplorare nuovi territori per ottenere risorse variabili.
   Logica:
   - Legge i valori dinamici di costo e benefici in base alla difficolt\[AGrave] selezionata.
   - Controlla se il giocatore ha sufficienti risorse per esplorare.
   - Aggiorna il numero di turni e le risorse correnti.
   - Registra l'azione nel log.
*)
Tooltip[
 Button["Esplora", 
  Module[
   {
    costoCostruzioneEffettivo, incrementoRendimentoEffettivo,
    costoRaccoltaEffettivo, beneficioRaccoltaEffettivo,
    costoEsplorazioneEffettivo, beneficioEsplorazioneEffettivo
   },
    (* Calcola i valori dinamici in base alla difficolt\[AGrave] selezionata *)
    costoCostruzioneEffettivo = Switch[difficolta, 
     "Facile", 35, 
     "Medio", 40, 
     "Difficile", 50
   ];
   incrementoRendimentoEffettivo = Switch[difficolta, 
     "Facile", 8, 
     "Medio", 10, 
     "Difficile", 12
   ];
    costoRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 10, 
     "Medio", 15, 
     "Difficile", 20
   ];
   beneficioRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 30, 
     "Medio", 25, 
     "Difficile", RandomInteger[{15, 30}]
   ];
   costoEsplorazioneEffettivo = Switch[difficolta, 
     "Facile", 15, 
     "Medio", 20, 
     "Difficile", 25
   ];
   beneficioEsplorazioneEffettivo = RandomInteger[
     Switch[difficolta, 
       "Facile", {20, 70}, 
       "Medio", {10, 50}, 
       "Difficile", {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]}]
   ];

      (* Controlla se le risorse sono sufficienti per esplorare *)
      If[ControllaRisorse[costoEsplorazioneEffettivo, "Esplora", risorseCorrenti, log],
        (* Aggiorna le risorse e registra il guadagno variabile *)
        risorseCorrenti -= costoEsplorazioneEffettivo;
        risorseCorrenti += beneficioEsplorazioneEffettivo;
        turniRestanti--; 
        azioniEsplorazione++;
        
        (* Registra l'azione nel log *)
        log = Column[{
          Row[{
            Style["Turno " <> ToString[turniIniziali - turniRestanti] <> ": ", Bold],
            Style["Esplora", Darker[Red]],
            "   -   Risorse Spese: ", Style[ToString[costoEsplorazioneEffettivo], Italic],
            "   |   Guadagno Variabile: ", Style[ToString[beneficioEsplorazioneEffettivo], Bold],
            "   |   Risorse Totali: ", Style[ToString[risorseCorrenti], Bold]
          }], log
        }],
        Return[] (* Termina l'azione se le risorse sono insufficienti *)
      ];

      (* Controlla globalmente se le risorse sono insufficienti per continuare *)
      ControllaRisorse[
        Min[costoRaccoltaEffettivo, costoCostruzioneEffettivo, costoEsplorazioneEffettivo],
        "Fine Partita", risorseCorrenti, log
      ];

      (* Log conclusivo se i turni sono terminati *)
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
        }];
      ];
    ],
    Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
  ],
  (* Descrizione dinamica del pulsante *)
  Dynamic["Esplora: Costo " <> ToString[
    Switch[difficolta, 
      "Facile", 15, 
      "Medio", 20, 
      "Difficile", 25
    ]] <> 
    " risorse\nGuadagno variabile: " <> 
    ToString[
      Switch[difficolta, 
        "Facile", "{20, 70}", 
        "Medio", "{10, 50}", 
        "Difficile", "{-30, 80}"
      ]]
  ]
],

Spacer[10],
	
(* Pulsante: Suggerimento Strategico *)
(* 
   Questo pulsante fornisce al giocatore un consiglio strategico basato sulla situazione corrente.
   Logica:
   - Calcola i valori dinamici (costo, benefici) in base alla difficolt\[AGrave] selezionata.
   - Determina la migliore azione strategica tra "Costruisci", "Esplora" e "Raccogli Risorse".
   - Mostra un dialogo con il consiglio calcolato, motivato dai valori economici e strategici.
*)
Tooltip[
 Button["Suggerimento Strategico",
  Module[
  {
    turnoCorrente, azioneSuggerita, spiegazione, azioneSuggeritaStyled, spiegazioneStyled, roiCostruzione, 
    guadagnoEsplorazione, rischioEsplorazione, costoCostruzioneEffettivo, incrementoRendimentoEffettivo,
    costoEsplorazioneEffettivo, beneficioEsplorazioneEffettivo, costoRaccoltaEffettivo, beneficioRaccoltaEffettivo
   },
   
    (* Calcola il turno corrente *)
   turnoCorrente = turniIniziali - turniRestanti + 1;
    (* Calcola dinamicamente i parametri in base alla difficolt\[AGrave] *)
   costoCostruzioneEffettivo = Switch[difficolta, 
     "Facile", 35, 
     "Medio", 40, 
     "Difficile", 50
   ];
   incrementoRendimentoEffettivo = Switch[difficolta, 
     "Facile", 8, 
     "Medio", 10, 
     "Difficile", 12
   ];
   costoEsplorazioneEffettivo = Switch[difficolta, 
     "Facile", 15, 
     "Medio", 20, 
     "Difficile", 25
   ];
   beneficioEsplorazioneEffettivo = RandomInteger[
     Switch[difficolta, 
       "Facile", {20, 70}, 
       "Medio", {10, 50}, 
       "Difficile", {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]}]
   ];
   costoRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 10, 
     "Medio", 15, 
     "Difficile", 20
   ];
   beneficioRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 30, 
     "Medio", 25, 
     "Difficile", RandomInteger[{15, 30}]
   ];
   
    (* Calcola il rischio di esplorazione (risorse residue dopo l'azione) *)
   rischioEsplorazione = risorseCorrenti - costoEsplorazioneEffettivo;

    (* Verifica se la partita \[EGrave] persa per mancanza di risorse *)
   If[
    risorseCorrenti < Min[costoRaccoltaEffettivo, costoEsplorazioneEffettivo, costoCostruzioneEffettivo],
    (* Notifica la fine della partita *)
    CreateDialog[{
      TextCell["Hai perso! Non puoi continuare il gioco.", FontWeight -> Bold, FontColor -> Red],
      TextCell["Clicca 'Reset Esercizio' per iniziare una nuova partita."]
    }];
    simulazioneIniziata = False;
    Return[];
   ];

   If[turniRestanti > 0,
	
    (* Calcola il rendimento per "Costruisci Struttura" *)
	rendimentoCostruzione = 
 If[turniRestanti > Ceiling[costoCostruzioneEffettivo / incrementoRendimentoEffettivo], 
  incrementoRendimentoEffettivo * (turniRestanti - Ceiling[costoCostruzioneEffettivo / incrementoRendimentoEffettivo]),
  0
];

AggiungiDebug["rendimentoCostruzione: " <> ToString[rendimentoCostruzione-5]];

    (* Calcola il guadagno medio ponderato per "Esplora" *)
   guadagnoEsplorazionePonderato = 
    0.7 * (Max[beneficioEsplorazioneEffettivo] - costoEsplorazioneEffettivo) + 
    0.3 * (Min[beneficioEsplorazioneEffettivo] - costoEsplorazioneEffettivo);
    
    (* Calcola il guadagno netto per "Raccogli Risorse" *)
    guadagnoRaccolta = beneficioRaccoltaEffettivo - costoRaccoltaEffettivo;
	
	AggiungiDebug["guadagnoEsplorazionePonderato: " <> ToString[guadagnoEsplorazionePonderato]];
	AggiungiDebug["guadagnoRaccolta: " <> ToString[guadagnoRaccolta]]

   (* Logica del consiglio *)
Which[
     (* Caso 1: Costruire *)
     rendimentoCostruzione > Max[guadagnoEsplorazionePonderato, guadagnoRaccolta] &&
       turniRestanti > Ceiling[costoCostruzioneEffettivo / incrementoRendimentoEffettivo] &&
       risorseCorrenti >= costoCostruzioneEffettivo,
     azioneSuggerita = "Costruisci";
     spiegazione = "Costruire conviene: rendimento stimato " <> 
       ToString[rendimentoCostruzione] <> 
       " con ROI in " <> 
       ToString[Ceiling[costoCostruzioneEffettivo / incrementoRendimentoEffettivo]] <> 
       " turni.",
     
     (* Caso 2: Esplorare *)
     guadagnoEsplorazionePonderato > guadagnoRaccolta && rischioEsplorazione > 0 &&
       risorseCorrenti >= costoEsplorazioneEffettivo,
     azioneSuggerita = "Esplora";
     spiegazione = "Esplorare conviene: guadagno medio ponderato di " <> 
       ToString[guadagnoEsplorazionePonderato] <> 
       " con massimo potenziale " <> ToString[Max[beneficioEsplorazioneEffettivo]] <> 
       " e rischio minimo.",
     
     (* Caso 3: Raccogliere risorse *)
     risorseCorrenti >= costoRaccoltaEffettivo,
     azioneSuggerita = "Raccogli Risorse";
     spiegazione = "Raccogliere \[EGrave] la scelta sicura: guadagno garantito di " <> 
       ToString[guadagnoRaccolta] <> " a turno.",
     
     (* Caso alternativo: Risorse insufficienti per azione migliore *)
     True,
     azioneAlternativa = If[
       risorseCorrenti >= costoEsplorazioneEffettivo, "Esplora", "Raccogli Risorse"
     ];
     azioneSuggerita = azioneAlternativa;
     spiegazione = "L'azione migliore richiederebbe pi\[UGrave] risorse. Ti consigliamo di: " <> azioneAlternativa <> "."
    ];

   
	AggiungiDebug["Richiesto consiglio DOPO al Which"];
	AggiungiDebug["ROI: " <> ToString[roiCostruzione]];
	AggiungiDebug["Valore azioneSuggerita: " <> ToString[azioneSuggerita]];
	AggiungiDebug["Valore spiegazione: " <> ToString[spiegazione]];
	
   (* Applica lo stile *)
   azioneSuggeritaStyled = Style[azioneSuggerita, Bold, Blue];
   spiegazioneStyled = Style[spiegazione, Italic];

    (* Mostra il consiglio come dialogo *)
   CreateDialog[{
     TextCell["Consiglio per il turno " <> ToString[turnoCorrente] <> ":", FontWeight -> Bold],
     TextCell[azioneSuggeritaStyled],
     TextCell[spiegazioneStyled]
   }]];
  ],
  Enabled -> Dynamic[simulazioneIniziata && turniRestanti > 0]
 ],
 "Ricevi un suggerimento strategico basato sulla strategia ottimale per il turno corrente."
]

 }]
 
 Spacer[10],
     
Row[{

 (* Pulsante per visualizzare i risultati della simulazione *)
 Tooltip[
  Button["Mostra Risultati", 
   log = Column[{
     "Statistiche finali:", (* Testo per indicare i risultati finali *)
     "Turni Rimanenti: " <> ToString[turniRestanti], (* Visualizza i turni rimasti *)
     "Risorse Rimanenti: " <> ToString[risorseCorrenti], (* Visualizza le risorse rimanenti *)
     Style["", Bold], (* Linea vuota per migliorare la leggibilit\[AGrave] *)
     log (* Mostra il log delle azioni svolte *)
    }];
   Enabled -> Dynamic[simulazioneIniziata] (* Abilita il pulsante solo se la simulazione \[EGrave] iniziata *)
  ], "Mostra le statistiche finali della simulazione."
  ],
  
     Spacer[10],

(* Pulsante per visualizzare la soluzione calcolata automaticamente *)
Tooltip[
 Button["Visualizza Soluzione",
  Module[{
    logSimulazione = {}, (* Lista per salvare il log della simulazione *)
    turnoCorrente, azioneSuggerita = "Nessuna", (* Variabili per il turno e l'azione suggerita *)
    spiegazione = "Inizializzazione", (* Stringa per spiegare la logica dell'azione suggerita *)
    turniSimulati = turniIniziali, (* Numero di turni disponibili all'inizio della simulazione *)
    risorse = risorseIniziali, (* Risorse iniziali del giocatore *)
    rendimentoCostruzioneSol = 0, (* Guadagno cumulativo derivante dalla costruzione *)
    guadagnoEsplorazionePonderato, guadagnoRaccolta, beneficioTotale = 0, (* Variabili per i calcoli *)
    costoRaccoltaEffettivo, beneficioRaccoltaEffettivo, (* Costo e beneficio della raccolta *)
    costoCostruzioneEffettivo, incrementoRendimentoEffettivo, (* Costo e incremento per la costruzione *)
    costoEsplorazioneEffettivo, beneficioEsplorazioneEffettivo, (* Costo e beneficio per l'esplorazione *)
    risorseMassime = risorseIniziali (* Risorse massime raggiunte durante la simulazione *)
  },

   (* Calcola i parametri dinamicamente in base alla difficolt\[AGrave] *)
   costoCostruzioneEffettivo = Switch[difficolta, 
     "Facile", 35, 
     "Medio", 40, 
     "Difficile", 50
   ];
   incrementoRendimentoEffettivo = Switch[difficolta, 
     "Facile", 8, 
     "Medio", 10, 
     "Difficile", 12
   ];
   costoEsplorazioneEffettivo = Switch[difficolta, 
     "Facile", 15, 
     "Medio", 20, 
     "Difficile", 25
   ];
   beneficioEsplorazioneEffettivo = Switch[difficolta, 
     "Facile", {20, 70}, 
     "Medio", {10, 50}, 
     "Difficile", {RandomInteger[{-30, 0}], RandomInteger[{0, 80}]}
   ];
   costoRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 10, 
     "Medio", 15, 
     "Difficile", 20
   ];
   beneficioRaccoltaEffettivo = Switch[difficolta, 
     "Facile", 30, 
     "Medio", 25, 
     "Difficile", RandomInteger[{15, 30}]
   ];

   (* Simula il comportamento del giocatore per determinare la soluzione ottimale *)
   While[turniSimulati > 0 && risorse > 0,

    (* Calcolo dei benefici per le azioni disponibili *)
    guadagnoEsplorazionePonderato = 
     0.7 * (Max[beneficioEsplorazioneEffettivo] - costoEsplorazioneEffettivo) + 
      0.3 * (Min[beneficioEsplorazioneEffettivo] - costoEsplorazioneEffettivo);
    guadagnoRaccolta = beneficioRaccoltaEffettivo - costoRaccoltaEffettivo;

    rendimentoCostruzioneSol = 
     If[turniSimulati > Ceiling[costoCostruzioneEffettivo / incrementoRendimentoEffettivo], 
      incrementoRendimentoEffettivo * (turniSimulati - Ceiling[costoCostruzioneEffettivo / incrementoRendimentoEffettivo]),
      0
     ];

    (* Logica per decidere l'azione migliore *)
    Which[
     (* Caso 1: Costruire *)
     rendimentoCostruzioneSol > Max[guadagnoEsplorazionePonderato, guadagnoRaccolta] &&
       turniSimulati > Ceiling[costoCostruzioneEffettivo / incrementoRendimentoEffettivo] &&
       risorse >= costoCostruzioneEffettivo &&
       risorse - costoCostruzioneEffettivo >= Min[costoRaccoltaEffettivo, costoEsplorazioneEffettivo],
     (
      azioneSuggerita = "Costruisci Struttura";
      spiegazione = "Costruire conviene: rendimento stimato " <> ToString[rendimentoCostruzioneSol];
      risorse -= costoCostruzioneEffettivo;
      rendimentoCostruzioneSol += incrementoRendimentoEffettivo;
     ),

     (* Caso 2: Esplorare *)
     guadagnoEsplorazionePonderato > guadagnoRaccolta &&
       risorse >= costoEsplorazioneEffettivo &&
       risorse - costoEsplorazioneEffettivo >= costoRaccoltaEffettivo,
     (
      azioneSuggerita = "Esplora";
      spiegazione = "Esplorare conviene: guadagno medio ponderato di " <> ToString[guadagnoEsplorazionePonderato];
      risorse -= costoEsplorazioneEffettivo;
      risorse += RandomInteger[beneficioEsplorazioneEffettivo];
     ),

     (* Caso 3: Raccogliere risorse *)
     risorse >= costoRaccoltaEffettivo,
     (
      azioneSuggerita = "Raccogli Risorse";
      spiegazione = "Raccogliere \[EGrave] la scelta sicura: guadagno garantito di " <> ToString[guadagnoRaccolta];
      risorse -= costoRaccoltaEffettivo;
      risorse += beneficioRaccoltaEffettivo;
     ),

     (* Caso 4: Nessuna azione possibile *)
     True,
     (
      azioneSuggerita = "Raccogli Risorse";
      spiegazione = "Non puoi fare altro: raccogli risorse per continuare.";
      risorse -= costoRaccoltaEffettivo;
      risorse += beneficioRaccoltaEffettivo;
     )
    ];

    (* Aggiorna le risorse massime raggiunte *)
    If[risorse > risorseMassime, risorseMassime = risorse];

    (* Aggiorna il log della simulazione *)
    AppendTo[logSimulazione, 
     "Turno " <> ToString[turniIniziali - turniSimulati + 1] <> ": " <> azioneSuggerita <>
      " - " <> spiegazione <> " | Risorse rimanenti: " <> ToString[risorse]
    ];

    (* Riduci i turni *)
    turniSimulati--;
   ];

   (* Aggiungi il totale delle risorse rimanenti e massime raggiunte al log *)
   AppendTo[logSimulazione, 
    "\nTotale risorse rimanenti: " <> ToString[risorse] <>
    "\nRisorse massime raggiunte: " <> ToString[risorseMassime]
   ];

   (* Mostra i risultati della simulazione *)
   CreateDialog[{
     TextCell["Simulazione completata:", FontWeight -> Bold],
     Pane[StringJoin[Riffle[logSimulazione, "\n"]], {500, 400}, Scrollbars -> True]
   }, WindowTitle -> "Log Simulazione"];
  ]
 ], "Simula una partita automatica basata sulla difficolt\[AGrave] e mostra il log della simulazione."]
	}],
  
 Spacer[10],
 
(* 
   Sezione di codice per gestire la visualizzazione del Debug Log (commentato).
   - Button["Mostra Debug Log"]: pulsante che apre una finestra di dialogo per visualizzare il log di debug dinamico.
   - TextCell["Debug Log:"]: titolo della finestra.
   - Pane[Dynamic[StringJoin[Riffle[debugLog, "\n"]]]]: area di visualizzazione per il contenuto del log.
   - WindowTitle -> "Debug Log": imposta il titolo della finestra di dialogo.
   - Spacer[10]: aggiunge uno spazio verticale per separare gli elementi.
*)
     
(* Pulsante per ripristinare l'interfaccia del simulatore *)
Tooltip[
 Button["Reset Esercizio", 
  (
   (* Reimposta i valori iniziali delle variabili principali *)
   risorseCorrenti = 100; (* Quantit\[AGrave] iniziale di risorse *)
   turniRestanti = 10; (* Numero iniziale di turni *)
   simulazioneIniziata = False; (* Flag che indica che la simulazione non \[EGrave] in corso *)
   log = ""; (* Log degli eventi vuoto *)
   azioniRaccolta = 0; (* Resetta il contatore delle azioni di raccolta *)
   azioniCostruzione = 0; (* Resetta il contatore delle azioni di costruzione *)
   azioniEsplorazione = 0; (* Resetta il contatore delle azioni di esplorazione *)
   turniIniziali = 10; (* Ripristina il numero di turni iniziali *)
   risorseIniziali = 100; (* Ripristina la quantit\[AGrave] iniziale di risorse *)
   difficolta = "Medio"; (* Imposta la difficolt\[AGrave] predefinita a "Medio" *)
   seedNumerico = 42; (* Ripristina il valore predefinito del seed *)
  ), 
  Enabled -> True (* Il pulsante \[EGrave] sempre abilitato *)
 ], 
 "Reimposta l'interfaccia per iniziare un nuovo esercizio." (* Tooltip che spiega il pulsante *)
],

Spacer[10], (* Spazio verticale per separare gli elementi *)
     
(* Area per visualizzare il log degli eventi *)
Style["Log degli Eventi:", Bold], (* Titolo della sezione del log *)
Pane[
 Dynamic[log], (* Contenuto dinamico del log *)
 {800, 200}, (* Dimensioni dell'area di visualizzazione: larghezza 800, altezza 200 *)
 Scrollbars -> True (* Aggiunge barre di scorrimento se il contenuto supera le dimensioni dell'area *)
]
}]
];

End[];
EndPackage[];
