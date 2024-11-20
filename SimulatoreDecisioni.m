(* ::Package:: *)

(* ::Package:: *)
(*SimulatoreDecisioni`*)


BeginPackage["SimulatoreDecisioni`"];

(* Dichiarazione delle funzioni pubbliche *)
IniziaSimulazione::usage = "IniziaSimulazione[] avvia una nuova simulazione con risorse iniziali e numero di turni predefiniti.";
RaccogliRisorse::usage = "RaccogliRisorse[] consuma risorse per guadagnarne altre con una probabilit\[AGrave] di successo definita.";
CostruisciStruttura::usage = "CostruisciStruttura[] investe risorse per ottenere benefici a lungo termine.";
Esplora::usage = "Esplora[] consuma risorse per ottenere guadagni variabili con rischio.";
MostraRisultati::usage = "MostraRisultati[] fornisce un riepilogo delle decisioni prese e del punteggio finale.";
MostraSoluzione::usage = "MostraSoluzione[] confronta le scelte dell'utente con una strategia ottimale.";

(* Inizializzazione delle variabili globali *)
risorseIniziali::usage = "Variabile globale che contiene le risorse disponibili all'inizio della simulazione.";
risorseCorrenti::usage = "Variabile globale che tiene traccia delle risorse disponibili durante la simulazione.";
turniRestanti::usage = "Variabile globale che indica i turni rimanenti nella simulazione.";

Begin["`Private`"];

(* Funzioni di base *)

(* 1. IniziaSimulazione *)
IniziaSimulazione[] := (
    risorseIniziali = 100;
    risorseCorrenti = risorseIniziali;
    turniRestanti = 10;
    Print["Simulazione iniziata con ", risorseCorrenti, " risorse e ", turniRestanti, " turni rimanenti."];
)

(* 2. RaccogliRisorse *)
RaccogliRisorse[] := Module[{costo = 10, guadagno = 15, probabilitaSuccesso = 0.9},
    If[turniRestanti > 0 && risorseCorrenti >= costo,
        turniRestanti--;
        risorseCorrenti -= costo;
        If[RandomReal[] < probabilitaSuccesso,
            risorseCorrenti += guadagno;
            Print["Operazione riuscita! Guadagnate ", guadagno, " risorse."],
            Print["Operazione fallita. Nessuna risorsa guadagnata."]
        ];
    ,
        Print["Non ci sono abbastanza risorse o turni disponibili per eseguire questa azione."];
    ];
    MostraRisultati[];
]

(* 3. CostruisciStruttura *)
CostruisciStruttura[] := Module[{costo = 30, beneficioPerTurno = 10, durata = 3},
    If[turniRestanti > 0 && risorseCorrenti >= costo,
        turniRestanti--;
        risorseCorrenti -= costo;
        Print["Struttura costruita! Guadagnerai ", beneficioPerTurno, " risorse per i prossimi ", durata, " turni."];
    ,
        Print["Non ci sono abbastanza risorse o turni disponibili per eseguire questa azione."];
    ];
    MostraRisultati[];
]

(* 4. Esplora *)
Esplora[] := Module[{costo = 20, guadagnoVariabile},
    If[turniRestanti > 0 && risorseCorrenti >= costo,
        turniRestanti--;
        risorseCorrenti -= costo;
        guadagnoVariabile = RandomInteger[{0, 40}];
        risorseCorrenti += guadagnoVariabile;
        Print["Hai esplorato e ottenuto ", guadagnoVariabile, " risorse."];
    ,
        Print["Non ci sono abbastanza risorse o turni disponibili per eseguire questa azione."];
    ];
    MostraRisultati[];
]

(* 5. MostraRisultati *)
MostraRisultati[] := (
    Print["Risorse attuali: ", risorseCorrenti];
    Print["Turni rimanenti: ", turniRestanti];
)

(* 6. MostraSoluzione *)
MostraSoluzione[] := (
    Print["Simulazione completata! Ecco una strategia ottimale che avresti potuto seguire:"];
    Print["1. RaccogliRisorse per aumentare rapidamente le risorse iniziali."];
    Print["2. CostruisciStruttura per ottenere benefici a lungo termine."];
    Print["3. Esplora con cautela per massimizzare i guadagni senza rischiare troppo."];
)

End[];
EndPackage[];




