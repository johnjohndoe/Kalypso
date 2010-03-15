#ifndef _MO2DEFS_H_
#define _MO2DEFS_H_

// Definitionen für Feldnamen der Layer, Tags und Symbole der TrackingLayer

// Feldnamen für Layer-Daten Max-Länge: 10 Zeichen, keine Leerzeichen
#define MO2_FIELD_SHAPE             TEXT("Shape")
#define MO2_FIELD_STATION           TEXT("Station")
#define MO2_FIELD_PROFILID          TEXT("ProfilID")
#define MO2_FIELD_FILE              TEXT("Dateiname")
#define MO2_FIELD_STATE             TEXT("Zustand")
#define MO2_FIELD_HEIGHT            TEXT("Hoehe")
#define MO2_FIELD_NEXTID            TEXT("NextPtID")
#define MO2_FIELD_FEATUREID         TEXT("FeatureId")
#define MO2_FIELD_YKRD              TEXT("yKoord")
#define MO2_FIELD_TEXT              TEXT("Text")
#define MO2_FIELD_HEADHEIGHT        TEXT("Kopfhoehe")
#define MO2_FIELD_FRONTSLOPE        TEXT("Neigung-v")
#define MO2_FIELD_BACKSLOPE         TEXT("Neigung-r")
#define MO2_FIELD_RELI              TEXT("Ausrichtng") 
#define MO2_FIELD_DELTAY            TEXT("y-Bezugs")
#define MO2_FIELD_VARIANT           TEXT("Variante")
#define MO2_FIELD_TRENNSPEZIAL      TEXT("Lage")
#define MO2_FIELD_MFB               TEXT("MFBNr")
#define MO2_FIELD_VZK               TEXT("VZK")
#define MO2_FIELD_PK                TEXT("PK")

#define MO2_FIELD_NAME              TEXT("Name")
#define MO2_FIELD_NUMBER            TEXT("Nummer")

#define MO2_FIELD_RAUHEIT           TEXT("Rauht-ks")
#define MO2_FIELD_RAUHEITKST        TEXT("Rauht-kSt")
#define MO2_FIELD_AXM               TEXT("AX")
#define MO2_FIELD_AYM               TEXT("AY")
#define MO2_FIELD_DPM               TEXT("DP")


// Nummern der Symbole des trackingLayer
#define MO2_TRACKSYMBOL_COUNT           7
#define MO2_TRACKSYMBOL_RAND            0
#define MO2_TRACKSYMBOL_MOVEPOINT       1
#define MO2_TRACKSYMBOL_MOVEOBJECT      2
#define MO2_TRACKSYMBOL_ACTIVEPROFILE   3
#define MO2_TRACKSYMBOL_GENERATEPROFILE 4
#define MO2_TRACKSYMBOL_RIVER           5
#define MO2_TRACKSYMBOL_MARK_OBJECT     6

// Tags der GeoEvente
#define MO2_TRACKTAG_RAND               "WATERLEVEL"
#define MO2_TRACKTAG_MOVEPOINT          "MOVEPOINT"
#define MO2_TRACKTAG_MOVEOBJECT         "MOVEOBJECT"
#define MO2_TRACKTAG_ACTIVEPROFILE      "ACTIVEPROFILE"
#define MO2_TRACKTAG_GENERATEPROFILE    "GENPROFILE"
#define MO2_TRACKTAG_RIVER              "RIVER"

// Flags für spezielle Anwendungen
#define MO2_TRACKFLAG_GERADE  moDashDotLine
#define MO2_TRACKFLAG_KRUMM   moDashLine

// sonstige zusätzliche Definitionen
#define MO2_EXTRA_OBJECT  "*EXTRA*OBJECT*" // Attributwert für die funktion CMapLayer::CutwithLine ( hoffentlich gibts diesen Text nie als Attribut )

#endif _MO2DEFS_H_