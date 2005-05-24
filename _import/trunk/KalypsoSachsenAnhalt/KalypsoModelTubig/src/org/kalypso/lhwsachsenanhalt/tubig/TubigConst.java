package org.kalypso.lhwsachsenanhalt.tubig;

import java.text.SimpleDateFormat;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

public class TubigConst
{
  /**
   * allgemeine Konstanten für Bode (und Ilse)
   * 
   * @author Thül
   */

  // TUBIG-Datenformate
  public static final String TUBIG_NUMBER_FORMAT = "%10.5f";

  public static final String TUBIG_CODEPAGE = "Cp1252";

  public static SimpleDateFormat TUBIG_DATE_FORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );

  public static final String TUBIG_INTEGER_FORMAT = "%i";

  public static final String TUBIG_STRING_FORMAT = "%s";

  public static final String TUBIG_SEP = " ";

  public static final String ZML_CODEPAGE = "UTF-8";

  // Metadaten-Property, die Kommentar-Zeile der TUBIG-Datei "aufbewahrt"
  public static final String PROP_COMMENT = "tubig_kommentar";

  public static final String PROP_NAME = "Name";

  public static final String PROP_STARTZEIT = "StartZeit";

  public static final String PROP_VORHERSAGE = "Vorhersage";

  // Name der Datei, in der das aktuelle Modelldatum steht
  public static final String AKTDT_FILE_NAME = "AKTDT.TXT";

  // erste Zeile in der Datei AKTDT_FILE_NAME
  public static final String AKTDT_FILE_COMMENT = "Aktuelle Modellzeit:";

  // Messages und Kommentare (CalcService, TubigBatchInterpreter)
  public static final String MESS_BERECHNUNG_WIRD_GESTARTET = "Modell: Berechnung wird gestartet";

  public static final String MESS_BERECHNUNG_BEENDET = "Modell: Berechnung beendet";

  public static final String MESS_BERECHNUNG_ABGEBROCHEN = "Modell: Berechnung abgebrochen";

  public static final String MESS_DATEIEN_ERZEUGEN = "Modell: Dateien für Rechenkern werden erzeugt";

  public static final String MESS_RECHENKERN_AUFRUFEN = "Modell: Rechenkern wird aufgerufen";

  public static final String MESS_ERGEBNISSE_ZURUECK = "Modell: Ergebnisse werden zurückgeschrieben";

  public static String LOG_COMMENT_UEBERLESEN = "Überlesen: ";

  // Dateinamen (CalcService, TubigBatchInterpreter)
  public static final String NAME_BAT = "modell_batch";

  public static final String NAME_EXT_LOG = ".log";

  public static final String NAME_EXT_ERR = ".err";

  public static final String NAME_EXT_ZML = ".zml";

  public static final String NAME_CALC_LOG = "modell.log";

  public static final String PATH_RECHENKERN_ZIP = "rechenkern/rechenkern.zip";

  public static final String PRE_COPY_OUT_BATCH = "bceCpyErg_";

  public static final String PRE_COPY_IN_BATCH = "bceCpyIn_";

  // Verzeichnisbezeichnungen
  public static final String LOGS = "Logs";

  public static final String ERGEBNISSE = "Ergebnisse";

  public static final String SPEICHER = "Speicher";

  public static final String PEGEL = "Pegel";

  // GML-Strings, die häufig verwendet werden und von der Modellierung abhängen
  public static final String GML_KURZ_NAME = "Kurz_Name";

  public static final String GML_PEGEL_COLL = "PegelCollectionAssociation/PegelMember[Pegel]";

  public static final String GML_NSGEB_COLL = "PegelCollectionAssociation/PegelMember[Niederschlagsgebiet]";

  public static final String GML_WLM_COLL = "PegelCollectionAssociation/PegelMember[WasserlaufModell]";

  public static final String GML_SPEICHER_COLL = "SpeicherCollectionAssociation/SpeicherMember[Speicher]";

  public static final String GML_UEBERL_COLL = "SpeicherCollectionAssociation/SpeicherMember[Überleitung]";

  public static final String GML_ALLE_SPEICHER_COLL = "SpeicherCollectionAssociation/SpeicherMember";

  // werden bei den Default-Metadaten für ZMLs verwendert
  public static final String DEFAULT = "default";

  public static final String UNBEKANNT = "unbekannt";

  public static final String STDOUT = "StdOut";

  public static final String STDERR = "StdErr";

  public static final String ENDE = "**ende**";

  // Finish-Text
  public static final String FINISH_ERROR = "Bei der Berechnung trat ein Fehler auf, weitere Informationen dazu finden sie in den Log-Dateien.";

  // ist betriebssystemabhängig...(z. B. für Windows 95/98
  // "command.com / c ") und wir für das Starten von BODESTEU.EXE
  // benötigt, das ein 16-Bit-Programm ist.
  public static final String START_IN_CMD = "cmd /c ";

  //
  public static final String CALCJOB_SPEC = "tubigcalcjob_spec.xml";

  public TubigConst()
  {
  // wird nicht instantiiert
  }

}