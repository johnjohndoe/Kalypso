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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wavos;

/**
 * 
 * @author thuel2
 */
public class WavosConst
{

  /**
   *  
   */
  public static final String CALCJOB_SPEC = "wavosCalcJob_spec.xml";

  public static final String LOG_FILE = "elbeWavosSachsen.log";
  public static final String LOG_DIR = "Log-Dateien";
  public static final String RESULT_DIR = "Ergebnisse";

  // besser als info vom client holen? oder andersrum, das encoding weitergeben an den client?
  public static final String WAVOS_CODEPAGE = "UTF-8";//"Cp1252";

  public static final String CALC_START = "Modell Berechnung wird gestartet";
  public static final String CALC_FILE_CREATION = "Dateien für Rechenkern werden erzeugt";
  public static final String CALC_RESULT_READ = "Ergebnisse werden zurückgelesen";
  public static final String CALC_CANCELLED = "Berechnung wurde durch den Benutzer abgebrochen.";
  public static final String CALC_FINISHED = "Berechnung ist beendet.";
  public static final String CALC_CALL = "Rechenkern wird aufgerufen";

  public static final String RECHENKERN_DATA_ZIP = "elbe/lhwzsachsen/resources/wavosElbeSachsenData.zip";
  public static final String RECHENKERN_BIN_ZIP = "elbe/lhwzsachsen/resources/wavosElbeSachsenBin.zip";

  public static final String DATA_STARTFORECAST_DATE = "startforecast_date";
  public static final String DATA_GML = "data_gml";
  public static final String DATA_GML_CONTEXT = "data_gml_context";

  public static final String DATA_FORECAST_DURATION = "forecast_duration";
  public static final String DATA_MERGE_CASE_PATH = "mergeCasePath";
  public static final String DATA_SIMULATION_DURATION = "simulation_duration";
  public static final String DATA_SIMULATION_START = "simulation_start";
  public static final String DATA_INTERVAL_AMOUNT = "interval_amount";
  public static final String DATA_USE_AWERTE = "use_awerte";

  public static final String DATA_COUNT_MEASURED_VALUES = "count_meas_vals";
  public static final String DATA_COUNT_FORECAST_VALUES = "count_forecast_vals";

  public static final int ZIP_TYPE_DATA = 1;
  public static final int ZIP_TYPE_BIN = 2;

  public static final String WAVOS_COUNT_WQ_PAIRS_FORMAT = "%5d Wertepaare (W,Q)";
  public static final String WAVOS_COUNT_WQ_FORMAT = "%5.0f %6.1f";

  // TODO andere Lösung, wenn andere Flussgebiete kommen...
  public static final String FLUSS = "elbe";
  public static final String FLUSS_INPUT = "Elbe";

  public static final String FILE_START_BAT = "start.bat";
  public static final String FILE_WAVOS_LOG = "output.log";
  public static final String FILE_WAVOS_PAR = "input.par";
  public static final String FILE_SHIFTVOR_PAR = "shiftvor.par";
  public static final String FILE_SHIFTVOR_LOG = "shiftvor.log";
  public static final String FILE_AWERTE_ZIP = "awerte.zip";

  public static final String DIR_ANFANGSWERTE = "Anfangswerte";
  public static final String DIR_ZEITREIHEN = "Zeitreihen";
  public static final String DIR_ZEITREIHEN_OHNE_SHIFTVOR = "Zeitreihen_ohneVerschiebung";
  public static final String DIR_AWERTE = "awerte";
  public static final String DIR_WAVOS = "wavos";
  public static final String DIR_BIN = "bin";
  public static final String DIR_VORHER = "vorher";
  public static final String DIR_VORHER_SAVE = "vorher_save";
  public static final String DIR_INPUT = "input";
  public static final String DIR_TAFEL = "tafel";

  public static final String ERROR_MINUS_4 = "Die WAVOS-Rechnung konnte nicht durchgeführt werden. Bitte überprüfen Sie die Messdaten und wählen Sie ggf. einen anderen Startzeitpunkt der Berechnung.";
  public static final String ERROR_MINUS = "Die WAVOS-Rechnung konnte nicht durchgeführt werden.";
  public static final String ERROR_PLUS = "Die WAVOS-Rechnung konnte nicht durchgeführt werden (unbekannter Fehlerstatus).";
  public static final String ERROR_SHIFTVOR = "Shiftvor konnte nicht erfolgreich durchgeführt werden.";

  public WavosConst()
  {
  // will not be instantiated
  }

}
