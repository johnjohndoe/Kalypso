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
package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Date;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class TubigInputWorker
{
  /**
   * Holt aus den Resourcen (PATH_RECHENKERN_ZIP) und entpackt sie nach fleDir <br>
   * PATH_RECHENKERN_ZIP enthält alle benötigten Batch-Dateien, Steuer-Dateien, allg. Parameter-Dateien etc.
   * 
   * @throws TubigException
   * 
   * @author Thül
   */
  public static void copyAndUnzipRechenkern( final File fleDir ) throws TubigException
  {
    final URL urlRk;
    InputStream zipStream = null;

    // rechenkern.zip aus den resourcen holen
    urlRk = TubigConverter.class.getResource( "resources/" + TubigConst.PATH_RECHENKERN_ZIP );

    try
    {
      // rechenkern.zip in fleDir entpacken
      zipStream = urlRk.openStream();
      ZipUtilities.unzip( zipStream, fleDir );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new TubigException( "Fehler beim Entpacken des Rechenkerns", e );
    }
    finally
    {
      IOUtils.closeQuietly( zipStream );
    }
  }

  /**
   * Bereitstellen und ggf. Umwandeln aller Eingabedaten für die Berechnung <br>
   * 
   * Rechenkern.zip aus den Resourcen holen und entpacken <br>
   * Datei AKTDT.TXT (aktuelle Modellzeit) aus .calculation (Id = CALC) erzeugen <br>
   * Eingabedateien (Zeitreihen) konvertieren <br>
   * Parameterdateien schreiben (Speicher, WLM_ <pegel>.par <br>
   * alle Dateien werden ins Rechenverzeichnis ("BODEVOR") geschrieben <br>
   * 
   * 
   * @param dirCalc
   *          Verzeichnis, in dem gerechnet wird (BODEVOR)
   * @throws TubigException
   * @throws CalcJobServiceException
   * @throws CalcJobServiceException
   * @author Thül
   */
  public static TubigCalculationData createCalcInput( final File dirCalc, final ICalcDataProvider inputData,
      final Map metaMap ) throws CalcJobServiceException, TubigException
  {
    final TubigCalculationData calcData;

    // Rechenkern.zip aus den Resourcen ins Bodevor-Verzeichnis entpacken
    copyAndUnzipRechenkern( dirCalc );

    // .calculation (Control-File) holen und parsen
    calcData = new TubigCalculationData( inputData.getURLForID( "CONTROL_GML" ) );

    // AKTDT.TXT erzeugen (startforecast = IST_Zeit)
    TubigConverter.createAktDtTxt( dirCalc, calcData.getStartforecast() );

    // Parameter aus modell.gml in Dateien schreiben (Speicher, Pegel)
    // und auch Zeitreihen in TUBIG-Dateien konvertieren
    writeParameterAndTimeseries( inputData.getURLForID( "MODELL_GML" ), dirCalc, calcData.getStartforecast(), metaMap );

    return calcData;
  }

  /**
   * 
   * Liest Parameter (Speicher, Pegel) aus modell.gml und schreibt die Parameterdateien ins Rechenverzeichnis (BODEVOR)
   * 
   * @param dtStartForecast
   * @throws TubigException
   * 
   * @author Thül
   */

  private static void writeParameterAndTimeseries( final URL urlGml, final File dirCalc, final Date dtStartForecast,
      final Map metaMap ) throws TubigException
  {
    final GMLWorkspace gmlWrkSpce;
    try
    {
      gmlWrkSpce = GmlSerializer.createGMLWorkspace( urlGml );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
      throw new TubigException( "Fehler beim Laden der Modelldaten-GML", e1 );
    }

    TubigExportParameter.writeSpeicherPars( gmlWrkSpce, dirCalc );
    TubigExportParameter.writePegelPars( gmlWrkSpce, dirCalc );

    // Speicherabgabe, Vergangenheit
    TubigFeatureVisitorZml2Tubig.writeTimeseries( gmlWrkSpce, TubigConst.GML_SPEICHER_COLL, urlGml, dirCalc,
        TubigConst.GML_KURZ_NAME, "Abgabe", -1, "vsa", dtStartForecast, metaMap, TubigConst.SPEICHER );

    // Speicherabgabe, Prognose (auch Überleitung)
    TubigFeatureVisitorZml2Tubig.writeTimeseries( gmlWrkSpce, TubigConst.GML_ALLE_SPEICHER_COLL, urlGml, dirCalc,
        TubigConst.GML_KURZ_NAME, "Abgabe", 1, "psa", dtStartForecast, metaMap, TubigConst.SPEICHER );

    // Speicherinhalt, Vergangenheit
    TubigFeatureVisitorZml2Tubig.writeTimeseries( gmlWrkSpce, TubigConst.GML_SPEICHER_COLL, urlGml, dirCalc,
        TubigConst.GML_KURZ_NAME, "Ganglinie_gemessen", -1, "vvs", dtStartForecast, metaMap, TubigConst.SPEICHER );

    // Abfluss am Pegel, Vergangenheit
    TubigFeatureVisitorZml2Tubig.writeTimeseries( gmlWrkSpce, TubigConst.GML_PEGEL_COLL, urlGml, dirCalc,
        TubigConst.GML_KURZ_NAME, "Ganglinie_gemessen", -1, "vq", dtStartForecast, metaMap, TubigConst.PEGEL );

    // Abfluss am Pegel, Prognose (wird für Batch 3-5 elen.pq und/oder wege.pq
    // benötigt)
    TubigFeatureVisitorZml2Tubig.writeTimeseries( gmlWrkSpce, TubigConst.GML_PEGEL_COLL, urlGml, dirCalc,
        TubigConst.GML_KURZ_NAME, "Ganglinie_gerechnet", 1, "pq", dtStartForecast, null, TubigConst.PEGEL );

    // Gebietsniederschlag, Vergangenheit
    TubigFeatureVisitorZml2Tubig.writeTimeseries( gmlWrkSpce, TubigConst.GML_NSGEB_COLL, urlGml, dirCalc,
        TubigConst.GML_KURZ_NAME, "Niederschlag", -1, "vns", dtStartForecast, metaMap, TubigConst.PEGEL );

    // Gebietsniederschlag, Prognose
    TubigFeatureVisitorZml2Tubig.writeTimeseries( gmlWrkSpce, TubigConst.GML_NSGEB_COLL, urlGml, dirCalc,
        TubigConst.GML_KURZ_NAME, "Niederschlag", 1, "pns", dtStartForecast, metaMap, TubigConst.PEGEL );
  }

  public static void main( final String[] args ) throws TubigException
  {
    copyAndUnzipRechenkern( new File( System.getProperty( "java.io.tmpdir" ) + "/rkTest" ) );
  }

  public TubigInputWorker()
  {
  // wird nicht instantiiert
  }
}