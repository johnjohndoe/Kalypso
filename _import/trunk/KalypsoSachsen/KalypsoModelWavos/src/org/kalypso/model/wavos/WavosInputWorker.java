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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.time.DateUtils;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.model.wavos.visitors.FeatureVisitorZml2WavosAT;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author thuel2
 */
public class WavosInputWorker
{
  private final static Logger m_logger = Logger.getLogger( WavosInputWorker.class.getName() );

  private final static String m_propZmlLink = "ganglinie_gesamt";

  private final static String m_propZmlLinkPolder = "ganglinie_zustand";

  private final static String m_kenn = "kenn";

  private final static String m_lfdNum = "lfdNum";

  private final static String m_projektionAllowed = "projektionErlaubt";

  private final static String m_projektionType = "projektionEingschaltet";

  private final static String m_axisToBeWritten = "datenUebergabe";

  private final static String m_zuflussVorhersage = "zuflussVorhersage";

  private final static UrlUtilities m_urlresolver = new UrlUtilities();

  public static File createNativeInput( final File tmpDir, final ISimulationDataProvider inputProvider, Properties props, final File nativeInDir, final String flussName, final Map metaMap ) throws Exception
  {
    try
    {
      final File exeDir = new File( tmpDir, "exe" );
      exeDir.mkdirs();

      // unzip (Data) Rechenkern, bevor AT-Dateien ggf. über die Dateien
      // in Verzeichnis tafel geschrieben werden
      m_logger.info( "\tKopiere Rechenkern (Daten)" );
      copyAndUnzipRechenkern( exeDir, WavosConst.ZIP_TYPE_DATA );
      // Binaries müssten nicht immer mit an den Benutzer gegeben werden
      m_logger.info( "\tKopiere Rechenkern (Binaries)" );
      copyAndUnzipRechenkern( exeDir, WavosConst.ZIP_TYPE_BIN );

      // Steuerparams lesen
      final URL controlGmlURL = (URL) inputProvider.getInputForID( "CONTROL_GML" );

      m_logger.info( "\tLese Steuerparameter: " + controlGmlURL.toString() );
      final HashMap map = parseControlFile( controlGmlURL );
      final URL urlForGmlId = (URL) inputProvider.getInputForID( "GML" );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( urlForGmlId, null );
      map.put( WavosConst.DATA_GML, workspace );
      map.put( WavosConst.DATA_GML_CONTEXT, urlForGmlId );

      props.putAll( map );

      final File wavosDir = new File( exeDir, WavosConst.DIR_WAVOS );
      // .../input.par
      // .../shiftvor.par
      m_logger.info( "\tSchreibe " + WavosConst.FILE_WAVOS_PAR + " und " + WavosConst.FILE_SHIFTVOR_PAR );
      writeParFiles( wavosDir, props );

      // .../start.bat
      m_logger.info( "\tSchreibe " + WavosConst.FILE_START_BAT );
      writeBatchFile( wavosDir, flussName );

      final File flussDir = new File( wavosDir, flussName );
      // .../elbe/bin/elbe.pro (aus calcCase.gml)
      m_logger.info( "\tSchreibe " + flussName + ".pro" );
      final File flussBinDir = new File( flussDir, WavosConst.DIR_BIN );
      flussBinDir.mkdirs();
      writeProFile( workspace, flussBinDir, flussName );

      // .../elbe/bin/bauwerke.offen (aus Polder-ZMLs)
      m_logger.info( "\tSchreibe bauwerke.offen" );
      writeBauwerkeOffen( workspace, flussBinDir, props );

      // .../elbe/awerte
      m_logger.info( "\tKopiere Anfangswerte" );
      final File awerteTargetDir = new File( flussDir, WavosConst.DIR_AWERTE );
      awerteTargetDir.mkdirs();
      final URL awerteURL = (URL) inputProvider.getInputForID( "AWERTE" );

      final String awerteURLstr = awerteURL.toExternalForm();
      final URL awerteURLDir = new URL( awerteURLstr + "/" );

      final URL awerteZip = new URL( awerteURLDir, WavosConst.FILE_AWERTE_ZIP );
      InputStream openStream = awerteZip.openStream();
      ZipUtilities.unzip( openStream, awerteTargetDir );
      IOUtils.closeQuietly( openStream );

      // Zeitreihen & AT-Dateien
      // .../elbe/input/
      // .../elbe/tafel/
      m_logger.info( "\tErzeuge Zeitreihen und AT-Dateien" );
      writeTimeseriesAndAtFiles( props, flussDir, metaMap );

      // save input data to native dir before it is changed by the kernel
      // (awerte)
      if( exeDir.exists() && nativeInDir.exists() )
      {
        final FileCopyVisitor copyVisitor = new FileCopyVisitor( exeDir, nativeInDir, true );
        FileUtilities.accept( exeDir, copyVisitor, true );
      }
      return wavosDir;
    }
    catch( final SimulationException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Erzeugen der Eingabedateien", e );
    }
  }

  /**
   * @param exeDir
   * @param props
   */
  private static void writeParFiles( final File exeDir, final Properties props ) throws Exception
  {
    // Schreibe input.par und shiftvor.par
    final Date forecastDate = (Date) props.get( WavosConst.DATA_STARTFORECAST_DATE );
    final boolean useAWerte = ((Boolean) props.get( WavosConst.DATA_USE_AWERTE )).booleanValue();
    final File fleInputPar = new File( exeDir, WavosConst.FILE_WAVOS_PAR );
    final Writer wrtrInputPar = new OutputStreamWriter( new FileOutputStream( fleInputPar ), WavosConst.WAVOS_CODEPAGE );
    final PrintWriter pWrtrInputPar = new PrintWriter( wrtrInputPar );
    pWrtrInputPar.println( "1" ); // Vorhersagemodus
    pWrtrInputPar.println( WavosUtils.createWavosDateShort( forecastDate ) ); // Start
    // der
    // Vorhersage
    final String mergeCasePath = (String) props.get( WavosConst.DATA_MERGE_CASE_PATH );
    if( useAWerte && !"".equals( mergeCasePath ) )// Verwendung von
      // Anfangswerten
      pWrtrInputPar.println( "j" );
    else
      pWrtrInputPar.println( "n" );
    IOUtils.closeQuietly( pWrtrInputPar );
    IOUtils.closeQuietly( wrtrInputPar );

    final File fleShiftvorPar = new File( exeDir, WavosConst.FILE_SHIFTVOR_PAR );
    final Writer wrtrShiftvorPar = new OutputStreamWriter( new FileOutputStream( fleShiftvorPar ), WavosConst.WAVOS_CODEPAGE );
    final PrintWriter pWrtrShiftvorPar = new PrintWriter( wrtrShiftvorPar );
    pWrtrShiftvorPar.println( WavosUtils.createWavosDateShort( forecastDate ) ); // Start
    // der
    // Vorhersage
    // ?
    IOUtils.closeQuietly( pWrtrShiftvorPar );
    IOUtils.closeQuietly( wrtrShiftvorPar );
  }

  private static void writeBatchFile( final File exeDir, final String flussName ) throws Exception
  {
    // schreibe start.bat
    final File fleInputPar = new File( exeDir, WavosConst.FILE_START_BAT );
    final Writer wrtrInputPar = new OutputStreamWriter( new FileOutputStream( fleInputPar ), WavosConst.WAVOS_CODEPAGE );
    final PrintWriter pWrtrInputPar = new PrintWriter( wrtrInputPar );

    pWrtrInputPar.println( "REM dr_wavos starten" );
    // bin\dr_wavos_kalypso.exe elbe\bin\objekte_elbe.dat < input.par >
    // output.log
    pWrtrInputPar.println( "bin\\dr_wavos_kalypso.exe " + flussName + "\\bin\\objekte_" + flussName + ".dat < " + WavosConst.FILE_WAVOS_PAR + " > " + WavosConst.FILE_WAVOS_LOG );

    pWrtrInputPar.println();
    pWrtrInputPar.println( "REM Ergebnisse zwischenspeichern" );
    // xcopy "<SOURCE>" "<TARGET>" /e/s/c/h/y
    // xcopy "<SOURCE>" "<TARGET>" /c/y/i
    pWrtrInputPar.println( "xcopy " + "\"" + flussName + File.separator + WavosConst.DIR_VORHER + "\"" + " " + "\"" + flussName + File.separator + WavosConst.DIR_VORHER_SAVE + "\" /y/i" ); // /
                                                                                                                                                                                             // c

    pWrtrInputPar.println();
    pWrtrInputPar.println( "REM shiftvor starten" );
    // bin\shiftvor.exe elbe\bin\objekte_shiftvor.dat < shiftvor.par >
    // shiftvor.log
    pWrtrInputPar.println( "bin\\shiftvor.exe " + flussName + "\\bin\\objekte_shiftvor.dat < " + WavosConst.FILE_SHIFTVOR_PAR + " > " + WavosConst.FILE_SHIFTVOR_LOG );

    IOUtils.closeQuietly( pWrtrInputPar );
    IOUtils.closeQuietly( wrtrInputPar );
  }

  private static void writeTimeseriesAndAtFiles( final Properties props, final File flussDir, final Map metaMap ) throws Exception
  {
    // workspace holen
    final GMLWorkspace wks = (GMLWorkspace) props.get( WavosConst.DATA_GML );

    // input file
    final File inputDir = new File( flussDir, WavosConst.DIR_INPUT );
    inputDir.mkdirs();
    writeInput( wks, inputDir, props, metaMap );

    // AT-Dateien an allen Pegeln
    final File tafelDir = new File( flussDir, WavosConst.DIR_TAFEL );
    tafelDir.mkdirs();
    FeatureVisitorZml2WavosAT.writeAT( wks, tafelDir, (URL) props.get( WavosConst.DATA_GML_CONTEXT ) );

  }

  /**
   * @param wks
   * @param flussBinDir
   * @param props
   */
  private static void writeBauwerkeOffen( final GMLWorkspace wks, final File flussBinDir, final Properties props ) throws Exception
  {
    final File fleOffen = new File( flussBinDir, "bauwerke.offen" );
    final Writer wrtrOffen = new OutputStreamWriter( new FileOutputStream( fleOffen ), WavosConst.WAVOS_CODEPAGE );
    final PrintWriter pWrtrOffen = new PrintWriter( wrtrOffen );

    final FeatureList bauwerkeList = (FeatureList) wks.getFeatureFromPath( "bauwerkMember" );

    pWrtrOffen.println( "# Wann sind welche Bauwerke offen? Ein Bauwerk kann auch in mehreren Zeilen vorkommen" );
    pWrtrOffen.println( "#" );
    ITuppleModel tplValues = null;
    final URL url = (URL) props.get( WavosConst.DATA_GML_CONTEXT );
    final int intervalAmount = ((Integer) props.get( WavosConst.DATA_INTERVAL_AMOUNT )).intValue();
    for( final Iterator iter = bauwerkeList.iterator(); iter.hasNext(); )
    {
      final Feature bauwerk = (Feature) iter.next();
      // Kennung finden (m_kenn)
      final String kenn = (String) bauwerk.getProperty( m_kenn );
      // Zeitreihe holen (m_propZmlLinkPolder)
      final TimeseriesLinkType tsLink = (TimeseriesLinkType) bauwerk.getProperty( m_propZmlLinkPolder );
      final String href = tsLink.getHref();
      final URL zmlUrl = m_urlresolver.resolveURL( url, href );
      final URLConnection urlConTest = UrlUtilities.connectQuietly( zmlUrl );

      if( urlConTest != null )
      {
        final IObservation obsZml = ZmlFactory.parseXML( zmlUrl, kenn );
        tplValues = obsZml.getValues( null );
        final IAxis axDatum = ObservationUtilities.findAxisByType( tplValues.getAxisList(), TimeserieConstants.TYPE_DATE );
        final IAxis axWerte = ObservationUtilities.findAxisByType( tplValues.getAxisList(), TimeserieConstants.TYPE_POLDER_CONTROL );
        boolean zustand = false;
        // String zeile = Format.sprintf( "%-14s", new Object[]
        // { kenn + "_aus" } );
        // TODO: check if format ok
        String zeile = String.format( "%-14s", new Object[] { kenn + "_aus" } );
        String suffix;
        for( int ii = 0; ii < tplValues.getCount(); ii++ )
        {
          boolean polderOffen = ((Boolean) tplValues.getElement( ii, axWerte )).booleanValue();
          Date datum = (Date) tplValues.getElement( ii, axDatum );
          if( ii == 0 )
          {
            // wenn direkt am Anfang offen, dann eine Stunde vor
            // Anfang schon offen
            final Date newDate = WavosUtils.addHour( datum, -intervalAmount );
            zeile = zeile + String.format( " %s", WavosUtils.createWavosDate( newDate ) );
          }
          if( !(zustand == polderOffen) )
          {
            if( polderOffen )
            {
              // Polder-Öffnung
              suffix = "_ein";
            }
            else
            {
              // Polder-Schließung
              suffix = "_aus";
            }
            zeile = zeile + String.format( "%28s", WavosUtils.createWavosDate( datum ) );

            pWrtrOffen.println( zeile );
            final Date newDate = WavosUtils.addHour( datum, intervalAmount );
            // TODO: check format
            zeile = String.format( "%-14s%s", new Object[] { kenn + suffix, WavosUtils.createWavosDate( newDate ) } );
            zustand = polderOffen;
          }
        }
      }
    }
    pWrtrOffen.println( "# viel Spass" );

    IOUtils.closeQuietly( pWrtrOffen );
    IOUtils.closeQuietly( wrtrOffen );
  }

  /**
   * @param wks
   * @param flussBinDir
   * @param flussName
   */
  private static void writeProFile( final GMLWorkspace wks, final File flussBinDir, final String flussName ) throws Exception
  {
    final File flePro = new File( flussBinDir, flussName + ".pro" );
    final Writer wrtrPro = new OutputStreamWriter( new FileOutputStream( flePro ), WavosConst.WAVOS_CODEPAGE );
    final PrintWriter pWrtrPro = new PrintWriter( wrtrPro );

    final FeatureList pegelList = (FeatureList) wks.getFeatureFromPath( "pegelMember" );
    pWrtrPro.println( "# Elbe.pro: Projektion: 0=nein, 1 oder W = auf W, Q= auf Q " );
    // TODO? nach lfdNummer sortiert schreiben?
    for( final Iterator iter = pegelList.iterator(); iter.hasNext(); )
    {
      final Feature pegel = (Feature) iter.next();
      final Object propAllowed = pegel.getProperty( m_projektionAllowed );
      final boolean allowed = propAllowed == null ? false : ((Boolean) propAllowed).booleanValue();

      if( allowed )
      {
        pWrtrPro.println( String.format( "%4s%3s", pegel.getProperty( m_kenn ), pegel.getProperty( m_projektionType ) ) );
      }
    }
    IOUtils.closeQuietly( pWrtrPro );
    IOUtils.closeQuietly( wrtrPro );
  }

  /**
   * @param wks
   * @param inputDir
   * @param props
   * @param metaMap
   */

  private static void writeInput( final GMLWorkspace wks, final File inputDir, final Properties props, final Map metaMap ) throws Exception
  {
    final URL url = (URL) props.get( WavosConst.DATA_GML_CONTEXT );
    // eine Datei (Endung aus Datum des letzten Messwerts)
    final Date startForecast = (Date) props.get( WavosConst.DATA_STARTFORECAST_DATE );
    final int intervalAmount = Integer.parseInt( props.get( WavosConst.DATA_INTERVAL_AMOUNT ).toString() );

    final String nmeInput = WavosUtils.createInputFleName( startForecast );
    final File fleInput = new File( inputDir, nmeInput );
    final Writer wrtrInput = new OutputStreamWriter( new FileOutputStream( fleInput ), WavosConst.WAVOS_CODEPAGE );
    final PrintWriter pWrtrInput = new PrintWriter( wrtrInput );

    // Header schreiben
    // ##### allgemeine Eingaben
    // 6 tzurueck
    // 72.00000000000000 Vorhersagezeit
    // 0 USTI
    // 0 ZG_1
    // 0 ZG_2
    // 0 LOEB
    // 0 nvorweiter
    pWrtrInput.println( " ##### allgemeine Eingaben" );
    pWrtrInput.println( " " + props.get( WavosConst.DATA_SIMULATION_DURATION ).toString() + "  tszurueck" );
    pWrtrInput.println( " " + props.get( WavosConst.DATA_FORECAST_DURATION ).toString() + ".00000000000000  Vorhersagezeit" );
    // Liste der Pegel mit ZuflussVorhersage
    final FeatureList pegelList = (FeatureList) wks.getFeatureFromPath( "pegelMember" );
    final Map pegelMap = new HashMap();
    final Map tuppleMap = new HashMap();
    ITuppleModel tplValues = null;
    for( final Iterator iter = pegelList.iterator(); iter.hasNext(); )
    {
      final Feature pegel = (Feature) iter.next();
      final String lfdNum = String.format( "%05d", pegel.getProperty( m_lfdNum ) );
      pegelMap.put( lfdNum, pegel );

      final TimeseriesLinkType tsLink = (TimeseriesLinkType) pegel.getProperty( m_propZmlLink );
      final String href = tsLink.getHref();
      final URL zmlUrl = m_urlresolver.resolveURL( url, href );
      final URLConnection urlConTest = UrlUtilities.connectQuietly( zmlUrl );

      if( urlConTest != null )
      {
        final IObservation obsZml = ZmlFactory.parseXML( zmlUrl, lfdNum );
        tplValues = obsZml.getValues( null );
        if( metaMap != null )
        {
          final ZmlInfo info = new ZmlInfo( obsZml.getName(), obsZml.getMetadataList() );
          final String kenn = pegel.getProperty( m_kenn ).toString();
          metaMap.put( kenn, info );
        }
        final String axisToBeWritten = (String) pegel.getProperty( m_axisToBeWritten );
        final String axisType = "Q".equals( axisToBeWritten ) ? TimeserieConstants.TYPE_RUNOFF : TimeserieConstants.TYPE_WATERLEVEL;
        final IAxis axWerte = ObservationUtilities.findAxisByType( tplValues.getAxisList(), axisType );

        final List lstWerte = new ArrayList();
        for( int ii = 0; ii < tplValues.getCount(); ii++ )
        {
          lstWerte.add( tplValues.getElement( ii, axWerte ) );
        }
        tuppleMap.put( lfdNum, lstWerte );
      }

    }
    String kennZeile = String.format( "%17s", ""  );
    String stundenZeile = String.format( "%18s", ""  );
    String projektionsZeile = String.format( "%17s", ""  );
    // TreeMap zur Sortierung...
    final TreeMap pegelTreeMap = new TreeMap( pegelMap );
    final Iterator iter = pegelTreeMap.entrySet().iterator();
    while( iter.hasNext() )
    {
      final Map.Entry mapEntry = (Map.Entry) iter.next();
      final Feature pegel = (Feature) mapEntry.getValue();
      final Object property = pegel.getProperty( m_zuflussVorhersage );
      final boolean zuflussVorhersage = property == null ? false : ((Boolean) property).booleanValue();

      final String kenn = pegel.getProperty( m_kenn ).toString();
      if( zuflussVorhersage )
        pWrtrInput.println( " 0  " + kenn );

      kennZeile = kennZeile + String.format( "%6s", kenn  );
      stundenZeile = stundenZeile + String.format( "%6s", "0" );
      final Object propAllowed = pegel.getProperty( m_projektionAllowed );
      final boolean allowed = propAllowed == null ? false : ((Boolean) propAllowed).booleanValue();

      projektionsZeile = projektionsZeile + String.format( "%6s", allowed ? pegel.getProperty( m_projektionType ).toString() : "0" );
    }
    pWrtrInput.println( " 0  nvorweiter" );
    pWrtrInput.println( " ##### Anzahl Stunden bekannt: Werte hier nicht aendern" );
    pWrtrInput.println( kennZeile );
    pWrtrInput.println( stundenZeile );
    pWrtrInput.println( " ##### Projektionen: Werte koennen hier geaendert werden" );
    pWrtrInput.println( kennZeile );
    pWrtrInput.println( projektionsZeile );
    pWrtrInput.println( " ##### Wasserstaende: Werte koennen geaendert werden" );
    pWrtrInput.println( kennZeile );

    // TODO? Zeitversatz (Sommer/Winter)
    final Calendar c = Calendar.getInstance();
    c.setTime( startForecast );
    final int countMeasValues = ((Integer) props.get( WavosConst.DATA_COUNT_MEASURED_VALUES )).intValue();
    // Zeitreihe steckt jeweils in m_propZmlLink
    final TreeMap obsTreeMap = new TreeMap( tuppleMap );
    Iterator iterObs;

    // für alle Zeiten
    // für alle Pegel
    // Kennzeichen, Datum, Werte (je nach Weitergabe-Achse
    // m_AxisToBeWritten)
    final Date simStart = (Date) (props.get( WavosConst.DATA_SIMULATION_START ));
    c.setTime( simStart );

    for( int i = 0; i < countMeasValues; i++ )
    {
      iterObs = obsTreeMap.entrySet().iterator();
      // Datum verhackstücken
      // TODO? Zeitversatz (Sommer/Winter)
      final Date dateS = c.getTime();
      String zeile = "S " + WavosUtils.createWavosDate( dateS );
      // Werte formatiert schreiben
      while( iterObs.hasNext() )
      {
        final Map.Entry mapEntry = (Map.Entry) iterObs.next();
        final ArrayList valList = (ArrayList) mapEntry.getValue();
        zeile = zeile + String.format( "%6d", valList.get( i ) );
      }
      pWrtrInput.println( zeile );
      c.add( Calendar.HOUR_OF_DAY, intervalAmount );
    }
    pWrtrInput.println( kennZeile );
    final int countForecastValues = ((Integer) props.get( WavosConst.DATA_COUNT_FORECAST_VALUES )).intValue();
    for( int i = countMeasValues; i < countForecastValues + countMeasValues; i++ )
    {
      iterObs = obsTreeMap.entrySet().iterator();
      // TODO? Zeitversatz (Sommer/Winter)
      final Date dateV = c.getTime();
      String zeile = "V " + WavosUtils.createWavosDate( dateV );

      // Werte formatiert schreiben
      while( iterObs.hasNext() )
      {
        final Map.Entry mapEntry = (Map.Entry) iterObs.next();
        final ArrayList valList = (ArrayList) mapEntry.getValue();
        if( valList.size() > i )
          zeile = zeile + String.format( "%6d", valList.get( i ) );
        else
          zeile = zeile + String.format( "%6d", new Integer( -1 ) );
      }
      pWrtrInput.println( zeile );
      c.add( Calendar.HOUR_OF_DAY, intervalAmount );
    }
    IOUtils.closeQuietly( pWrtrInput );
    IOUtils.closeQuietly( wrtrInput );
  }

  private static void copyAndUnzipRechenkern( final File targetDir, final int zipType ) throws SimulationException
  {
    URL urlZip = null;
    InputStream zipStream = null;

    // rechenkern.zip aus den resourcen holen
    try
    {
      switch( zipType )
      {
        case WavosConst.ZIP_TYPE_BIN:
        {
          urlZip = WavosInputWorker.class.getResource( WavosConst.RECHENKERN_BIN_ZIP );
          break;
        }
        case WavosConst.ZIP_TYPE_DATA:
        {
          urlZip = WavosInputWorker.class.getResource( WavosConst.RECHENKERN_DATA_ZIP );
          break;
        }
      }

      zipStream = urlZip.openStream();
      ZipUtilities.unzip( zipStream, targetDir );

    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Entpacken des Rechenkerns", e );
    }
    finally
    {
      IOUtils.closeQuietly( zipStream );
    }
  }

  public static HashMap parseControlFile( final URL gmlURL ) throws SimulationException
  {
    try
    {
      final Feature controlFeature = GmlSerializer.createGMLWorkspace( gmlURL, null ).getRootFeature();

      final HashMap dataMap = new HashMap();

      final Date startForecastTime = (Date) controlFeature.getProperty( "startforecast" );
      dataMap.put( WavosConst.DATA_STARTFORECAST_DATE, startForecastTime );

      // String
      final Object objMergeCasePath = controlFeature.getProperty( "mergeCasePath" );
      if( objMergeCasePath != null )
      {
        final String mergeCasePath = ((String) objMergeCasePath);
        dataMap.put( WavosConst.DATA_MERGE_CASE_PATH, mergeCasePath );
      }
      else
        dataMap.put( WavosConst.DATA_MERGE_CASE_PATH, "" );

      // int [h]
      final Integer forecastDur = (Integer) controlFeature.getProperty( "forecastDuration" );
      dataMap.put( WavosConst.DATA_FORECAST_DURATION, forecastDur );

      // int [d]
      final Integer simulationDur = (Integer) controlFeature.getProperty( "simulationDuration" );
      dataMap.put( WavosConst.DATA_SIMULATION_DURATION, simulationDur );

      // int [h]
      final Integer intervalAmount = (Integer) controlFeature.getProperty( "intervalAmount" );
      dataMap.put( WavosConst.DATA_INTERVAL_AMOUNT, intervalAmount );

      // boolean
      final Object useAWerteProp = controlFeature.getProperty( "useAWerte" );
      final Boolean useAWerte = useAWerteProp == null ? Boolean.FALSE : ((Boolean) useAWerteProp);
      dataMap.put( WavosConst.DATA_USE_AWERTE, useAWerte );

      // calculate simulationStart = Tagesanfang(startForecast -
      // simulationDuration)
      final Calendar cal = Calendar.getInstance();
      cal.setTime( startForecastTime );
      cal.add( CalendarUtilities.getCalendarField( "DAY_OF_MONTH" ), -simulationDur.intValue() );
      Date simulationStart = cal.getTime();
      // TODO during upgrade to new KALYPSO replace usage of "trunc" with
      // method from
      // org.apache.commons.lang.CalendarUtils
      simulationStart = DateUtils.truncate( simulationStart, Calendar.DAY_OF_MONTH );
      dataMap.put( WavosConst.DATA_SIMULATION_START, simulationStart );

      // calculate count of measured values
      // TODO (interval.amount <> 1) überdenken: aufrunden oder abrunden
      // oder so...
      cal.setTime( startForecastTime );
      // TODO wird "1+" benötigt oder hat das was mit Winter und Sommer zu
      // tun?
      final Integer cntMeasVals = new Integer( (1 + simulationDur.intValue() * 24 + cal.get( Calendar.HOUR_OF_DAY )) / intervalAmount.intValue() );
      dataMap.put( WavosConst.DATA_COUNT_MEASURED_VALUES, cntMeasVals );

      // calculate count of vorgabe values
      // TODO (interval.amount <> 1) überdenken...
      final Integer cntForecastVals = new Integer( forecastDur.intValue() / intervalAmount.intValue() );
      dataMap.put( WavosConst.DATA_COUNT_FORECAST_VALUES, cntForecastVals );

      return dataMap;
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Fehler beim Einlesen der Berechnungsparameter", e );
    }
  }
}
