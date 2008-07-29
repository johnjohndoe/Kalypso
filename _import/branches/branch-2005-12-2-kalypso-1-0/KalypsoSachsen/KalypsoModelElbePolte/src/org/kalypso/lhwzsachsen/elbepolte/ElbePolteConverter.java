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
package org.kalypso.lhwzsachsen.elbepolte;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.interpolation.InterpolationFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObservationType;

import com.braju.format.Format;

/**
 * 
 * Conversion between native timeseries format and ZML
 * 
 * @author thuel2
 */
public class ElbePolteConverter
{
  public final static String hwvsTypeQ = TimeserieConstants.TYPE_RUNOFF;
  public final static int cntValuesPast = 120;

  public static void hwvs2zml( final File fleHwvs, final File fleZml, Map metaMap )
  {
    OutputStream writer = null;
    try
    {
      final InputStreamReader isr = new InputStreamReader( new FileInputStream( fleHwvs ) );
      final BufferedReader reader = new BufferedReader( isr );
      final IObservation obsZml = hwvs2zml( reader );
      ZmlInfo info = null;

      // add metaData

      // key erzeugen
      final String mapKey = getExtension(fleHwvs);
      info = (ZmlInfo)metaMap.get( mapKey );

      // Metadaten in obs übertragen
      // ein bisserl tricky, damit die neuen Werte garantiert die alten
      // überschreiben
      final MetadataList metadataList = obsZml.getMetadataList();
      final MetadataList newmeta = new MetadataList();
      if( info != null )
      {
        final MetadataList oldmeta = info.getMetadata();
        newmeta.putAll( oldmeta );
      }
      newmeta.putAll( metadataList );
      metadataList.putAll( newmeta );

      // convert it
      writer = new FileOutputStream( fleZml );
      ObservationType observationType = ZmlFactory.createXML( obsZml, null );
      ZmlFactory.getMarshaller().marshal( observationType, writer );

    }
    // TODO Exceptions behandeln
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  public static IObservation hwvs2zml( final File fleHwvs )
  {
    InputStreamReader isRdrHwvs = null;
    IObservation obsZML = null;

    try
    {
      isRdrHwvs = new InputStreamReader( new FileInputStream( fleHwvs ), ElbePolteConst.ELBEPOLTE_CODEPAGE );
      obsZML = hwvs2zml( new BufferedReader( isRdrHwvs ) );
    }
    catch( FileNotFoundException e )
    {
      e.printStackTrace();
    }
    catch( UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( isRdrHwvs );
    }
    return obsZML;
  }

  /**
   * @param bufRdrHwvs
   */
  private static IObservation hwvs2zml( final BufferedReader bufRdrHwvs )
  {
    final LineNumberReader lneNumRdr;

    final List lstEingWerte;
    final List lstEingDatum;
    final Calendar calendar = new GregorianCalendar();

    IObservation obsOut = null;
    MetadataList metaDataList;
    IAxis[] axis;
    ITuppleModel tplWerte;
    StringTokenizer strTok;
    String sEingabeZeile;
    String sComment;
    Date dtDatum = null;
    Date vorhersageDte = null;

    lstEingWerte = new ArrayList();
    lstEingDatum = new ArrayList();
    Object[][] tuppleData;
    metaDataList = new MetadataList();

    // Codepage wird dem reader schon vorher mitgegeben
    lneNumRdr = new LineNumberReader( bufRdrHwvs );
    try
    {
      // Kommentar (= erste Zeile) lesen
      sComment = lneNumRdr.readLine();
      metaDataList.setProperty( ElbePolteConst.PROP_COMMENT, sComment );
      axis = createAxis( hwvsTypeQ );

      sEingabeZeile = lneNumRdr.readLine();
      strTok = new StringTokenizer( sEingabeZeile );
      String strDate = "";

      if( strTok.hasMoreTokens() )
      {
        // Datum und Werte lesen und Listeneinträge generieren
        while( sEingabeZeile != null )
        {

          try
          {
            int lfdNum = 0;
            Integer.parseInt( sEingabeZeile.trim().substring( 0, 1 ) );

            // Format: lfd_Nummer, Jahr, Monat, Tag, Stunde, Wert
            strTok = new StringTokenizer( sEingabeZeile );
            if( strTok.hasMoreTokens() )
            {
              // die ersten Einträge werden übersprungen
              lfdNum = Integer.valueOf( strTok.nextToken() ).intValue();
              strDate = strTok.nextToken() + " " + strTok.nextToken() + " " + strTok.nextToken() + " "
                  + strTok.nextToken();
              dtDatum = ElbePolteConst.HWVS_DATE_FORMAT.parse( strDate );
              calendar.setTime( dtDatum );

              if( lfdNum == cntValuesPast )
                vorhersageDte = dtDatum;

              lstEingDatum.add( calendar.getTime() );
              lstEingWerte.add( new Double( strTok.nextToken() ) );
            }

          }
          catch( NumberFormatException e )
          {
            // keine gültige Zeile (bzw. Kommentarzeile)
          }
          sEingabeZeile = lneNumRdr.readLine();
        }

        tuppleData = new Object[lstEingDatum.size()][2];
        for( int ii = 0; ii < lstEingDatum.size(); ii++ )
        {
          tuppleData[ii][0] = lstEingDatum.get( ii );
          tuppleData[ii][1] = lstEingWerte.get( ii );
        }
        tplWerte = new SimpleTuppleModel( axis, tuppleData );
        obsOut = new SimpleObservation( "href", "ID", hwvsTypeQ, false, null, metaDataList, axis, tplWerte );

        // Vorhersagezeitraum in MetaData setzen
        TimeserieUtils.setForecast( obsOut, vorhersageDte, dtDatum );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      //      throw new TubigException( "Fehler beim Lesen einer TUBIG-Datei (Schreiben von Zeitreihen - ZML", e );
    }
    catch( final ParseException e1 )
    {
      // lneNumRdr.getLineNumber() gibt die problematische Zeilennummer an
      e1.printStackTrace();
      // throw new TubigException( "Fehler beim Parsen eines Datums (Schreiben von Zeitreihen - ZML", e1 );
    }
    finally
    {
      IOUtils.closeQuietly( lneNumRdr );
    }
    return obsOut;
  }

  public static void zml2Hwvs( final File fleZml, final File fleHwvs, final Map metaMap ) throws Exception
  {

    final URL url = fleZml.toURL();

    if( !zml2Hwvs( url, fleHwvs, url.toString(), metaMap ) )
    {
      throw new Exception( "Zeitreihe " + url.toString()
          + " kann nicht geöffnet werden (da vielleicht nicht vorhanden)." );
    }
  }

  /**
   * @param zmlUrl
   * @param fleHwvs
   * @param metaMap
   * @return has zml (URL) successfully been written to fleHwvs
   * @throws Exception
   */
  public static boolean zml2Hwvs( final URL zmlUrl, final File fleHwvs, final String zmlId, Map metaMap )
      throws Exception
  {
    final IObservation obsZml = ZmlFactory.parseXML( zmlUrl, zmlId );
    
    final URLConnection urlConTest = UrlUtilities.connectQuietly( zmlUrl );

    if( urlConTest != null )
    {
      // TODO Zeitraum in Kommentarzeile wäre noch schön :-)
       final String sComment = fleHwvs.getName() + ": " + obsZml.getIdentifier() + " (" + zmlId + ")";

      zml2Hwvs( obsZml, fleHwvs, sComment, metaMap );
      return true;
    }
    return false;
  }

  /**
   * @param obsZml
   * @param fleHwvs
   * @param sComment
   * @throws Exception
   */
  public static void zml2Hwvs( final IObservation obsZml, final File fleHwvs, final String sComment, final Map metaMap )
      throws Exception

  {

    final Writer wrtrHwvs = new OutputStreamWriter( new FileOutputStream( fleHwvs ), ElbePolteConst.ELBEPOLTE_CODEPAGE );
    final String mapKey = getExtension(fleHwvs);

    zml2Hwvs( obsZml, wrtrHwvs, sComment, metaMap, mapKey );
    IOUtils.closeQuietly( wrtrHwvs );
  }

  public static void zml2Hwvs( final IObservation obsZml, final Writer wrtr, String sComment, final Map metaMap, final String mapKey )
  {
    final PrintWriter pWrtr;
    final InterpolationFilter intpolFlt;
    final ITuppleModel tplWerte;
    final IAxis axDatum;
    final IAxis axWerte;

    int ii;
    Number wert;
    Date dtDatum;

    pWrtr = new PrintWriter( wrtr );

    if( metaMap != null )
    {
      final ZmlInfo info = new ZmlInfo( obsZml.getName(), obsZml.getMetadataList() );

      metaMap.put( mapKey, info );
    }

    try
    {
      intpolFlt = new InterpolationFilter( Calendar.HOUR_OF_DAY, Math.abs( ElbePolteConst.ELBEPOLTE_TIMESTEP ), false,
          "0.0", KalypsoStati.BIT_CHECK );

      intpolFlt.initFilter( null, obsZml, null );

      // Kommentar aus ZML-Datei lesen, anhängen und in ElbePolte-Datei schreiben
      sComment = sComment + " " + intpolFlt.getMetadataList().getProperty( ElbePolteConst.PROP_COMMENT, "REM " );

      pWrtr.println( sComment );

      tplWerte = intpolFlt.getValues( null );
      axDatum = ObservationUtilities.findAxisByType( tplWerte.getAxisList(), TimeserieConstants.TYPE_DATE );
      axWerte = ObservationUtilities.findAxisByType( tplWerte.getAxisList(), hwvsTypeQ );

      for( ii = 0; ii < tplWerte.getCount(); ii++ )
      {
        // Format: lfd_Nummer, Jahr, Monat, Tag, Stunde, Wert

        dtDatum = (Date)tplWerte.getElement( ii, axDatum );
        wert = (Number)tplWerte.getElement( ii, axWerte );

        Format.fprintf( pWrtr, ElbePolteConst.HWVS_FORMAT_DATA_ROW, new Object[]
        {
            ( new Integer( ii + 1 ) ),
            ElbePolteConst.HWVS_DATE_FORMAT_YEAR.format( dtDatum ),
            ElbePolteConst.HWVS_DATE_FORMAT_MONTH.format( dtDatum ),
            ElbePolteConst.HWVS_DATE_FORMAT_DAY.format( dtDatum ),
            ElbePolteConst.HWVS_DATE_FORMAT_HOUR.format( dtDatum ),
            wert } );
        pWrtr.println();
      }
    }

    catch( SensorException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( pWrtr );
    }
  }

  public static IAxis[] createAxis( final String sValueType )
  {
    final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true );
    TimeserieUtils.getUnit( sValueType );
    final IAxis valueAxis = new DefaultAxis( TimeserieUtils.getName( sValueType ), sValueType, TimeserieUtils
        .getUnit( sValueType ), Double.class, false );
    final IAxis[] axis = new IAxis[]
    {
        dateAxis,
        valueAxis };
    return axis;
  }

  public static void main( String[] args )
  {
    try

    {

      File fleHwvsIn = new File( "c:/temp/polte_zr/Modell.009" );
      File fleZml = new File( "c:/temp/polte_zr", "Modell.009.zml" );
      hwvs2zml( fleHwvsIn, fleZml, null );

      //      File fleHwvsIn = new File( "c:/temp/polte_zr/Dresden_Torgau.001" );
      //      File fleZml = new File( "c:/temp/polte_zr", "Dresden_Torgau.001.zml" );
      //      hwvs2zml( fleHwvsIn, fleZml );
      //
      //      
      //       fleHwvsIn = new File( "c:/temp/polte_zr/Usti_Dresden.001" );
      //       fleZml = new File( "c:/temp/polte_zr", "Usti_Dresden.001.zml" );
      //      hwvs2zml( fleHwvsIn, fleZml );
      //
      //       fleHwvsIn = new File( "c:/temp/polte_zr/Loeben.001" );
      //       fleZml = new File( "c:/temp/polte_zr", "Loeben.001.zml" );
      //      hwvs2zml( fleHwvsIn, fleZml );

      //      for( int ii = 1; ii <= 15; ii++ )
      //      {
      //        File fleHwvsIn = new File( "c:/temp/polte_zr/Modell.0" + ii );
      //        File fleZml = new File( "c:/temp/polte_zr", "Modell.0" + ii + ".zml" );
      //        if( fleHwvsIn.exists() )
      //          hwvs2zml( fleHwvsIn, fleZml );
      //
      //        fleHwvsIn = new File( "c:/temp/polte_zr/Modell.00" + ii );
      //        fleZml = new File( "c:/temp/polte_zr", "Modell.00" + ii + ".zml" );
      //        if( fleHwvsIn.exists() )
      //          hwvs2zml( fleHwvsIn, fleZml );
      //
      //        fleHwvsIn = new File( "c:/temp/polte_zr/Daten.0" + ii );
      //        fleZml = new File( "c:/temp/polte_zr", "Daten.0" + ii + ".zml" );
      //        if( fleHwvsIn.exists() )
      //          hwvs2zml( fleHwvsIn, fleZml );
      //
      //        fleHwvsIn = new File( "c:/temp/polte_zr/Daten.00" + ii );
      //        fleZml = new File( "c:/temp/polte_zr", "Daten.00" + ii + ".zml" );
      //        if( fleHwvsIn.exists() )
      //          hwvs2zml( fleHwvsIn, fleZml );
      //      }

      //      File fleHwvsIn = new File( ElbePolteConverter.class.getResource( "resources/test/Daten/Daten.001" ).getFile()
      // );
      //      File fleZml = new File( System.getProperty( "java.io.tmpdir" ), "Daten.001.zml" );
      //      File fleHwvsOut = new File( System.getProperty( "java.io.tmpdir" ), "Daten.001" );
      //      hwvs2zml( fleHwvsIn, fleZml );
      //      zml2Hwvs( fleZml, fleHwvsOut );
      //
      //      fleHwvsIn = new File( ElbePolteConverter.class.getResource( "resources/test/Modell/Modell.001" ).getFile() );
      //      fleZml = new File( System.getProperty( "java.io.tmpdir" ), "Modell.001.zml" );
      //      fleHwvsOut = new File( System.getProperty( "java.io.tmpdir" ), "Modell.001" );
      //      hwvs2zml( fleHwvsIn, fleZml );
      //      zml2Hwvs( fleZml, fleHwvsOut );
      //
      //      fleHwvsIn = new File( ElbePolteConverter.class.getResource( "resources/test/ZWG/Dresden_Torgau.001" ).getFile()
      // );
      //      fleZml = new File( System.getProperty( "java.io.tmpdir" ), "Dresden_Torgau.001.zml" );
      //      fleHwvsOut = new File( System.getProperty( "java.io.tmpdir" ), "Dresden_Torgau.001" );
      //      hwvs2zml( fleHwvsIn, fleZml );
      //      zml2Hwvs( fleZml, fleHwvsOut );
      //
      //      fleHwvsIn = new File( ElbePolteConverter.class.getResource( "resources/test/ZWG/Loeben.001" ).getFile() );
      //      fleZml = new File( System.getProperty( "java.io.tmpdir" ), "Loeben.001.zml" );
      //      fleHwvsOut = new File( System.getProperty( "java.io.tmpdir" ), "Loeben.001" );
      //      hwvs2zml( fleHwvsIn, fleZml );
      //      zml2Hwvs( fleZml, fleHwvsOut );
      //
      //      fleHwvsIn = new File( ElbePolteConverter.class.getResource( "resources/test/ZWG/Usti_Dresden.001" ).getFile()
      // );
      //      fleZml = new File( System.getProperty( "java.io.tmpdir" ), "Usti_Dresden.001.zml" );
      //      fleHwvsOut = new File( System.getProperty( "java.io.tmpdir" ), "Usti_Dresden.001" );
      //      hwvs2zml( fleHwvsIn, fleZml );
      //      zml2Hwvs( fleZml, fleHwvsOut );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * gibt nur den Dateinamen zurück (ohne Dateiendung) <br>
   */
  public static String getFileNameWOExt( final File file )
  {
    String sFleName;
    final String sExt;
    int pos;

    sFleName = file.getName();
    sExt = getExtension( file );
    pos = sFleName.lastIndexOf( "." + sExt );
    sFleName = sFleName.substring( 0, pos );
    return sFleName;
  }

  /**
   * Gibt Datei-Extension zurück (z.B. png)
   */
  public static String getExtension( final File file )
  {
    String ext;
    String s;
    int pos;

    ext = null;
    s = file.getName();
    pos = s.lastIndexOf( '.' );

    if( pos > 0 && pos < s.length() - 1 )
    {
      ext = s.substring( pos + 1 );
    }
    return ext;
  }
}
