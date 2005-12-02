/*
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestraße 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URL;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.lhwsachsenanhalt.tubig.exceptions.TubigException;
import org.kalypso.lhwsachsenanhalt.tubig.utils.TubigUtils;
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

import com.braju.format.Format;

/**
 * Im- und Export von TUBIG-Dateien von und nach ZML <br>
 * TUBIG = spezielles Dateiformat der Thiele & Büttner Ingenieurgesellschaft für Zeitreihen-Dateien. <br>
 * Verwendung für Tubigmodell-Dateien (*.vq, *.pq, *.q usw.)
 * 
 * @author Thül
 */

public class TubigConverter
{

  /**
   * schreibt die gesamte Zeitreihe in TUBIG-Datei
   */
  public static void zml2Tubig( final IObservation obsZml, final Writer wrtr, final int step, final String sValueType )
      throws SensorException, IOException
  {
    zml2Tubig( obsZml, wrtr, step, sValueType, null );
  }

  /**
   * schreibt Zeitreihe ab bzw. bis zu angegebenem Datum in TUBIG-Datei (bei step = -1 werden die Daten bis
   * einschließlich dtStartForecast geschrieben, bei step = 1 werden die Werte zum Datum echt größer dtStartForecast
   * geschrieben)
   * 
   * @param step
   *          gibt Sortierung/Reihenfolge der Daten an (Zukunft = 1, Vergangenheit = -1)
   * @param dtStartForecast
   *          Beginn der Vorhersage = aktuelle Modellzeit
   */
  public static void zml2Tubig( final IObservation obsZml, final Writer wrtr, final int step, final String sValueType,
      final Date dtStartForecast ) throws SensorException, IOException
  {
    final String sComment;
    final PrintWriter pWrtr;
    final InterpolationFilter intpolFlt;
    final ITuppleModel tplWerte;
    final IAxis axDatum;
    final IAxis axWerte;
    final List lstAusgWerte;
    final List lstAusgDatum; // Ist eigentlich überflüssig
    //final Iterator itDatum;
    Calendar calendar;

    int ii;
    Number wert;
    Date dtDatum;

    lstAusgWerte = new ArrayList();
    lstAusgDatum = new ArrayList();
    calendar = new GregorianCalendar();

    pWrtr = new PrintWriter( wrtr );

    try
    {
      intpolFlt = new InterpolationFilter( Calendar.HOUR_OF_DAY, Math.abs( step ), false, 0.0, KalypsoStati.BIT_CHECK );
      intpolFlt.initFilter( null, obsZml, null );

      // Kommentar aus ZML-Datei lesen und in TUBIG-Datei schreiben
      sComment = intpolFlt.getMetadataList().getProperty( TubigConst.PROP_COMMENT, "REM " );
      pWrtr.println( sComment );

      tplWerte = intpolFlt.getValues( null );
      axDatum = ObservationUtilities.findAxisByType( tplWerte.getAxisList(), TimeserieConstants.TYPE_DATE );
      axWerte = ObservationUtilities.findAxisByType( tplWerte.getAxisList(), sValueType );
      //      axStatus = KalypsoStatusUtils.findStatusAxisFor(
      // tplWerte.getAxisList(), axWerte );

      if( dtStartForecast != null )
      {
        for( ii = 0; ii < tplWerte.getCount(); ii++ )
        {
          dtDatum = (Date)tplWerte.getElement( ii, axDatum );
          // Nur in Listen mit aufnehmen, wenn kleiner oder gleich bzw. größer
          // dtStartForecast
          if( ( step < 0 && ( dtDatum.before( dtStartForecast ) || dtDatum.equals( dtStartForecast ) ) )
              || ( step > 0 && dtDatum.after( dtStartForecast ) ) )
          {
            lstAusgDatum.add( dtDatum );

            wert = (Number)tplWerte.getElement( ii, axWerte );
            lstAusgWerte.add( wert );
          }
        }
      }
      else
      {
        for( ii = 0; ii < tplWerte.getCount(); ii++ )
        {
          dtDatum = (Date)tplWerte.getElement( ii, axDatum );
          lstAusgDatum.add( dtDatum );

          wert = (Number)tplWerte.getElement( ii, axWerte );
          lstAusgWerte.add( wert );
        }
      }
      if( step < 0 )
      {
        Collections.reverse( lstAusgDatum );
        Collections.reverse( lstAusgWerte );
      }

      // Datum, Schrittweite und Werte
      // wenn kein Zahlenwert vorhanden, besteht die Datei die Prüfung durch m_fehler nicht...
      if( lstAusgDatum.size() > 0 )
      {
        dtDatum = (Date)lstAusgDatum.get( 0 );
      }
      else
      {
        if( step < 0 )
        {
          dtDatum = dtStartForecast;
        }
        else
        {
          calendar.setTime( dtStartForecast );
          calendar.add( Calendar.HOUR_OF_DAY, step );
          dtDatum = calendar.getTime();
        }
      }
      pWrtr.println( TubigConst.TUBIG_DATE_FORMAT.format( dtDatum ) );
      pWrtr.println( step );

      //itDatum = lstAusgDatum.iterator();
      for( final Iterator it = lstAusgWerte.iterator(); it.hasNext(); )
      {
        wert = (Number)it.next();
        Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
        { wert } );

        // Datum kann geschrieben werden (zusätzliche Informationen werden vom
        // Rechenkern ignoriert)
        //dtDatum = (Date)itDatum.next();
        //pWrtr.print( " " + TUBIG_DATE_FORMAT.format( dtDatum ) );

        pWrtr.println();
      }
    }
    finally
    {
      IOUtils.closeQuietly( pWrtr );
    }
  }

  public static void zml2Tubig( final IObservation obsZml, final File fleTubig, final int step )
      throws SensorException, IOException
  {
    zml2Tubig( obsZml, fleTubig, step, null );
  }

  public static void zml2Tubig( final IObservation obsZml, final File fleTubig, final int step,
      final Date dtStartForecast ) throws SensorException, IOException
  {
    final Writer wrtrTubig;
    final String sValueType;

    wrtrTubig = new OutputStreamWriter( new FileOutputStream( fleTubig ), TubigConst.TUBIG_CODEPAGE );
    sValueType = TubigUtils.getObservationType( fleTubig );
    zml2Tubig( obsZml, wrtrTubig, step, sValueType, dtStartForecast );
    IOUtils.closeQuietly( wrtrTubig );
  }

  public static IObservation tubig2Zml( final Reader reader, final String sValueType, final String name )
      throws TubigException
  {
    final LineNumberReader lneNumRdr;
    final Date dtDatum;
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
    int step;
    int ii;

    lstEingWerte = new ArrayList();
    lstEingDatum = new ArrayList();
    Object[][] tuppleData;
    metaDataList = new MetadataList();

    // Codepage wird dem reader schon vorher mitgegeben
    lneNumRdr = new LineNumberReader( reader );
    try
    {
      // Kommentar, Datum und Schrittweite lesen
      sComment = lneNumRdr.readLine();
      dtDatum = TubigConst.TUBIG_DATE_FORMAT.parse( lneNumRdr.readLine() );
      calendar.setTime( dtDatum );
      // ein bisschen umständlich, da manchmal noch Text nach der Schrittweite
      // kommt...
      sEingabeZeile = lneNumRdr.readLine();
      strTok = new StringTokenizer( sEingabeZeile );
      if( strTok.hasMoreTokens() )
      {
        step = (int)( Double.parseDouble( strTok.nextToken() ) );

        metaDataList.setProperty( TubigConst.PROP_COMMENT, sComment );
        axis = createAxis( sValueType );

        // Werte lesen und Datum generieren
        sEingabeZeile = lneNumRdr.readLine();
        while( sEingabeZeile != null )
        {
          strTok = new StringTokenizer( sEingabeZeile );

          if( strTok.hasMoreTokens() )
            lstEingWerte.add( new Double( strTok.nextToken() ) );
          lstEingDatum.add( calendar.getTime() );
          calendar.add( Calendar.HOUR_OF_DAY, step );

          sEingabeZeile = lneNumRdr.readLine();
        }

        if( step < 0 )
        {
          Collections.reverse( lstEingDatum );
          Collections.reverse( lstEingWerte );
        }

        tuppleData = new Object[lstEingDatum.size()][2];
        for( ii = 0; ii < lstEingDatum.size(); ii++ )
        {
          tuppleData[ii][0] = lstEingDatum.get( ii );
          tuppleData[ii][1] = lstEingWerte.get( ii );
        }

        tplWerte = new SimpleTuppleModel( axis, tuppleData );
        obsOut = new SimpleObservation( "href", "ID", name, false, null, metaDataList, axis, tplWerte );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new TubigException( "Fehler beim Lesen einer TUBIG-Datei (Schreiben von Zeitreihen - ZML", e );
    }
    catch( final ParseException e1 )
    {
      // lneNumRdr.getLineNumber() gibt die problematische Zeilennummer an
      e1.printStackTrace();
      throw new TubigException( "Fehler beim Parsen eines Datums (Schreiben von Zeitreihen - ZML", e1 );
    }
    finally
    {
      IOUtils.closeQuietly( lneNumRdr );
    }
    return obsOut;
  }

  public static IObservation tubig2Zml( final File fleTubig, final String name ) throws TubigException
  {
    InputStreamReader rdrTubig;
    final String sObsType;

    IObservation obsZml;

    obsZml = null;
    rdrTubig = null;

    sObsType = TubigUtils.getObservationType( fleTubig );
    try
    {
      //rdrTubig = new FileReader( fleTubig );
      rdrTubig = new InputStreamReader( new FileInputStream( fleTubig ), TubigConst.TUBIG_CODEPAGE );
      obsZml = tubig2Zml( rdrTubig, sObsType, name );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
      throw new TubigException( "Encoding " + TubigConst.TUBIG_CODEPAGE + " wird für " + fleTubig.getName()
          + " nicht unterstützt. (Schreiben von Zeitreihen - ZML", e );
    }
    catch( final FileNotFoundException e )
    {
      e.printStackTrace();
      throw new TubigException( "Datei " + fleTubig.getName()
          + " kann nicht gefunden werden (Schreiben von Zeitreihen - ZML)", e );
    }
    finally
    {
      IOUtils.closeQuietly( rdrTubig );
    }
    return obsZml;
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

  public static void createAktDtTxt( final File fleExeDir, final Date dtZeit ) throws TubigException
  {
    final String sSchritt = "1.00000000000000E-0004";
    final String sMin = "0";
    final String sMax = "100";

    createAktDtTxt( fleExeDir, dtZeit, sSchritt, sMin, sMax );
  }

  public static void createAktDtTxt( final File fleExeDir, final Date dtAktModellZeit, final String sSchritt,
      final String sMin, final String sMax ) throws TubigException
  {
    try
    {
      final FileWriter wrtrAktDtTxt = new FileWriter( new File( fleExeDir, TubigConst.AKTDT_FILE_NAME ) );
      createAktDtTxt( wrtrAktDtTxt, dtAktModellZeit, sSchritt, sMin, sMax );
      IOUtils.closeQuietly( wrtrAktDtTxt );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new TubigException( "Fehler beim Schreiben der Datei " + TubigConst.AKTDT_FILE_NAME, e );
    }
  }

  public static void createAktDtTxt( final Writer wrtr, final Date dtAktModellZeit, final String sSchritt,
      final String sMin, final String sMax )
  {
    final PrintWriter pWrtr = new PrintWriter( wrtr );

    pWrtr.println( TubigConst.AKTDT_FILE_COMMENT );
    pWrtr.println( TubigConst.TUBIG_DATE_FORMAT.format( dtAktModellZeit ) );
    pWrtr.println( sSchritt );
    pWrtr.println( sMin );
    pWrtr.println( sMax );
  }

  public static void main( final String[] args ) throws SensorException, IOException, ParseException, TubigException
  {
    //    String sDateiEndung;

    // Test-ZML laden
    //final URL zmlurl = TubigConverter.class.getResource(
    // "resources/W_SCHIRG.zml" );
    final URL zmlurl = TubigConverter.class.getResource( "resources/elen_vw2.zml" );
    final IObservation observation = ZmlFactory.parseXML( zmlurl, "Test" );

    // convert it
    final FileWriter writer = new FileWriter( new File( System.getProperty( "java.io.tmpdir" ), "elen_test.vw" ) );
    zml2Tubig( observation, writer, -1, TimeserieConstants.TYPE_WATERLEVEL );
    IOUtils.closeQuietly( writer );
    Date dtZeit;
    dtZeit = TubigConst.TUBIG_DATE_FORMAT.parse( "15.12.2004 07:00" );

    createAktDtTxt( new File( System.getProperty( "java.io.tmpdir" ) ), dtZeit );
    // read it

    final File leseDatei = new File( System.getProperty( "java.io.tmpdir" ), "elen.vw" );
    final File schreibeDatei = new File( System.getProperty( "java.io.tmpdir" ), "elen_neu.vw" );
    //final InputStreamReader reader = new FileReader( leseDatei );
    //sDateiEndung = TubigUtils.getObservationType( leseDatei );
    //tubig2Zml( reader, sDateiEndung );
    //IOUtils.closeQuietly( reader );

    IObservation obsTest;
    obsTest = tubig2Zml( leseDatei, "name" );
    zml2Tubig( obsTest, schreibeDatei, -1 );
  }

}

//

//

//

//