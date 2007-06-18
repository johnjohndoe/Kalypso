/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * 
 * Conversion between native timeseries format and ZML
 * 
 * @author thuel2
 */
public class ElbePolteConverter
{
  public final static String hwvsTypeQ = TimeserieConstants.TYPE_RUNOFF;
  public final static int step = 3;

  public static IObservation hwvs2zml( final File fleHwvs, final String name )
  {
    InputStreamReader isRdrHwvs = null;
    IObservation obsZML = null;

    try
    {
      isRdrHwvs = new InputStreamReader( new FileInputStream( fleHwvs ), ElbePolteConst.ELBEPOLTE_CODEPAGE );
      obsZML = hwvs2zml( isRdrHwvs, name );
    }
    catch( UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }
    catch( FileNotFoundException e )
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
   * @param isRdrHwvs
   * @param name
   * @return
   */
  private static IObservation hwvs2zml( final InputStreamReader isRdrHwvs, final String name )
  {
    final LineNumberReader lneNumRdr;
    final Date dtDatum;
    final List lstEingWerte;
    final List lstEingDatum;
    final Calendar calendar = new GregorianCalendar();

    IObservation obsOut = null;
    //    MetadataList metaDataList;
    IAxis[] axis;
    ITuppleModel tplWerte;
    StringTokenizer strTok;
    String sEingabeZeile;
    String sComment;

    //    int ii;

    lstEingWerte = new ArrayList();
    lstEingDatum = new ArrayList();
    Object[][] tuppleData;
    //    metaDataList = new MetadataList();

    // Codepage wird dem reader schon vorher mitgegeben
    lneNumRdr = new LineNumberReader( isRdrHwvs );
    try
    {
      // Kommentar, Datum und Schrittweite lesen
      sComment = lneNumRdr.readLine();
      //      dtDatum = TubigConst.TUBIG_DATE_FORMAT.parse( lneNumRdr.readLine() );
      //      calendar.setTime( dtDatum );
      // ein bisschen umst‰ndlich, da manchmal noch Text nach der Schrittweite
      // kommt...
      sEingabeZeile = lneNumRdr.readLine();
      strTok = new StringTokenizer( sEingabeZeile );
      if( strTok.hasMoreTokens() )
      {
        axis = createAxis( hwvsTypeQ );
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
        //        tuppleData = new Object[lstEingDatum.size()][2];
        //        for( ii = 0; ii < lstEingDatum.size(); ii++ )
        //        {
        //          tuppleData[ii][0] = lstEingDatum.get( ii );
        //          tuppleData[ii][1] = lstEingWerte.get( ii );
        //        }
        //
        //        tplWerte = new SimpleTuppleModel( axis, tuppleData );
        //        obsOut = new SimpleObservation( "href", "ID", name, false, null, metaDataList, axis, tplWerte );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      //      throw new TubigException( "Fehler beim Lesen einer TUBIG-Datei (Schreiben von Zeitreihen - ZML", e );
    }
//    catch( final ParseException e1 )
//    {
//      // lneNumRdr.getLineNumber() gibt die problematische Zeilennummer an
//      e1.printStackTrace();
//      //      throw new TubigException( "Fehler beim Parsen eines Datums (Schreiben von Zeitreihen - ZML", e1 );
//    }
    finally
    {
      IOUtils.closeQuietly( lneNumRdr );
    }
    return obsOut;
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
    final InputStream is = ElbePolteConverter.class.getResourceAsStream( "resources/test/Daten/Daten.001" );
    final Reader reader = new BufferedReader( new InputStreamReader( is ) );
  }
}
