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
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Formatter;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author ilya
 */
public class SWANResults2RmaConverter
{
  private final Map<String, Map<GM_Position, Double>> m_mapSWANResults;

  private final List<GM_Position> m_listOriginalPositions;

  private final Date[] m_arraySteps;

  private String[] m_arrayNamesForOutput = new String[] { "WForce_x", "WForce_y" }; //$NON-NLS-1$ //$NON-NLS-2$

  private int m_intAmountOfValues;

  private Double m_doubleShiftX;

  private Double m_doubleShiftY;

  public SWANResults2RmaConverter( )
  {
    m_mapSWANResults = null;
    m_listOriginalPositions = null;
    m_arraySteps = null;
    m_intAmountOfValues = 0;
  }

  public SWANResults2RmaConverter( final Map<String, Map<GM_Position, Double>> pMapSWANResults, final List<GM_Position> pListOriginalPositions, final Date[] pArraySteps, final List<String> pListValuesNames, final Double pDoubleShiftX, final Double pDoubleShiftY )
  {
    m_mapSWANResults = pMapSWANResults;
    m_listOriginalPositions = pListOriginalPositions;
    m_arraySteps = pArraySteps;
    m_doubleShiftX = pDoubleShiftX;
    m_doubleShiftY = pDoubleShiftY;
    if( m_mapSWANResults.size() > 0 )
    {
      final Iterator<Map<GM_Position, Double>> lIterator = m_mapSWANResults.values().iterator();
      if( lIterator.hasNext() )
      {
        final Map<GM_Position, Double> lMapValues = lIterator.next();
        m_intAmountOfValues = lMapValues.size();
      }
    }
    if( pListValuesNames != null )
      m_arrayNamesForOutput = pListValuesNames.toArray( new String[pListValuesNames.size()] );
  }

  public void writeRMAWaterSurfaceASCDataFile( final File outputFile ) throws IOException
  {
    try( OutputStream outputStream = new BufferedOutputStream( new FileOutputStream( outputFile ) ) )
    {
      writeRMAWaterSurfaceASCDataFile( outputStream );
    }
  }

  private void writeRMAWaterSurfaceASCDataFile( final OutputStream pOutputStream ) throws IOException
  {
    // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
    // so no local parameter for each format is needed any more .
    try( Formatter formatter = new Formatter( pOutputStream, Charset.defaultCharset().name(), Locale.US ) )
    {
      writeASCFile( formatter );
      FormatterUtils.checkIoException( formatter );
    }
  }

  private void writeASCFile( final Formatter formatter )
  {
    writeHeader( formatter );
    if( m_arraySteps != null )
    {
      for( final Date lActDate : m_arraySteps )
      {
        writeDateLine( formatter, lActDate );
        writeData( formatter, lActDate );
      }
    }
    writeEndLine( formatter );

  }

  private void writeEndLine( final Formatter formatter )
  {
    formatter.format( "ENDDATA\n" ); //$NON-NLS-1$
  }

  /**
   * 01-02 ID A "DY" 03-08 Blank 09-16 IDYY I Julian day of year 17-24 TTT R Hour of day 25-32 NV I Number of external
   * stress values 33-40 IYDD I Year
   */
  private void writeDateLine( final Formatter formatter, final Date actDate )
  {
    final Calendar lCal = Calendar.getInstance();
    lCal.setTime( actDate );
    formatter.format( "DY     %7d %7d %7d %7d", lCal.get( Calendar.DAY_OF_YEAR ), lCal.get( Calendar.HOUR_OF_DAY ), m_intAmountOfValues, lCal.get( Calendar.YEAR ) ); // actDate.getHours() //$NON-NLS-1$
                                                                                                                                                                      // actDate.getYear()

  }

  /**
   * 01-02 A ID "ST" 03-08 Blank 09-16 M I Coordinate number 17-32 STR11 R Surface traction in x-direction (Pascals).
   * 33-48 STR21 R Surface traction in y-direction (Pascals). 49-56 SPWP R Spectral peak wave period (secs) 57-64 AWL R
   * Average wave length (m) 65-72 SWH R Significant wave height (m) 73-80 WVDR R Wave direction measure counter
   * clockwise from the x ñaxis. This is the direction the waves are blowing to (deg)
   * real data output should be provided only for surface tractions - because the reading procedure in RMA in the fact
   * for now needs only this information.
   */
  private void writeData( final Formatter formatter, final Date actDate )
  {
    final String lStrActDatePartName = getSWANFormatedDate( actDate );
    for( int i = 0; i < m_listOriginalPositions.size(); ++i )
    {
      final String lStrFormatedDataLine = getActLine( i, lStrActDatePartName );
      formatter.format( "ST      %7d %s\n", i, lStrFormatedDataLine ); //$NON-NLS-1$
    }
  }

  private String getActLine( final int pIntIndex, final String pStrActDatePartName )
  {
    String lStrRes = ""; //$NON-NLS-1$
    GM_Position lGMPosition = m_listOriginalPositions.get( pIntIndex );
    lGMPosition = GeometryFactory.createGM_Position( lGMPosition.getX() - m_doubleShiftX, lGMPosition.getY() - m_doubleShiftY );

    for( final String lStrName : m_arrayNamesForOutput )
    {
      final Map<GM_Position, Double> lMapAct = m_mapSWANResults.get( lStrName + pStrActDatePartName );
      lStrRes += String.format( "%15f ", lMapAct.get( lGMPosition ) ); //$NON-NLS-1$
    }
    return lStrRes;
  }

  private void writeHeader( final Formatter formatter )
  {
    formatter.format( "HEADWT    A\n" ); //$NON-NLS-1$
  }

  private String getSWANFormatedDate( final Object pObjTime )
  {
    String lStrTimeRes = ""; //$NON-NLS-1$
    if( pObjTime instanceof String )
    {
      lStrTimeRes = (String)pObjTime;
    }
    else if( pObjTime instanceof Date )
    {
      SimpleDateFormat lDateFormatter;
      lDateFormatter = new SimpleDateFormat( "_yyyyMMdd_HHmmss" ); //$NON-NLS-1$

      final Date lDate = (Date)pObjTime;
      lStrTimeRes = lDateFormatter.format( lDate );
    }
    return lStrTimeRes;
  }

}
