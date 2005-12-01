/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
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
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.sensor.zml.diff;

import java.io.InputStream;
import java.util.Date;

import org.kalypso.commons.diff.IDiffComparator;
import org.kalypso.commons.diff.IDiffLogger;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.xml.sax.InputSource;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class ZMLDiffComparator implements IDiffComparator
{
  public ZMLDiffComparator()
  {}

  private double m_tollerance = 0.01;

  /**
   * 
   * @see org.kalypso.commons.diff.IDiffComparator#diff(org.kalypso.commons.diff.IDiffLogger, java.lang.Object,
   *      java.lang.Object)
   */
  public boolean diff( IDiffLogger logger, Object content, Object content2 ) throws Exception
  {
    boolean result = false;
    InputStream i1 = (InputStream)content;
    InputStream i2 = (InputStream)content2;
    IObservation obs1 = ZmlFactory.parseXML( new InputSource( i1 ), "1", null );
    IObservation obs2 = ZmlFactory.parseXML( new InputSource( i2 ), "2", null );
    //    result |= diffMetadata( logger, obs1.getMetadataList(), obs2.getMetadataList() );
    //    result |= diffAxis( logger, obs1.getAxisList(), obs2.getAxisList() );
    logger.block();
    logger.log( DIFF_INFO, "Wertevergleich" );
    boolean valuesResult = diffValues( logger, obs1, obs2 );
    logger.unblock( valuesResult );

    result |= valuesResult;

    return result;
  }

  /**
   * @param logger
   * @param obs1
   * @param obs2
   * @throws SensorException
   */
  private boolean diffValues( IDiffLogger logger, IObservation obs1, IObservation obs2 ) throws SensorException
  {
    boolean result = false;
    double differenceAll = 0;
    final IAxis[] axes1 = obs1.getAxisList();
    final IAxis[] axes2 = obs2.getAxisList();
    final IAxis dateAxis1 = ObservationUtilities.findAxisByClass( axes1, Date.class );
    final IAxis dateAxis2 = ObservationUtilities.findAxisByClass( axes2, Date.class );
    final IAxis valueAxis1 = ObservationUtilities.findAxisByClass( axes1, Double.class );
    final IAxis valueAxis2 = ObservationUtilities.findAxisByClass( axes2, Double.class );

    final ITuppleModel values1 = obs1.getValues( null );
    final ITuppleModel values2 = obs2.getValues( null );
    final int max1 = values1.getCount();
    final int max2 = values2.getCount();
    if( max1 != max2 )
    {
      logger.log( IDiffComparator.DIFF_CONTENT, "Anzahl der Werte : " + max1 + " : " + max2 );
      return true;
    }
    final double v1 = ( (Double)values1.getElement( 0, valueAxis1 ) ).doubleValue();
    final double v2 = ( (Double)values2.getElement( 0, valueAxis2 ) ).doubleValue();
    double maxValue1 = v1;
    double minValue1 = v1;
    double maxValue2 = v2;
    double minValue2 = v2;
    double maxDelta = 0;
    int diffCount = 0;
    for( int i = 0; i < max1; i++ )
    {
      final Date date1 = (Date)values1.getElement( i, dateAxis1 );
      final Date date2 = (Date)values2.getElement( i, dateAxis2 );
      if( !date1.equals( date2 ) )
      {
        logger.log( IDiffComparator.DIFF_CONTENT, "Datum  " + date1 + " : " + date2 );
        return true;
      }
      final double value1 = ( (Double)values1.getElement( i, valueAxis1 ) ).doubleValue();
      final double value2 = ( (Double)values2.getElement( i, valueAxis2 ) ).doubleValue();
      if( value1 > maxValue1 )
        maxValue1 = value1;
      if( value2 > maxValue2 )
        maxValue2 = value2;
      if( value1 < minValue1 )
        minValue1 = value1;
      if( value2 < minValue2 )
        minValue2 = value2;
      double delta = Math.abs( value1 - value2 );
      if( delta > m_tollerance )
      {
        differenceAll += delta;
        result = true;
        if( delta > maxDelta )
          maxDelta = delta;
        diffCount++;
      }
    }
    if( result )
    {
      logger.log( IDiffComparator.DIFF_CONTENT, "Anzahl Überschreitungen : #" + diffCount );
      logger.log( IDiffComparator.DIFF_CONTENT, "maximale Überschreitung : " + maxDelta );
      logger.log( IDiffComparator.DIFF_CONTENT, "Summe Überscheitungen " + differenceAll + " (delta >" + m_tollerance
          + ")" );
    }
    if( minValue1 != minValue2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "min " + minValue1 + " : " + minValue2 );
    if( maxValue1 != maxValue2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "max " + maxValue1 + " : " + maxValue2 );
    return result;
  }

  //  /**
  //   * @param logger
  //   * @param axisList
  //   * @param axisList2
  //   */
  //  private boolean diffAxis( IDiffLogger logger, IAxis[] axisList, IAxis[] axisList2 )
  //  {
  //    final List list1 = new ArrayList();
  //    final List list2 = new ArrayList();
  //    for( int i = 0; i < axisList.length; i++ )
  //      list1.add( axisList[i].getName() + ":" + axisList[i].getType() + ":" + axisList[i].getUnit() + ":"
  //          + axisList[i].getClass().getName() );
  //    for( int i = 0; i < axisList2.length; i++ )
  //      list2.add( axisList2[i].getName() + ":" + axisList2[i].getType() + ":" + axisList2[i].getUnit() + ":"
  //          + axisList2[i].getClass().getName() );
  //    return DiffUtils.diffIgnoreOrder( logger, list1, list2, " Axen " );
  //  }

  //  /**
  //   *
  //   * @param logger
  //   * @param metadataList
  //   * @param metadataList2
  //   * @return diff
  //   */
  //  private boolean diffMetadata( IDiffLogger logger, MetadataList metadataList, MetadataList metadataList2 )
  //  {
  //    List list1 = new ArrayList();
  //    List list2 = new ArrayList();
  //    for( Iterator iter = metadataList.keySet().iterator(); iter.hasNext(); )
  //    {
  //      final String key = (String)iter.next();
  //      final String property = metadataList.getProperty( key );
  //      list1.add( key + " = " + property );
  //    }
  //    for( Iterator iter = metadataList2.keySet().iterator(); iter.hasNext(); )
  //    {
  //      final String key = (String)iter.next();
  //      final String property = metadataList2.getProperty( key );
  //      list2.add( key + " = " + property );
  //    }
  //    return DiffUtils.diffIgnoreOrder( logger, list1, list2, "Metadaten" );
  //  }
}
