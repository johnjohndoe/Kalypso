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
package org.kalypso.convert.namodel.timeseries.diff;

import java.util.Date;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.kalypso.commons.diff.IDiffComparator;
import org.kalypso.commons.diff.IDiffLogger;

/**
 * @author kuepfer
 */
public class BlockTimeSeriesDiffComperator implements IDiffComparator
{

  private double m_tollerance = 1e-3d;

  public BlockTimeSeriesDiffComperator( )
  {
  }

  /**
   * @see org.kalypso.commons.diff.IDiffComparator#diff(org.kalypso.commons.diff.IDiffLogger, java.lang.Object,
   *      java.lang.Object)
   */
  public boolean diff( IDiffLogger logger, Object content1, Object content2 ) throws Exception
  {
    boolean result = false;
    TreeMap timeSeries1 = ((TreeMap) content1);
    TreeMap timeSeries2 = ((TreeMap) content2);

    logger.log( DIFF_INFO, "Wertevergleich" );
    boolean valuesResult = diffValues( logger, timeSeries1, timeSeries2 );

    result |= valuesResult;

    return result;
  }

  private boolean diffValues( IDiffLogger logger, TreeMap timeSeries1, TreeMap timeSeries2 )
  {
    boolean result = false;
    double differenceAll = 0;
    double sum1 = 0d;
    double sum2 = 0d;
    int max1 = timeSeries1.size();
    int max2 = timeSeries2.size();
    if( max1 != max2 )
    {
      logger.log( IDiffComparator.DIFF_CONTENT, "Anzahl der Werte : " + max1 + " : " + max2 );
      return true;
    }
    double maxValue1 = 0;
    double minValue1 = 0;
    double maxValue2 = 0;
    double minValue2 = 0;
    double maxDelta = 0;
    int diffCount = 0;
    final Set set1 = timeSeries1.keySet();
    final Iterator it1 = set1.iterator();
    final Set set2 = timeSeries2.keySet();
    final Iterator it2 = set2.iterator();
    while( it1.hasNext() && it2.hasNext() )
    {
      Object o1 = it1.next();
      Object o2 = it2.next();
      if( o1.getClass().equals( Date.class ) && o2.getClass().equals( Date.class ) )
      {
        final Date date1 = ((Date) o1);
        final Date date2 = ((Date) o2);
        if( !date1.equals( date2 ) )
        {
          logger.log( IDiffComparator.DIFF_CONTENT, "Datum  " + date1 + " : " + date2 );
          return true;
        }

        String str1 = (String) timeSeries1.get( date1 );
        String str2 = (String) timeSeries2.get( date2 );
        double value1 = Double.parseDouble( str1 );
        double value2 = Double.parseDouble( str2 );
        sum1 = sum1 + value1;
        sum2 = sum2 + value2;
        maxValue1 = value1;
        maxValue2 = value2;
        minValue1 = value1;
        minValue2 = value2;
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
    }
    if( result )
    {
      logger.log( IDiffComparator.DIFF_CONTENT, "Anzahl Überschreitungen : #" + diffCount );
      logger.log( IDiffComparator.DIFF_CONTENT, "maximale Überschreitung : " + maxDelta );
      logger.log( IDiffComparator.DIFF_CONTENT, "Summe Überschreitungen " + differenceAll + " (delta >" + m_tollerance + ")" );
    }
    if( minValue1 != minValue2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "min " + minValue1 + " : " + minValue2 );
    if( maxValue1 != maxValue2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "max " + maxValue1 + " : " + maxValue2 );
    double mean1 = sum1 / max1;
    double mean2 = sum2 / max2;
    if( mean1 != mean2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "mean1-mean2 = " + (mean1 - mean2) );
    else
      logger.log( IDiffComparator.DIFF_CONTENT, "mean " + mean1 );

    double sigma1 = getSigma( mean1, timeSeries1 );
    double sigma2 = getSigma( mean2, timeSeries2 );
    if( sigma1 != sigma2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "sigma1-sigma2 = " + (sigma1 - sigma2) );
    else
      logger.log( IDiffComparator.DIFF_CONTENT, "sigma " + sigma1 );
    return result;
  }

  private double getSigma( double mean, TreeMap timeSeries )
  {
    double sum = 0d;
    final Set values = timeSeries.entrySet();
    final Iterator iterator = values.iterator();
    while( iterator.hasNext() )
    {
      Entry e = (Entry) iterator.next();
      Object value = e.getValue();
      if( value instanceof String )
      {
        double d = Double.parseDouble( (String) value );
        sum = sum + Math.pow( d - mean, 2 );
      }
    }
    return sum / values.size();
  }

}
