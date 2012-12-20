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
package org.kalypso.model.hydrology.internal.postprocessing.diff;

import java.util.Date;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;

import org.kalypso.commons.diff.IDiffComparator;
import org.kalypso.commons.diff.IDiffLogger;
import org.kalypso.model.hydrology.internal.i18n.Messages;

/**
 * @author kuepfer
 */
public class BlockTimeSeriesDiffComperator implements IDiffComparator
{
  private final double m_tollerance = 1e-3d;

  @Override
  public boolean diff( final IDiffLogger logger, final Object content1, final Object content2 ) throws Exception
  {
    boolean result = false;
    final SortedMap<Date, String> timeSeries1 = (SortedMap<Date, String>) content1;
    final SortedMap<Date, String> timeSeries2 = (SortedMap<Date, String>) content2;

    logger.log( DIFF_INFO, Messages.getString( "org.kalypso.convert.namodel.timeseries.diff.BlockTimeSeriesDiffComperator.0" ) ); //$NON-NLS-1$
    final boolean valuesResult = diffValues( logger, timeSeries1, timeSeries2 );

    result |= valuesResult;

    return result;
  }

  private boolean diffValues( final IDiffLogger logger, final SortedMap<Date, String> timeSeries1, final SortedMap<Date, String> timeSeries2 )
  {
    boolean result = false;
    double differenceAll = 0;
    double sum1 = 0d;
    double sum2 = 0d;
    final int max1 = timeSeries1.size();
    final int max2 = timeSeries2.size();
    if( max1 != max2 )
    {
      logger.log( IDiffComparator.DIFF_CONTENT, Messages.getString( "org.kalypso.convert.namodel.timeseries.diff.BlockTimeSeriesDiffComperator.1", max1, max2 ) ); //$NON-NLS-1$
      return true;
    }
    double maxValue1 = 0;
    double minValue1 = 0;
    double maxValue2 = 0;
    double minValue2 = 0;
    double maxDelta = 0;
    int diffCount = 0;
    final Set<Date> set1 = timeSeries1.keySet();
    final Iterator<Date> it1 = set1.iterator();
    final Set<Date> set2 = timeSeries2.keySet();
    final Iterator<Date> it2 = set2.iterator();
    while( it1.hasNext() && it2.hasNext() )
    {
      final Object o1 = it1.next();
      final Object o2 = it2.next();
      if( o1.getClass().equals( Date.class ) && o2.getClass().equals( Date.class ) )
      {
        final Date date1 = (Date) o1;
        final Date date2 = (Date) o2;
        if( !date1.equals( date2 ) )
        {
          logger.log( IDiffComparator.DIFF_CONTENT, Messages.getString( "org.kalypso.convert.namodel.timeseries.diff.BlockTimeSeriesDiffComperator.3" ) + date1 + " : " + date2 ); //$NON-NLS-1$ //$NON-NLS-2$
          return true;
        }

        final String str1 = timeSeries1.get( date1 );
        final String str2 = timeSeries2.get( date2 );
        final double value1 = Double.parseDouble( str1 );
        final double value2 = Double.parseDouble( str2 );
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
        final double delta = Math.abs( value1 - value2 );
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
      logger.log( IDiffComparator.DIFF_CONTENT, Messages.getString( "org.kalypso.convert.namodel.timeseries.diff.BlockTimeSeriesDiffComperator.5", diffCount ) ); //$NON-NLS-1$
      logger.log( IDiffComparator.DIFF_CONTENT, Messages.getString( "org.kalypso.convert.namodel.timeseries.diff.BlockTimeSeriesDiffComperator.6", maxDelta ) ); //$NON-NLS-1$
      logger.log( IDiffComparator.DIFF_CONTENT, Messages.getString( "org.kalypso.convert.namodel.timeseries.diff.BlockTimeSeriesDiffComperator.7", differenceAll, m_tollerance ) ); //$NON-NLS-1$
    }
    if( minValue1 != minValue2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "min " + minValue1 + " : " + minValue2 ); //$NON-NLS-1$ //$NON-NLS-2$
    if( maxValue1 != maxValue2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "max " + maxValue1 + " : " + maxValue2 ); //$NON-NLS-1$ //$NON-NLS-2$
    final double mean1 = sum1 / max1;
    final double mean2 = sum2 / max2;
    if( mean1 != mean2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "mean1-mean2 = " + (mean1 - mean2) ); //$NON-NLS-1$
    else
      logger.log( IDiffComparator.DIFF_CONTENT, "mean " + mean1 ); //$NON-NLS-1$

    final double sigma1 = getSigma( mean1, timeSeries1 );
    final double sigma2 = getSigma( mean2, timeSeries2 );
    if( sigma1 != sigma2 )
      logger.log( IDiffComparator.DIFF_CONTENT, "sigma1-sigma2 = " + (sigma1 - sigma2) ); //$NON-NLS-1$
    else
      logger.log( IDiffComparator.DIFF_CONTENT, "sigma " + sigma1 ); //$NON-NLS-1$
    return result;
  }

  private double getSigma( final double mean, final SortedMap<Date, String> timeSeries )
  {
    double sum = 0d;
    final Set<Entry<Date, String>> values = timeSeries.entrySet();
    final Iterator<Entry<Date, String>> iterator = values.iterator();
    while( iterator.hasNext() )
    {
      final Entry<Date, String> e = iterator.next();
      final Object value = e.getValue();
      if( value instanceof String )
      {
        final double d = Double.parseDouble( (String) value );
        sum = sum + Math.pow( d - mean, 2 );
      }
    }
    return sum / values.size();
  }

}
