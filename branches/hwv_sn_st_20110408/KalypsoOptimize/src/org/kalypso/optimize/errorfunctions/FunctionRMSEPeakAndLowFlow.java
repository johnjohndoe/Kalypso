/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.optimize.errorfunctions;

import java.util.Date;
import java.util.Map.Entry;
import java.util.SortedMap;

/**
 * @author doemming
 */
public class FunctionRMSEPeakAndLowFlow extends IErrorFunktion
{
  public final static double UNBOUND = -1;

  private static final int STATUS_SEARCH_INTERVALL = 1;

  private static final int STATUS_INSIDE_INTERVALL = 2;

  private static final int STATUS_OUTSIDE_INTERVALL = 3;

  private final double m_min;

  private final double m_max;

  public FunctionRMSEPeakAndLowFlow( final SortedMap<Date, Double> measuredTS, final Date startCompare, final Date endCompare, final double min, final double max )
  {
    super( measuredTS, startCompare, endCompare );
    m_min = min == UNBOUND ? Double.NEGATIVE_INFINITY : min;
    m_max = max == UNBOUND ? Double.POSITIVE_INFINITY : max;
  }

  @Override
  public double calculateError( final SortedMap<Date, Double> calced )
  {
    final Date startCompare = getStartCompare();
    final Date endCompare = getEndCompare();
    final SortedMap<Date, Double> measured = getMeasuredTS();

    int status = STATUS_SEARCH_INTERVALL;
    double error = 0;
    int c = 0;

    double errorAll = 0;
    int cAll = 0;

    for( final Entry<Date, Double> entry : measured.entrySet() )
    {
      final Date dateKey = entry.getKey();
      if( !dateKey.before(  startCompare ) && !dateKey.after(  endCompare ) )
      {
        final Double measuredValue = entry.getValue();
        final Double calcedValue = calced.get( dateKey );
        if( measuredValue == null || calcedValue == null )
        {
          System.out.println( "Missing calced value for date: " + dateKey );
          continue;
        }

        final double valueMeasured = measuredValue.doubleValue();
        final double valueCalced = calcedValue.doubleValue();
        try
        {
          if( m_min <= valueCalced && valueCalced <= m_max )
          {
            // valid interval
            if( status == STATUS_OUTSIDE_INTERVALL && c != 0 )
            {
              // reentering it
              errorAll += Math.sqrt( error / c );
              cAll++;
              error = 0;
              c = 0;
            }
            error += Math.pow( valueMeasured - valueCalced, 2 );
            c++;
            status = STATUS_INSIDE_INTERVALL;
          }
          else
            status = STATUS_OUTSIDE_INTERVALL;
        }
        catch( final Exception e )
        {
          status = STATUS_OUTSIDE_INTERVALL;
// e.printStackTrace();
        }
      }
    }
    errorAll += Math.sqrt( error / c );
    cAll++;
    return errorAll / cAll + getNormalizeOffset();
  }
}