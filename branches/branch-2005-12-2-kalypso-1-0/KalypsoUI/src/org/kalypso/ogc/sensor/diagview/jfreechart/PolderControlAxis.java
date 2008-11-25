/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.text.DecimalFormat;

import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.axis.TickUnits;
import org.jfree.data.Range;

/**
 * A 'boolean' axis. IN reality this is still a number axis, but renders 'true' for value bigger than 0.5 and 'false'
 * for values smaller than 0.5; works with the XYCurveSerie that returns 1 for <code>true</code> and 0 for <code>false</code>.
 * 
 * @author Gernot Belger
 */
public class PolderControlAxis extends NumberAxis
{
  public PolderControlAxis( final String label )
  {
    super( label );

    final TickUnitSource tickUnits = createBooleanTickUnits();
    setStandardTickUnits( tickUnits );
  }

  protected void autoAdjustRange()
  {
    setRange( new Range( 0.0, 3 ), true, true );
  }
  
  /**
   * Returns a collection of tick units for boolean values (i.e. only 0 and 1).
   * 
   * @return a collection of tick units for integer values.
   */
  public static TickUnitSource createBooleanTickUnits()
  {
    final TickUnits units = new TickUnits();

    final NumberTickUnit unit = new NumberTickUnit( 1, new DecimalFormat( "0" ) )
    {
      /**
       * @see org.jfree.chart.axis.NumberTickUnit#valueToString(double)
       */
      public String valueToString( double value )
      {
        if( value > 1.5 )
          return "";
        
        if( value < 0.5 )
          return "zu";

        return "auf";
      }
    };

    units.add( unit );

    return units;
  }
}
