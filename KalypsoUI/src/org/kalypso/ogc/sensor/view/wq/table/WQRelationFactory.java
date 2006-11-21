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

package org.kalypso.ogc.sensor.view.wq.table;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.table.TableModel;

import org.kalypso.commons.math.Spline;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQPair;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;

/**
 * @author schlienger
 */
public class WQRelationFactory
{
  private WQRelationFactory()
  {
  // empty
  }

  public static TableModel createTableModel( final WQTable table )
  {
    // build spline
    final WQPair[] pairs = table.getPairs();
    final double[] Q = new double[ pairs.length ];
    final double[] W = new double[ pairs.length ];
    
    for( int i = 0; i < pairs.length; i++ )
    {
      W[i] = pairs[i].getW();
      Q[i] = pairs[i].getQ();
    }
    
    final Spline spline = new Spline( W, Q );
    
    // sort W
    final double[] sortedW = (double[])W.clone();
    Arrays.sort( sortedW );
    
    // compute Q
    final List resQ = new ArrayList();
    
    for( double w = sortedW[0]; w <= sortedW[sortedW.length - 1]; w++ )
    {
      final double q = spline.eval( w );
      
      resQ.add( new Double( q ) );
    }
    
    return new WQTableModel( new Double(sortedW[0]), (Double[])resQ.toArray( new Double[resQ.size()] ) );
  }
}
