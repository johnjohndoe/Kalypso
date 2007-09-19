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
package org.kalypso.model.wspm.core.util.pointpropertycalculator;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;

/**
 * @author kimwerner
 */
public class AddPercentValueOperator implements IPointPropertyCalculator
{

  /**
   * @see org.kalypso.model.wspm.core.util.pointpropertycalculator.IPointPropertyCalculator#calculate(java.lang.Double,
   *      java.lang.String[], java.util.List)
   */
  public IProfilChange[] calculate( Double operand, String[] properties, List<IProfilPoint> points )
  {
    final List<IProfilChange> changes = new ArrayList<IProfilChange>();
    for( final IProfilPoint point : points )
    {
      for( final String property : properties )
      {
        final Double oldValue = point.getValueFor( property );
        final double newValue = oldValue * operand;
        changes.add( new PointPropertyEdit( point, property, newValue ) );
      }
    }
    return changes.toArray( new IProfilChange[changes.size()] );
  }

}
