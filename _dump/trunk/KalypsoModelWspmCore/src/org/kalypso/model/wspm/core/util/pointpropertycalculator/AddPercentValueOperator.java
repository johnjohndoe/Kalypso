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
package org.kalypso.model.wspm.core.util.pointpropertycalculator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class AddPercentValueOperator implements IPointPropertyCalculator
{

  /**
   * @see org.kalypso.model.wspm.core.util.pointpropertycalculator.IPointPropertyCalculator#calculate(java.lang.Double,
   *      java.lang.String[], java.util.List)
   */
  public IProfilChange[] calculate( final Double operand, final IComponent[] properties, final List<IRecord> points )
  {
    final List<IProfilChange> changes = new ArrayList<IProfilChange>();
    for( final IRecord point : points )
    {
      for( final IComponent property : properties )
      {
        final TupleResult result = point.getOwner();

        if( result.hasComponent( property ) )
        {
          final Double oldValue = (Double) point.getValue( property );
          final double newValue = oldValue * operand;
          changes.add( new PointPropertyEdit( point, property, newValue ) );
        }
        else
          KalypsoModelWspmCorePlugin.getDefault().getLog().log( new Status( Status.CANCEL, KalypsoModelWspmCorePlugin.getID(), property + " existiert nicht für Punkt[" + point.toString() + "]" ) );
      }
    }
    return changes.toArray( new IProfilChange[changes.size()] );
  }

}
