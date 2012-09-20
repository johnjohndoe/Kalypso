/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.math.BigDecimal;

import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * Divides one columns by another.
 * 
 * @author Gernot Belger
 */
public class LengthSectionColumnDivide implements ILengthSectionColumn
{
  private final String m_componentTarget;

  private final String m_componentDivident;

  private final String m_componentDivisor;

  public LengthSectionColumnDivide( final String componentTarget, final String componentDivident, final String componentDivisor )
  {
    m_componentTarget = componentTarget;
    m_componentDivident = componentDivident;
    m_componentDivisor = componentDivisor;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.ILengthSectionColumn#addColumn(org.kalypso.observation.result.TupleResult)
   */
  @Override
  public void addColumn( final TupleResult result )
  {
    /* Make sure the target component exists */
    result.addComponent( ProfileUtil.getFeatureComponent( m_componentTarget ) );

    final int targetComponent = result.indexOfComponent( m_componentTarget );

    final int dividentComponent = result.indexOfComponent( m_componentDivident );
    final int divisorComponent = result.indexOfComponent( m_componentDivisor );

    for( final IRecord record : result )
      divideValue( record, targetComponent, dividentComponent, divisorComponent );
  }

  private void divideValue( final IRecord record, final int targetComponent, final int dividentComponent, final int divisorComponent )
  {
    final Object dividentValue = record.getValue( dividentComponent );
    final Object divisorValue = record.getValue( divisorComponent );

    if( dividentValue instanceof Number && divisorValue instanceof Number )
    {
      final double divident = ((Number) dividentValue).doubleValue();
      final double divisor = ((Number) divisorValue).doubleValue();
      final double quotient = divident / divisor;

      if( Double.isInfinite( quotient ) || Double.isNaN( quotient ) )
        record.setValue( targetComponent, null );
      else
        record.setValue( targetComponent, new BigDecimal( quotient ) );
    }

  }
}
