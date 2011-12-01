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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.math.BigDecimal;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * Calculates froude from other columns.
 * 
 * @author Gernot Belger
 */
public class LengthSectionColumnFroude implements ILengthSectionColumn
{
  public final static double gravity = 9.81;

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.ILengthSectionColumn#addColumn(org.kalypso.observation.result.TupleResult)
   */
  @Override
  public void addColumn( final TupleResult result )
  {
    /* Make sure the target component exists */
    result.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_FROUDE ) );

    final int targetComponent = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_FROUDE );
    final int velocityComponent = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_V_M );
    final int areaComponent = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_F );
    final int widthComponent = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BR );

    for( final IRecord record : result )
      buildFroude( record, targetComponent, velocityComponent, areaComponent, widthComponent );
  }

  private void buildFroude( final IRecord record, final int targetComponent, final int velocityComponent, final int areaComponent, final int widthComponent )
  {
    final Object velocityValue = record.getValue( velocityComponent );
    final Object areaValue = record.getValue( areaComponent );
    final Object widthValue = record.getValue( widthComponent );

    if( velocityValue instanceof Number && areaValue instanceof Number && widthValue instanceof Number )
    {
      final double velocity = ((Number) velocityValue).doubleValue();
      final double area = ((Number) areaValue).doubleValue();
      final double width = ((Number) widthValue).doubleValue();

      /* froude = v / sqrt( g * A/b ) */
      final double divisor = gravity * area / width;
      final double froude = velocity / (Math.sqrt( divisor ));

      if( Double.isInfinite( froude ) || Double.isNaN( froude ) )
        record.setValue( targetComponent, null );
      else
        record.setValue( targetComponent, new BigDecimal( froude ) );
    }
  }

}
