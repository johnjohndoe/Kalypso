/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.math.BigDecimal;

import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
abstract class AbstractLengthSectionColumn implements ILengthSectionColumn
{
  private final String m_targetComponentID;

  public AbstractLengthSectionColumn( final String targetComponentID )
  {
    m_targetComponentID = targetComponentID;
  }

  @Override
  public final void addColumn( final TupleResult result )
  {
    /* Make sure the target component exists */
    final IComponent targetComponent = ProfileUtil.getFeatureComponent( m_targetComponentID );
    result.addComponent( targetComponent );

    final int scale = ComponentUtilities.getScale( targetComponent );

    final int targetComponentIndex = result.indexOfComponent( targetComponent );

    for( final IRecord record : result )
    {
      final double rawValue = calculateValue( record );

      final BigDecimal resultValue = buildResultValue( rawValue, scale );
      record.setValue( targetComponentIndex, resultValue );
    }
  }

  protected abstract double calculateValue( IRecord record );

  private BigDecimal buildResultValue( final double rawValue, final int scale )
  {
    if( Double.isInfinite( rawValue ) || Double.isNaN( rawValue ) )
      return null;

    final BigDecimal decimalValue = new BigDecimal( rawValue );
    if( scale == -1 )
      return decimalValue;

    return decimalValue.setScale( scale, BigDecimal.ROUND_HALF_UP );
  }
}