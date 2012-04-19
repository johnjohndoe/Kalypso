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
package org.kalypso.model.wspm.tuhh.core.profile.energyloss;

import java.math.BigDecimal;

import org.kalypso.model.wspm.core.profil.AbstractProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class EnergylossProfileObject extends AbstractProfileObject implements IEnergylossProfileObject
{
  public EnergylossProfileObject( )
  {
    this( buildObservation() );
  }

  private static IObservation<TupleResult> buildObservation( )
  {
    final TupleResult loss = new TupleResult();
    loss.addComponent( getObjectComponent( IEnergylossProfileObject.PROPERTY_TYPE ) );
    loss.addComponent( getObjectComponent( IEnergylossProfileObject.PROPERTY_DESCRIPTION ) );
    loss.addComponent( getObjectComponent( IEnergylossProfileObject.PROPERTY_VALUE ) );
    final Observation<TupleResult> observation = new Observation<TupleResult>( IEnergylossProfileObject.ID, "energyloss", loss ); //$NON-NLS-1$
    return observation;
  }

  public EnergylossProfileObject( final IObservation<TupleResult> observation )
  {
    super( observation );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getId()
   */
  @Override
  public String getId( )
  {
    return IEnergylossProfileObject.ID;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.AbstractProfileObject#getProfileProperties()
   */
  @Override
  protected String[] getProfileProperties( )
  {
    return new String[] {};
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject#getValue(java.lang.String)
   */
  @Override
  public BigDecimal getValue( final int index )
  {
    final TupleResult result = getObservation().getResult();
    if( result == null || result.size() <= index )
    {
      return null;
    }
    final int iValue = result.indexOfComponent( IEnergylossProfileObject.PROPERTY_VALUE );
    return new BigDecimal( (Double) result.get( index ).getValue( iValue ) );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject#getValue(org.kalypso.model.wspm.tuhh.core.profile.energyloss.ENERGYLOSS_TYPE)
   */
  @Override
  public String getType( final int index )
  {
    final TupleResult result = getObservation().getResult();
    if( result == null || result.size() <= index )
    {
      return null;
    }
    final int iType = result.indexOfComponent( IEnergylossProfileObject.PROPERTY_TYPE );
    return result.get( index ).getValue( iType ).toString();
  }
}
