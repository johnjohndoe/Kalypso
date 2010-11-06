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
package org.kalypso.model.wspm.tuhh.core.profile.buildings;

import org.kalypso.model.wspm.core.profil.AbstractProfileObject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Kim Werner
 * @author Dirk Kuch <br>
 * <br>
 *         "marker" class for tuhh buildings.
 */
public abstract class AbstractObservationBuilding extends AbstractProfileObject implements IProfileBuilding
{
  protected AbstractObservationBuilding( final IObservation<TupleResult> observation )
  {
    super( observation );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getValue(org.kalypso.observation.result.IComponent)
   */
  @Override
  public Object getValue( final IComponent component )
  {
    final TupleResult result = getObservation().getResult();
    final int index = result.indexOfComponent( component );
    if( index < 0 )
      throw new IllegalArgumentException( component == null ? getObservation().getDescription() : component.getDescription() );
    if( result.size() > 1 )
      throw new IllegalStateException( Messages.getString( "org.kalypso.model.wspm.tuhh.core.profile.buildingsAbstractObservationBuilding.0" ) ); //$NON-NLS-1$
    else if( result.size() == 0 )
      result.add( result.createRecord() );

    return result.get( 0 ).getValue( index );
  }

  public void cloneValuesFrom( final IProfileBuilding building )
  {
    for( final IComponent cmp : this.getObjectProperties() )
    {
      try
      {
        this.setValue( cmp, building.getValue(cmp ) );
      }
      catch( IllegalArgumentException e )
      {
        continue;
      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getValueFor(String)
   */
  @Override
  public Object getValueFor( final String componentID )
  {
    return getValue( getObjectProperty( componentID ) );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#setValue(org.kalypso.observation.result.IComponent,
   *      java.lang.Object)
   */
  @Override
  public void setValue( final IComponent component, final Object value )
  {
    final TupleResult result = getObservation().getResult();
    if( result.size() > 1 )
      throw new IllegalStateException( Messages.getString( "org.kalypso.model.wspm.tuhh.core.profile.buildingsAbstractObservationBuilding.1" ) ); //$NON-NLS-1$
    final int index = result.indexOfComponent( component );
    if( index < 0 )
      throw new IllegalArgumentException( component.getName() );

    final IRecord record;
    if( result.size() == 0 )
    {
      record = result.createRecord();
      result.add( record );
    }
    else
      record = result.get( 0 );
    record.setValue( index, value );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#setValueFor(String, java.lang.Object)
   */
  @Override
  public void setValueFor( final String componentID, final Object value )
  {
    setValue( getObjectProperty( componentID ), value );
  }

}
