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
package org.kalypso.observation.result;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.observation.result.ITupleResultChangedListener.TYPE;
import org.kalypso.observation.result.ITupleResultChangedListener.ValueChange;

/**
 * @author schlienger Default visibility, use IRecord and TupleResult.createRecord.
 */
public class Record implements IRecord
{
  private final Map<IComponent, Object> m_values = new HashMap<IComponent, Object>();

  private final TupleResult m_owner;

  public Record( final TupleResult result, final Set<IComponent> components )
  {
    m_owner = result;

    for( final IComponent component : components )
      m_values.put( component, component.getDefaultValue() );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_values.values().toString();
  }

  /**
   * @see org.kalypso.om.tuple.IRecord#getValue(org.kalypso.om.tuple.IComponent)
   */
  public Object getValue( final IComponent comp )
  {
    checkComponent( comp );

    return m_values.get( comp );
  }

  private void checkComponent( final IComponent comp )
  {
    if( !m_owner.hasComponent( comp ) )
      throw new IllegalArgumentException( "Unknown component: " + comp );
  }

  /**
   * @see org.kalypso.om.tuple.IRecord#setValue(org.kalypso.om.tuple.IComponent, java.lang.Object)
   */
  public void setValue( final IComponent comp, final Object value )
  {
    checkComponent( comp );

    final Object oldValue = m_values.get( comp );
    if( ObjectUtils.equals( value, oldValue ) )
      return;

    m_values.put( comp, value );

    if( m_owner != null )
    {
      if( m_owner.invalidateSort( comp ) )
        m_owner.fireRecordsChanged( null, TYPE.CHANGED );
      else
      {
        final ValueChange[] changes = new ValueChange[] { new ValueChange( this, comp, value ) };
        m_owner.fireValuesChanged( changes );
      }
    }

  }

  /* default */void remove( final IComponent comp )
  {
    m_values.remove( comp );
  }

  public void checkComponents( final Set<IComponent> components )
  {
    // check, if i have too much components
    final Set<IComponent> keySet = m_values.keySet();
    for( final IComponent component : keySet )
    {
      if( !components.contains( component ) )
        throw new IllegalArgumentException( "Illegal record: Unknown component: " + component );
    }

    // check if i need a new component (i.e. set default value)
    for( final IComponent component : components )
    {
      if( !keySet.contains( component ) )
        m_values.put( component, component.getDefaultValue() );
    }
  }

  /**
   * @see org.kalypso.observation.result.IRecord#getOwner()
   */
  public TupleResult getOwner( )
  {
    return m_owner;
  }

  /**
   * @see org.kalypso.observation.result.IRecord#cloneRecord()
   */
  public IRecord cloneRecord( )
  {
    final TupleResult result = this.getOwner();
    final IComponent[] components = result.getComponents();
    final Set<IComponent> sComp = new LinkedHashSet<IComponent>();

    for( final IComponent component : components )
    {
      sComp.add( component );
    }

    final Record record = new Record( result, sComp );
    for( final IComponent component : components )
    {
      record.setValue( component, this.getValue( component ) );
    }

    return record;
  }
}
