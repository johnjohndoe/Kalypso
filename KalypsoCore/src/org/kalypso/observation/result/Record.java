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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.ObjectUtils;
import org.kalypso.core.i18n.Messages;
import org.kalypso.observation.result.ITupleResultChangedListener.TYPE;
import org.kalypso.observation.result.ITupleResultChangedListener.ValueChange;

/**
 * Default visibility: do NOT use outside of TupleResult.
 * 
 * @author schlienger Default visibility, use IRecord and TupleResult.createRecord.
 */
/* default */class Record implements IRecord
{
  private final List<Object> m_values = new ArrayList<Object>();

  private final TupleResult m_owner;

  Record( final TupleResult result, final IComponent[] components )
  {
    m_owner = result;

    for( final IComponent component : components )
      m_values.add( component.getDefaultValue() );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return ArrayUtils.toString( m_values );
  }

  /**
   * @see org.kalypso.om.tuple.IRecord#getValue(org.kalypso.om.tuple.IComponent)
   */
  @Deprecated
  public Object getValue( final IComponent comp )
  {
    final int index = checkComponent( comp );
    return getValue( index );
  }

  /**
   * @see org.kalypso.observation.result.IRecord#getValue(int)
   */
  public Object getValue( final int index ) throws IndexOutOfBoundsException
  {
    return m_values.get( index );
  }

  private int checkComponent( final IComponent comp )
  {
    final int index = m_owner.indexOfComponent( comp );
    if( index == -1 )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.observation.result.Record.0") + comp ); //$NON-NLS-1$

    return index;
  }

  /**
   * @see org.kalypso.om.tuple.IRecord#setValue(org.kalypso.om.tuple.IComponent, java.lang.Object)
   */
  @Deprecated
  public void setValue( final IComponent comp, final Object value )
  {
    final int index = checkComponent( comp );
    setValue( index, value );
  }

  /**
   * @see org.kalypso.observation.result.IRecord#setValue(int, java.lang.Object)
   */
  public void setValue( final int index, final Object value ) throws IndexOutOfBoundsException
  {
    final Object oldValue = m_values.get( index );
    if( ObjectUtils.equals( value, oldValue ) )
      return;

    m_values.set( index, value );

    if( m_owner != null )
    {
      if( m_owner.invalidateSort( index ) )
        m_owner.fireRecordsChanged( null, TYPE.CHANGED );
      else
      {
        final ValueChange[] changes = new ValueChange[] { new ValueChange( this, index, oldValue, value ) };
        m_owner.fireValuesChanged( changes );
      }
    }
  }

  /* default */void remove( final int index )
  {
    m_values.remove( index );
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
    final TupleResult result = getOwner();
    final IComponent[] components = result.getComponents();

    final Record record = new Record( result, components );
    for( final IComponent component : components )
      record.setValue( component, getValue( component ) );

    return record;
  }

  /**
   * sets a value of an given index - index doesn't exists (new value end of list) -> index will be created
   */
  /* default */void set( final int index, final Object value )
  {
    if( m_values.size() == index )
    {
      m_values.add( value );
    }
    else
    {
      // Might throw IndexOutOfBoundsException..
      m_values.set( index, value );
    }
  }
}
