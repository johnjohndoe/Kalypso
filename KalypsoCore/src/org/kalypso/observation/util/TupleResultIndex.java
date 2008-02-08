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
package org.kalypso.observation.util;

import java.util.Date;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.ITupleResultChangedListener.ValueChange;

/**
 * @author Gernot Belger
 */
public class TupleResultIndex
{
  private final ITupleResultChangedListener m_changeListener = new ITupleResultChangedListener()
  {
    public void componentsChanged( IComponent[] components, TYPE type )
    {
      handleComponentsChanged( components );
    }

    public void recordsChanged( final IRecord[] records, final TYPE type )
    {
      handleRecordsChanged( records, type );
    }

    public void valuesChanged( ITupleResultChangedListener.ValueChange[] changes )
    {
      handleValuesChanged( changes );
    }
  };

  private final IComponent m_component;

  private final TupleResult m_result;

  private SortedMap<Object, IRecord> m_index = null;

  public TupleResultIndex( final TupleResult result, final IComponent component )
  {
    m_result = result;
    m_component = component;

    result.addChangeListener( m_changeListener );
  }

  public void dispose( )
  {
    m_result.removeChangeListener( m_changeListener );
    m_index = null;
  }

  private void checkIndex( )
  {
    if( m_index != null )
      return;

    m_index = new TreeMap<Object, IRecord>( m_component );

    for( final IRecord record : m_result )
    {
      final Object value = record.getValue( m_component );
      m_index.put( value, record );
    }
  }

  /**
   * returns the record defined by the domain, if none is found null is returned.
   */
  public synchronized IRecord getRecord( final Object domain )
  {
    checkIndex();

    if( m_index.containsKey( domain ) )
      return m_index.get( domain );
    else
      return null;
  }

  /**
   * TODO: move into helper class<br>
   * 
   * @return an interpolated object based in the neighboring objects before and after the given domain
   */
  public synchronized Object getValue( final IComponent component, final Date date )
  {
    checkIndex();

    XMLGregorianCalendar domain = DateUtilities.toXMLGregorianCalendar( date );

    if( m_index.containsKey( domain ) )
      return m_index.get( domain ).getValue( component );

    final SortedMap<Object, IRecord> head = m_index.headMap( domain );
    if( head.isEmpty() )
      return null;

    final SortedMap<Object, IRecord> tail = m_index.tailMap( domain );
    if( tail.isEmpty() )
      return null;

    final Object domainBefore = head.lastKey();
    final Object domainAfter = tail.firstKey();

    final Object valueBefore = head.get( domainBefore ).getValue( component );
    final Object valueAfter = tail.get( domainAfter ).getValue( component );

    /* Linear interpolation */
    final long before = ((XMLGregorianCalendar) domainBefore).toGregorianCalendar().getTimeInMillis();
    final long after = ((XMLGregorianCalendar) domainAfter).toGregorianCalendar().getTimeInMillis();

    final long dom = date.getTime();

    final double valBefore = ((Number) valueBefore).doubleValue();
    final double valAfter = ((Number) valueAfter).doubleValue();

    try
    {
      final LinearEquation equation = new LinearEquation( before, valBefore, after, valAfter );
      return equation.computeY( dom );
    }
    catch( final SameXValuesException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  public Iterator<IRecord> getIterator( )
  {
    checkIndex();

    return m_index.values().iterator();
  }

  protected void handleComponentsChanged( final IComponent[] components )
  {
    if( m_index == null )
      return;

    for( final IComponent component : components )
    {
      if( component.equals( m_component ) )
        m_index = null;
    }
  }

  protected void handleRecordsChanged( final IRecord[] records, final ITupleResultChangedListener.TYPE type )
  {
    if( m_index == null )
      return;

    if( records == null )
    {
      m_index = null;
      return;
    }

    switch( type )
    {
      case ADDED:
      {
        for( final IRecord record : records )
          m_index.put( record.getValue( m_component ), record );
        return;
      }

      case CHANGED:
        // TODO: check: we need the old record value, but we do not have it...

        for( final IRecord record : records )
          m_index.put( record.getValue( m_component ), record );
        break;

      case REMOVED:
      {
        for( final IRecord record : records )
          m_index.remove( record.getValue( m_component ) );
        return;
      }

      default:
        break;
    }
  }

  protected void handleValuesChanged( final ITupleResultChangedListener.ValueChange[] changes )
  {
    if( m_index == null )
      return;

    for( int i = 0; i < changes.length; i++ )
    {
      final ValueChange change = changes[i];

      final int index = change.getComponent();
      final IComponent component = m_result.getComponent( index );
      if( component.equals( m_component ) )
      {
        m_index.remove( change.getOldValue() );
        m_index.put( change.getNewValue(), change.getRecord() );
      }
    }
  }
}
