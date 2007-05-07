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
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
public class TupleResultIndex
{
  private SortedMap<Object, IRecord> m_index;

  public TupleResultIndex( final TupleResult result, final IComponent component )
  {
    m_index = createIndex( result, component );
  }

  private SortedMap<Object, IRecord> createIndex( final TupleResult result, final IComponent component )
  {
    final SortedMap<Object, IRecord> map = new TreeMap<Object, IRecord>();

    for( final IRecord record : result )
    {
      final Object value = result.getValue( record, component );

      // HACK: convert xml-gregorian calendars to dates
      if( value instanceof XMLGregorianCalendar )
        map.put( DateUtilities.toDate( (XMLGregorianCalendar) value ), record );
      else
        map.put( value, record );
    }

    return map;
  }

  public Object getValue( final IComponent component, final Object domain )
  {
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
    // TODO: should be independent of involved types...
    // now we know, that we have times and doubles
    final long before = ((Date) domainBefore).getTime();
    final long after = ((Date) domainAfter).getTime();

    final long dom = ((Date) domain).getTime();

    final double valBefore = ((Number) valueBefore).doubleValue();
    final double valAfter = ((Number) valueAfter).doubleValue();

    try
    {
      final LinearEquation equation = new LinearEquation( before, valBefore, after, valAfter );
      return equation.computeY( dom );
    }
    catch( final SameXValuesException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();

      return null;
    }
  }

  public Iterator<IRecord> getIterator( )
  {
    return m_index.values().iterator();
  }

}
