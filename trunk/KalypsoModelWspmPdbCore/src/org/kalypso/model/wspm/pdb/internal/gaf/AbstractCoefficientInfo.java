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
package org.kalypso.model.wspm.pdb.internal.gaf;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.model.wspm.pdb.db.mapping.Coefficient;
import org.kalypso.model.wspm.pdb.db.mapping.CoefficientId;

/**
 * @author Gernot Belger
 */
public class AbstractCoefficientInfo<T extends Coefficient>
{
  private final Map<String, T> m_definitions = new HashMap<>();

  private final Class<T> m_type;

  public AbstractCoefficientInfo( final Session session, final Class<T> type, final String kind )
  {
    m_type = type;

    final T[] allData = loadDefinition( session, kind );
    hashDefinitions( allData );
  }

  private T[] loadDefinition( final Session session, final String kind )
  {
    final Criteria criteria = session.createCriteria( m_type );
    criteria.add( Restrictions.eq( "id.pointKind", kind ) ); //$NON-NLS-1$

    final List<T> list = criteria.list();
    return Arrays.toArray( list, m_type );
  }

  private void hashDefinitions( final T[] allData )
  {
    for( final T definition : allData )
    {
      final CoefficientId id = definition.getId();
      final String name = id.getName();
      m_definitions.put( name, definition );
    }
  }

  public T getCoefficient( final String name )
  {
    return m_definitions.get( name );
  }

  public T[] getAllCoefficients( )
  {
    final Collection<T> values = m_definitions.values();

    return values.toArray( (T[]) Array.newInstance( m_type, values.size() ) );
  }
}