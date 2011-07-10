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
package org.kalypso.model.wspm.pdb.connect.command;

import java.lang.reflect.Array;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;

/**
 * @author Gernot Belger
 */
public final class GetPdbList<T> implements IPdbOperation
{
  private List<T> m_result;

  private final Class<T> m_type;

  public GetPdbList( final Class<T> type )
  {
    m_type = type;
  }

  @Override
  public String getLabel( )
  {
    return "Get elements";
  }

  @SuppressWarnings("unchecked")
  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    try
    {
      final Criteria criteria = session.createCriteria( m_type );
      m_result = criteria.list();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();

      final String message = String.format( "Failed to retreive: %s", m_type.getName() );
      throw new PdbConnectException( message, e );
    }
  }

  public List<T> getResult( )
  {
    return m_result;
  }

  public static <T> List<T> getList( final Session session, final Class<T> type ) throws PdbConnectException
  {
    final GetPdbList<T> operation = new GetPdbList<T>( type );
    operation.execute( session );
    return operation.getResult();
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] getArray( final Session session, final Class<T> type ) throws PdbConnectException
  {
    final List<T> list = getList( session, type );
    return list.toArray( (T[]) Array.newInstance( type, list.size() ) );
  }

  @SuppressWarnings("unchecked")
  public T[] getResultAsArray( )
  {
    return m_result.toArray( (T[]) Array.newInstance( m_type, m_result.size() ) );
  }
}