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
package org.kalypso.model.wspm.pdb.connect.command;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;

/**
 * @author Gernot Belger
 * @deprecated
 */
@Deprecated
public class ListOperation<T> implements IPdbOperation
{
  private final Class<T> m_type;

  private List<T> m_list;

  public ListOperation( final Class<T> type )
  {
    m_type = type;
  }

  @Override
  public String getLabel( )
  {
    return "Get List";
  }

  @SuppressWarnings("unchecked")
  @Override
  public void execute( final Session session )
  {
    final String query = String.format( "from %s", m_type.getName() );
    final Query q = session.createQuery( query );
    m_list = q.list();
  }

  public List<T> getList( )
  {
    return m_list;
  }
}
