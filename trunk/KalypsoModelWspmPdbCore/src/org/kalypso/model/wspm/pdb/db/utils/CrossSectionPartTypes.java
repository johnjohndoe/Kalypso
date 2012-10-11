/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.pdb.db.utils;

import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;

/**
 * @author Gernot Belger
 */
public class CrossSectionPartTypes
{
  private final CrossSectionPartType[] m_types;

  /**
   * Initialize with all part types of a database from an existing session.
   */
  public CrossSectionPartTypes( final Session session ) throws PdbConnectException
  {
    this( GetPdbList.getArray( session, CrossSectionPartType.class ) );
  }

  public CrossSectionPartTypes( final CrossSectionPartType[] types )
  {
    m_types = types;
  }

  public CrossSectionPartType findPartType( final String partType )
  {
    if( m_types == null )
      return null;

    for( final CrossSectionPartType type : m_types )
    {
      if( type.getCategory().equals( partType ) )
        return type;
    }

    return null;
  }

  public CrossSectionPartType[] getTypes( )
  {
    return m_types;
  }
}