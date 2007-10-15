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
package org.kalypso.model.wspm.sobek.core.model;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.SobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * @author kuch
 */
public class Branch implements IBranch
{
  private final Feature m_branch;

  public Branch( Feature branch )
  {
    m_branch = branch;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getGeometryProperty()
   */
  public GM_Object getGeometryProperty( )
  {
    return m_branch.getDefaultGeometryProperty();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getName()
   */
  public String getName( )
  {
    throw (new NotImplementedException());
  }

  public static String createBranchId( SobekModelMember model )
  {
    int count = 0;

    IBranch[] branches = model.getBranchMembers();
    for( final IBranch branch : branches )
    {
      String branchId = branch.getId();
      if( branchId == null )
        continue;

      final String[] split = branchId.split( "_" );
      if( split.length != 2 )
        throw new IllegalStateException();

      final Integer iBranch = new Integer( split[1] );
      if( iBranch > count )
        count = iBranch;
    }

    return String.format( "b_%05d", ++count );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranch#getId()
   */
  public String getId( )
  {
    return (String) m_branch.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID );
  }
}
