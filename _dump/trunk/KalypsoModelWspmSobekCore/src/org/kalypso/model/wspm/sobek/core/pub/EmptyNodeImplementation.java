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
package org.kalypso.model.wspm.sobek.core.pub;

import javax.transaction.NotSupportedException;

import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.model.Branch;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Objects of class EmptyNodeImplementation will be returned for all created nodes which this plugin doesn't recognize
 * 
 * @author kuch
 */
public class EmptyNodeImplementation implements INode
{

  private final Feature m_node;

  public EmptyNodeImplementation( Feature node )
  {
    m_node = node;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#delete()
   */
  public void delete( ) throws Exception
  {
    throw (new NotSupportedException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getFeature()
   */
  public Feature getFeature( )
  {
    return m_node;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getGeometry()
   */
  public GM_Point getGeometry( )
  {
    throw (new IllegalStateException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getId()
   */
  public String getId( )
  {
    return (String) m_node.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getInflowingBranches()
   */
  public IBranch[] getInflowingBranches( )
  {
    throw (new IllegalStateException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getName()
   */
  public String getName( )
  {
    throw (new IllegalStateException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getOutflowingBranches()
   */
  public IBranch[] getOutflowingBranches( )
  {
    throw (new IllegalStateException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  public TYPE getType( )
  {
    throw (new IllegalStateException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    throw (new IllegalStateException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#removeBranch(org.kalypso.model.wspm.sobek.core.model.Branch)
   */
  public void removeBranch( Branch branch )
  {
    throw (new IllegalStateException());
  }

}
