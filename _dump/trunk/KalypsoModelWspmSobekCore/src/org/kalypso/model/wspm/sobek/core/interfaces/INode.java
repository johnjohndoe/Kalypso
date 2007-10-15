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
package org.kalypso.model.wspm.sobek.core.interfaces;

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kuch
 */
public interface INode
{
  public enum TYPE
  {
    eBoundaryNode,
    eConnectionNode,
    eCrossSectionNode,
    eLinkageNode
  }

  public enum NODE_BRANCH_TYPE
  {
    eOutflowingBranch,
    eInflowingBranch;

    public static NODE_BRANCH_TYPE getType( final GMLWorkspace workspace, final Feature branch, final Feature node )
    {
      // final Object objUpperNode = branch.getProperty( GmlConstants.QN_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE );
      // final Feature upperNode = FNGmlUtils.getLinkedNodeFeature( model, objUpperNode );
      // if( node.equals( upperNode ) )
      // return eOutflowingBranch;
      //
      // final Object objLowerNode = branch.getProperty( GmlConstants.QN_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE );
      // final Feature lowerNode = FNGmlUtils.getLinkedNodeFeature( model, objLowerNode );
      // if( node.equals( lowerNode ) )
      // return eInflowingBranch;
      //
      // return null;
      throw (new NotImplementedException());
    }
  }

  public TYPE getType( );

  public String getName( );
}
