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

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ILinkageNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class LinkageNode extends AbstractConnectionNode implements ILinkageNode
{

  public LinkageNode( IModelMember model, Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#delete()
   */
  public void delete( ) throws Exception
  {
    FeatureUtils.deleteFeature( getFeature() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  public TYPE getType( )
  {
    return TYPE.eLinkageNode;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    IBranch[] inflowing = getInflowingBranches();
    IBranch[] outflowing = getOutflowingBranches();

    if( inflowing.length == 0 && outflowing.length == 0 )
      return true;

    return false;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#setLinkToBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch[])
   */
  public void setLinkToBranch( IBranch[] branches ) throws Exception
  {
    IBranch[] inflowing = getInflowingBranches();
    IBranch[] outflowing = getOutflowingBranches();

    for( IBranch branch : branches )
    {
      /* if branch is an in- or outflowing branch -> continue */
      if( ArrayUtils.contains( inflowing, branch ) )
        continue;
      if( ArrayUtils.contains( outflowing, branch ) )
        continue;

      /* linkage node lays on branch?!? */
      GM_Curve curve = branch.getGeometryProperty();
      GM_Point point = this.getLocation();

      if( curve.intersects( point ) ) // point lays on branch
      {
        FeatureUtils.updateLinkedFeature( getFeature(), ISobekConstants.QN_LN_LINKS_TO_BRANCH, branch.getId() );

      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ILinkageNode#getLinkToBranch()
   */
  public IBranch getLinkToBranch( )
  {
    final Object objBranch = getFeature().getProperty( ISobekConstants.QN_LN_LINKS_TO_BRANCH );
    final Feature f;
    if( objBranch instanceof Feature )
      // this branch should never be reached according to the schema file
      f = (Feature) objBranch;
    else
      f = getFeature().getWorkspace().getFeature( (String) objBranch );
    
    final IBranch result = new Branch( getModel(), f );
    
    if( result == null )
      return null;
    return result;
  }

}
