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
package org.kalypso.model.wspm.sobek.core.model;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.utils.ILinkFeatureWrapperDelegate;
import org.kalypso.model.wspm.sobek.core.utils.LinkFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public abstract class AbstractConnectionNode extends AbstractNode implements IConnectionNode
{

  public AbstractConnectionNode( IModelMember model, Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#addInflowingBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public void addInflowingBranch( IBranch branch )
  {
    List inflowing = (List) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );
    inflowing.add( branch.getFeature().getId() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#addOutflowingBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public void addOutflowingBranch( IBranch branch )
  {
    List outflowing = (List) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );
    outflowing.add( branch.getFeature().getId() );
  }

  private IBranch[] getBranches( List< ? > inflowing )
  {
    final List<IBranch> branches = new ArrayList<IBranch>();

    for( final Object obj : inflowing )
    {
      ILinkFeatureWrapperDelegate delegate = new ILinkFeatureWrapperDelegate()
      {

        public Feature getLinkedFeature( String id )
        {
          IBranch[] myBranches = getModel().getBranchMembers();
          for( IBranch branch : myBranches )
          {
            if( branch.getFeature().getId().equals( id ) )
              return branch.getFeature();
          }

          return null;
        }

        public Object getProperty( )
        {
          return obj;
        }
      };

      LinkFeatureWrapper wrapper = new LinkFeatureWrapper( delegate );
      Feature feature = wrapper.getFeature();
      if( feature == null )
        continue;

      branches.add( new Branch( getModel(), feature ) );
    }

    return branches.toArray( new IBranch[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode#getInflowingBranches()
   */
  public IBranch[] getInflowingBranches( )
  {
    List< ? > inflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );

    return getBranches( inflowing );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode#getOutflowingBranches()
   */
  public IBranch[] getOutflowingBranches( )
  {
    List< ? > outflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );

    return getBranches( outflowing );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#removeBranch(org.kalypso.model.wspm.sobek.core.model.Branch)
   */
  public void removeBranch( Branch branch )
  {
    IBranch[] inflowing = getInflowingBranches();
    IBranch[] outflowingBranches = getOutflowingBranches();

    if( ArrayUtils.contains( inflowing, branch ) )
    {
      this.removeInflowingBranch( branch );
    }

    if( ArrayUtils.contains( outflowingBranches, branch ) )
    {
      this.removeOutflowingBranch( branch );
    }
  }

  private void removeInflowingBranch( Branch branch )
  {
    List< ? > inflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );
    inflowing.remove( branch.getFeature().getId() );
  }

  private void removeOutflowingBranch( Branch branch )
  {
    List< ? > inflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );
    inflowing.remove( branch.getFeature().getId() );
  }
}
