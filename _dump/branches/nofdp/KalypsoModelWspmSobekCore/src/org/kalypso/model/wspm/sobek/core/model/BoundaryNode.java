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

import java.util.List;

import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class BoundaryNode extends AbstractConnectionNode implements IBoundaryNode
{

  public BoundaryNode( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#delete()
   */
  public void delete( ) throws Exception
  {
    FeatureUtils.deleteFeature( getModel().getWorkspace(), getFeature() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode#getBoundaryType()
   */
  public BOUNDARY_TYPE getBoundaryType( )
  {
    return BOUNDARY_TYPE.getType( this );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode#getLastfallCondition(org.kalypso.model.wspm.sobek.core.interfaces.ILastfall)
   */
  public IBoundaryNodeLastfallCondition getLastfallCondition( final ILastfall lastfall ) throws Exception
  {
    final List< ? > members = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER );
    for( final Object object : members )
    {
      if( !(object instanceof Feature) )
        continue;

      final Feature member = (Feature) object;

      final Feature fLastfall = FeatureUtils.resolveFeature( member.getWorkspace(), member.getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL ) );
      if( fLastfall == null ) // lastfall doesn't exist - remove entry! - perhaps, system.out and continue statement
        throw new IllegalStateException( Messages.BoundaryNode_0 );

      // $ANALYSIS-IGNORE
      if( lastfall.getFeature().equals( fLastfall ) )
        return new BoundaryNodeLastfallCondition( lastfall, this, member );
    }

    final Feature condition = NodeUtils.createBoundaryNodeLastfallCondition( lastfall, this );
    return new BoundaryNodeLastfallCondition( lastfall, this, condition );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  public TYPE getType( )
  {
    return TYPE.eBoundaryNode;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    final IBranch[] inflowingBranches = getInflowingBranches();
    final IBranch[] outflowingBranches = getOutflowingBranches();

    if( (inflowingBranches.length == 0) && (outflowingBranches.length == 0) )
      return true;

    return false;
  }

}
