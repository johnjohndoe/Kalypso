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
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzone;
import org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzonenDistances;
import org.kalypso.model.wspm.sobek.core.sperrzone.Sperrzone;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddRelationCommand;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractConnectionNode extends AbstractNode implements INode
{

  private Sperrzone m_sperrzone = null;

  public AbstractConnectionNode( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#addInflowingBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public void addInflowingBranch( final IBranch branch ) throws Exception
  {
    final List< ? > inflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );
    for( final Object object : inflowing )
    {
      if( object instanceof XLinkedFeature_Impl )
      {
        final XLinkedFeature_Impl lnk = (XLinkedFeature_Impl) object;
        if( lnk.getFeature().equals( branch.getFeature() ) )
          return;
      }
      else if( object instanceof Feature )
      {
        final Feature feature = (Feature) object;
        if( feature.equals( branch.getFeature() ) )
          return;
      }
      else if( object instanceof String )
      {
        if( branch.getFeature().getId().equals( object ) )
          return;
      }
      else
        throw new IllegalStateException();
    }

    final IRelationType lnkFeatureType = (IRelationType) getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );

    final AtomarAddRelationCommand cmd = new AtomarAddRelationCommand( getFeature(), lnkFeatureType, -1, branch.getFeature() );

    final CommandableWorkspace workspace = getModel().getWorkspace();
    workspace.postCommand( cmd );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#addOutflowingBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public void addOutflowingBranch( final IBranch branch ) throws Exception
  {
    final List< ? > outflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );
    for( final Object object : outflowing )
    {
      if( object instanceof XLinkedFeature_Impl )
      {
        final XLinkedFeature_Impl lnk = (XLinkedFeature_Impl) object;
        if( lnk.getFeature().equals( branch.getFeature() ) )
          return;
      }
      else if( object instanceof Feature )
      {
        final Feature feature = (Feature) object;
        if( feature.equals( branch.getFeature() ) )
          return;
      }
      else if( object instanceof String )
      {
        if( branch.getFeature().getId().equals( object ) )
          return;
      }
      else
        throw new IllegalStateException();
    }

    final IRelationType lnkFeatureType = (IRelationType) getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );

    final AtomarAddRelationCommand cmd = new AtomarAddRelationCommand( getFeature(), lnkFeatureType, -1, branch.getFeature() );

    final CommandableWorkspace workspace = getModel().getWorkspace();
    workspace.postCommand( cmd );
  }

  private IBranch[] getBranches( final List< ? > inflowing )
  {
    final List<IBranch> branches = new ArrayList<IBranch>();

    for( final Object obj : inflowing )
    {
      final Feature feature = FeatureUtils.resolveFeature( getFeature().getWorkspace(), obj );
      branches.add( new Branch( getModel(), feature ) );
    }

    return branches.toArray( new IBranch[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode#getInflowingBranches()
   */
  public IBranch[] getInflowingBranches( )
  {
    final List< ? > inflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );

    return getBranches( inflowing );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode#getOutflowingBranches()
   */
  public IBranch[] getOutflowingBranches( )
  {
    final List< ? > outflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );

    return getBranches( outflowing );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#removeBranch(org.kalypso.model.wspm.sobek.core.model.Branch)
   */
  public void removeBranch( final Branch branch )
  {
    final IBranch[] inflowing = getInflowingBranches();
    final IBranch[] outflowingBranches = getOutflowingBranches();

    if( ArrayUtils.contains( inflowing, branch ) )
      removeInflowingBranch( branch );

    if( ArrayUtils.contains( outflowingBranches, branch ) )
      removeOutflowingBranch( branch );
  }

  private void removeInflowingBranch( final Branch branch )
  {
    final List< ? > inflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );
    inflowing.remove( branch.getFeature().getId() );
  }

  private void removeOutflowingBranch( final Branch branch )
  {
    final List< ? > inflowing = (List< ? >) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );
    inflowing.remove( branch.getFeature().getId() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getSperrzone(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public ISperrzone getSperrzone( )
  {
    if( m_sperrzone == null )
    {
      m_sperrzone = new Sperrzone( getFeature() );
      try
      {
        final GM_Point point = getLocation();
        final Geometry jtsPoint = JTSAdapter.export( point );
        final Geometry bufferedPoint = jtsPoint.buffer( ISperrzonenDistances.CONNECTION_NODE );

        final IBranch[] inflowingBranches = getInflowingBranches();
        final IBranch[] outflowingBranches = getOutflowingBranches();

        for( final IBranch branch : inflowingBranches )
        {
          m_sperrzone.addSperrzone( branch, bufferedPoint );
        }

        for( final IBranch branch : outflowingBranches )
        {
          m_sperrzone.addSperrzone( branch, bufferedPoint );
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }

    return m_sperrzone;
  }
}
