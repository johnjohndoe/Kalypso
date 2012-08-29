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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Command for deleting one element. The change event has to fired from outside!
 *
 * @author Patrice Congo
 */
public class DeletePolyElementCmd implements IFeatureChangeCommand
{
  private final Set<Feature> m_changedFeatures = new HashSet<>();

  private final Set<Feature> m_setFeaturesToRemove = new HashSet<Feature>();

  private final IFEDiscretisationModel1d2d m_model1d2d;

  public DeletePolyElementCmd( final IFEDiscretisationModel1d2d model1d2d )
  {
    this( model1d2d, null );
  }

  public DeletePolyElementCmd( final IFEDiscretisationModel1d2d model1d2d, final Feature pFeature )
  {
    m_model1d2d = model1d2d;

    addElementToRemove( pFeature );
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    final RemoveEdgeWithoutContainerOrInvCmd remEdgeCmd = new RemoveEdgeWithoutContainerOrInvCmd( m_model1d2d, null );

    for( final Feature lFeature : m_setFeaturesToRemove )
    {
      final IPolyElement lElement = (IPolyElement) lFeature.getAdapter( IPolyElement.class );
      final String elementID = lElement.getId();

      // delete link to complex elements
      final List<IFE1D2DComplexElement> parentComplexElements = lElement.getContainers();
      for( final IFE1D2DComplexElement complexElement : parentComplexElements )
      {
        complexElement.getElements().getFeatureList().removeLink( lElement );
        m_changedFeatures.add( complexElement );
      }
      m_changedFeatures.add( lElement );

      // delete link to edges and the edges itself (with the nodes)
      final IFeatureBindingCollection<IFE1D2DEdge> edges = lElement.getEdges();
      for( final IFE1D2DEdge edge : edges )
      {
        final FeatureList containers = edge.getContainers().getFeatureList();
        final IFeatureBindingCollection<IFE1D2DElement> lListContainers = edge.getContainers();
        boolean lBoolContainsAll = true;
        for( final IFE1D2DElement element : lListContainers )
        {
          final IPolyElement lFeatureAct = (IPolyElement) element;
          if( !m_setFeaturesToRemove.contains( lFeatureAct ) )
          {
            lBoolContainsAll = false;
            break;
          }
        }

        if( lBoolContainsAll )
        {
          remEdgeCmd.addEdgeToRemove( edge );
        }
        containers.remove( elementID );
        m_changedFeatures.add( edge );

        final IFeatureBindingCollection< ? > nodes = edge.getNodes();
        for( final Feature feature : nodes )
        {
          m_changedFeatures.add( feature );
        }
      }
    }
    remEdgeCmd.process();
    // delete element from model
    m_model1d2d.getElements().removeAll( m_setFeaturesToRemove );
  }

  @Override
  public void redo( ) throws Exception
  {

  }

  @Override
  public void undo( ) throws Exception
  {

  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return m_changedFeatures.toArray( new Feature[m_changedFeatures.size()] );
  }

  public void addElementToRemove( final Feature pFeature )
  {
    if( pFeature != null )
      m_setFeaturesToRemove.add( pFeature );
  }
}
