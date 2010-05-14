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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Command for deleting one element. The change event has to fired from outside!
 * 
 * @author Patrice Congo
 */
public class DeletePolyElementCmd implements IDiscrModel1d2dChangeCommand
{
  private final IFEDiscretisationModel1d2d m_model1d2d;

  @SuppressWarnings("unchecked")
  private final IPolyElement m_element2D;

  private List<Feature> m_changedFeatureList;

  @SuppressWarnings("unchecked")
  public DeletePolyElementCmd( final IFEDiscretisationModel1d2d model1d2d, final Feature feature )
  {
    m_model1d2d = model1d2d;
    m_element2D = (IPolyElement) feature.getAdapter( IPolyElement.class );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd.0"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @SuppressWarnings("unchecked")
  public void process( ) throws Exception
  {
    m_changedFeatureList = new ArrayList<Feature>();
    final String elementID = m_element2D.getGmlID();
    final RemoveEdgeWithoutContainerOrInvCmd remEdgeCmd = new RemoveEdgeWithoutContainerOrInvCmd( m_model1d2d, null );

    // delete link to complex elements
    final List<IFE1D2DComplexElement> parentComplexElements = m_element2D.getContainers();
    for( final IFE1D2DComplexElement complexElement : parentComplexElements )
    {
      complexElement.getElements().remove( elementID );
      m_changedFeatureList.add( complexElement.getFeature() );
    }
    m_changedFeatureList.add( m_element2D.getFeature() );

    // delete link to edges and the edges itself (with the nodes)
    final IFeatureWrapperCollection<IFE1D2DEdge> edges = m_element2D.getEdges();
    for( final IFE1D2DEdge edge : edges )
    {
      final FeatureList containers = edge.getContainers().getWrappedList();
      if( containers.contains( elementID ) )
        containers.remove( elementID );
      remEdgeCmd.setEdgeToDel( edge );
      m_changedFeatureList.add( edge.getFeature() );

      remEdgeCmd.process();
      final IFeatureWrapperCollection nodes = edge.getNodes();
      for( Iterator iterator = nodes.iterator(); iterator.hasNext(); )
      {
        IFeatureWrapper2 featureWrapper = (IFeatureWrapper2) iterator.next();
        Feature wrappedFeature = featureWrapper.getFeature();
        m_changedFeatureList.add( wrappedFeature );
      }
    }
    // delete element from model
    m_model1d2d.getElements().remove( m_element2D );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { m_element2D };
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model1d2d;
  }

  public List<Feature> getChangedFeatureList( )
  {
    return m_changedFeatureList;
  }
}
