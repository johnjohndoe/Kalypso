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
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Undoable command to change the position of a node. the change can be specified as a point or a change elevation
 * 
 * 
 * @author Patrice Congo
 */
@SuppressWarnings("deprecation")
public class ChangeNodePositionCommand implements IDiscrModel1d2dChangeCommand
{
  private final IFE1D2DNode m_node;

  private final GM_Point m_newPosition;

  private GM_Point m_oldPosition = null;

  private final IFEDiscretisationModel1d2d m_discretisationModel;

  /** If <code>true</code>, also modell-change events for depending elements (edges, elements) are fired. */
  private final boolean m_fireEventsForDependendElements;

  public ChangeNodePositionCommand( final IFEDiscretisationModel1d2d model, final IFE1D2DNode nodeToChange, final double elevation, final boolean fireEventsForDependendElements )
  {
    this( model, nodeToChange, createPoint( elevation, nodeToChange.getPoint() ), fireEventsForDependendElements );
  }

  public static GM_Point createPoint( final double elevation, final GM_Point pos )
  {
    if( Double.isNaN( elevation ) )
      return GeometryFactory.createGM_Point( pos.getX(), pos.getY(), pos.getCoordinateSystem() );

    return GeometryFactory.createGM_Point( pos.getX(), pos.getY(), elevation, pos.getCoordinateSystem() );
  }

  /**
   * @param fireEvents
   *          If <code>true</code>, modell-events will be fired.
   */
  public ChangeNodePositionCommand( final IFEDiscretisationModel1d2d model, final IFE1D2DNode nodeToChange, final GM_Point newNodePoint, final boolean fireEventsForDependendElements )
  {
    m_discretisationModel = model;
    m_node = nodeToChange;
    m_newPosition = newNodePoint;
    m_oldPosition = m_node.getPoint();
    m_fireEventsForDependendElements = fireEventsForDependendElements;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand.0" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    process( m_newPosition );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( final GM_Point position ) throws Exception
  {
    m_node.setPoint( position );

    final List<Feature> changedFeatures = new ArrayList<Feature>( 10 );
    changedFeatures.add( m_node.getFeature() );

    if( m_fireEventsForDependendElements )
    {
      /* Edges etc. */
      final IFeatureWrapperCollection<IFeatureWrapper2> containers = m_node.getContainers();
      for( final IFeatureWrapper2 featureWrapper2 : containers )
      {
        changedFeatures.add( featureWrapper2.getFeature() );
        featureWrapper2.getFeature().setEnvelopesUpdated();
      }

      /* Elements */
      final IFE1D2DElement[] elements = m_node.getElements();
      for( final IFE1D2DElement element : elements )
      {
        changedFeatures.add( element.getFeature() );
        element.getFeature().setEnvelopesUpdated();
      }
    }

    final GMLWorkspace workspace = m_discretisationModel.getFeature().getWorkspace();
    workspace.fireModellEvent( new FeaturesChangedModellEvent( workspace, changedFeatures.toArray( new Feature[changedFeatures.size()] ) ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    process( m_newPosition );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    process( m_oldPosition );
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { m_node };
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  @Deprecated
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_discretisationModel;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer();
    buf.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand.1" ) ); //$NON-NLS-1$
    buf.append( m_node );
    buf.append( ']' );
    return buf.toString();
  }

  public IFE1D2DNode getMovedNode( )
  {
    return m_node;
  }
}
