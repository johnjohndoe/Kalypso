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
package org.kalypso.kalypsomodel1d2d.ui.map.element1d;

import java.util.HashSet;
import java.util.Set;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class Create2dElementCommand implements ICommand
{
  private final IFEDiscretisationModel1d2d m_discModel;

  private final Set<IFE1D2DNode> m_changedNodes = new HashSet<>();

  private final Set<IFE1D2DEdge> m_changedEdges = new HashSet<>();

  private final Set<IElement1D> m_changedElements = new HashSet<>();

  private final GM_Point[] m_points;

  private IPolyElement m_newElement;

  public Create2dElementCommand( final IFEDiscretisationModel1d2d discModel, final GM_Point[] points )
  {
    m_discModel = discModel;
    m_points = points;
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void redo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void undo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.1" ); //$NON-NLS-1$
  }

  @Override
  public void process( ) throws Exception
  {
    /* Build new nodes */
    final IFE1D2DNode[] nodes1 = ElementGeometryHelper.buildNewNodes( m_discModel, m_points );
    final IFE1D2DNode[] nodes = ElementGeometryHelper.makeCCW( nodes1 );

    /* Build new edges */
    final IFE1D2DEdge[] edges = ElementGeometryHelper.buildNewEdges( m_discModel, nodes, m_points.length );

    /* Build new element */
    m_newElement = m_discModel.createElement2D( edges );

    /* fire workspace events */
    final GMLWorkspace discWorkspace = m_discModel.getWorkspace();

    // FIXME: fill hashsets with changed elements to improve events

    final Feature[] changedNodes = m_changedNodes.toArray( new Feature[m_changedNodes.size()] );
    discWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( discWorkspace, m_discModel, changedNodes, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    final Feature[] changedEdges = m_changedEdges.toArray( new Feature[m_changedEdges.size()] );
    discWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( discWorkspace, m_discModel, changedEdges, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    final Feature[] changedElements = m_changedElements.toArray( new Feature[m_changedElements.size()] );
    discWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( discWorkspace, m_discModel, changedElements, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }

  public IPolyElement getNewElement( )
  {
    return m_newElement;
  }
}