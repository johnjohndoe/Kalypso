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

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Command for deleting one 1D element. The change event has to fired from outside!
 *
 * @author Patrice Congo
 * @author ig, barbarins
 */
public class DeleteElement1DCmd implements IFeatureChangeCommand
{
  private final Collection<Feature> m_changedFeatures = new HashSet<>();

  private final Set<IElement1D> m_elementsToRemove = new HashSet<>();

  private final IFEDiscretisationModel1d2d m_model1d2d;

  public DeleteElement1DCmd( final IFEDiscretisationModel1d2d model1d2d )
  {
    this( model1d2d, null );
  }

  public DeleteElement1DCmd( final IFEDiscretisationModel1d2d model1d2d, final IElement1D pFeature )
  {
    m_model1d2d = model1d2d;

    addElementToRemove( pFeature );
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteElement1DCmd.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void process( ) throws Exception
  {
    final Set<Feature> lSetEdges = new HashSet<>();
    final Set<Feature> elementsToRemove = new HashSet<>();

    final RemoveEdgeWithoutContainerOrInvCmd lCmdEdgeRemove = new RemoveEdgeWithoutContainerOrInvCmd( m_model1d2d, null );

    for( final IElement1D element1d : m_elementsToRemove )
    {
      elementsToRemove.add( element1d );

      m_changedFeatures.add( element1d );

      final List<IFE1D2DComplexElement> elementConmtainers = element1d.getContainers();

      /* Remove element from itsa containers */
      for( final IFE1D2DComplexElement elementContainer : elementConmtainers )
      {
        final IFeatureBindingCollection<IFENetItem> elements = elementContainer.getElements();
        elements.getFeatureList().removeLink( element1d );

        m_changedFeatures.add( elementContainer );
      }

      /* Remove corresponding edge */
      final IFE1D2DEdge edge = element1d.getEdge();

      /* Remove edge from its containers */

      // TODO: why is this necessary? The edge is removed, so are its container list
      final IFeatureBindingCollection edgeContainers = edge.getContainers();
      final boolean isRemoved = edgeContainers.getFeatureList().removeLink( element1d );
      if( !isRemoved )
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteElement1DCmd.1" ) ); //$NON-NLS-1$

      final IFeatureBindingCollection<IFE1D2DNode> nodes = edge.getNodes();
      m_changedFeatures.addAll( nodes );

      lSetEdges.add( edge );

      m_changedFeatures.add( edge );

      lCmdEdgeRemove.addEdgeToRemove( edge );
    }

    lCmdEdgeRemove.process();

    // FIXME: did not the edge command remove the edges?
    // FIXME: makes no sense: elements list never contains edges at all!
    m_model1d2d.getElements().removeAll( lSetEdges );

    m_model1d2d.getElements().removeAll( elementsToRemove );
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

  public void addElementToRemove( final IElement1D element )
  {
    if( element != null )
      m_elementsToRemove.add( element );
  }
}