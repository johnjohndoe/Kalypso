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
package org.kalypso.kalypsomodel1d2d.internal.validation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

/**
 * @author Thomas Jung
 */
@SuppressWarnings("rawtypes")
public class ValidateDiscretisationOperation implements ICoreRunnableWithProgress
{
  private final CompositeCommand m_commands = new CompositeCommand( "Fix validation problems" );

  private final IFEDiscretisationModel1d2d m_discModel;

  public ValidateDiscretisationOperation( final IFEDiscretisationModel1d2d discModel )
  {
    m_discModel = discModel;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

    log.add( IStatus.INFO, "validate elements" );

    // get the elements from discModel
    List<IFE1D2DElement> elements = m_discModel.getElements();

    for( final IFE1D2DElement element : elements )
    {
      // check geometry of 2D-Elements
      if( element instanceof IPolyElement )
      {
        // get surface
        final IPolyElement< ? , ? > element2D = (IPolyElement< ? , ? >) element;
        final GM_Surface<GM_SurfacePatch> eleGeom = element2D.getGeometry();

        if( eleGeom == null )
        {
          // delete element with no geometry
          log.add( IStatus.WARNING, "element with id " + element2D.getId() + "has no geometry and will be removed." );

          // check counter reference of the nodes of the bad element

          // TODO: this check has to be made with all nodes!!
          log.add( IStatus.INFO, "checking nodes..." );

          final List<IFE1D2DNode> nodes = element2D.getNodes();
          for( final IFE1D2DNode node : nodes )
          {
            final IFE1D2DElement[] nodeElems = node.getElements();
            if( nodeElems.length == 0 )
            {
              // delete node
              log.add( IStatus.WARNING, "node with id " + node.getId() + "has no corresponding element and will be removed." );
            }
          }

        }
      }
    }

    /* edges */
    log.add( IStatus.INFO, "validate edges" );

    // get edges from discModel
    final IFeatureBindingCollection<IFE1D2DEdge> edges = m_discModel.getEdges();

    for( final IFE1D2DEdge edge : edges )
    {
      // get nodes of the edge
      final IFeatureBindingCollection nodes = edge.getNodes();

      // check number of nodes
      if( nodes.size() != 2 )
      {
        log.add( IStatus.WARNING, "edge with id " + edge.getId() + "has " + nodes.size() + " nodes." );
        // delete node
        // log.add( IStatus.WARNING, "node with id " + node.getGmlID() +
        // "has corresponding element and will be removed." );
      }
    }

    /* nodes */
    log.add( IStatus.INFO, "validate nodes" );

    // get nodes from discModel
    final IFeatureBindingCollection<IFE1D2DNode> nodes = m_discModel.getNodes();

    for( final IFE1D2DNode node : nodes )
    {
      // check geometry of node
      final GM_Point point = node.getPoint();
      if( point == null )
      {
        // remove point with no geometry
        log.add( IStatus.WARNING, "node with id " + node.getId() + " has no geometry and will be removed." );
      }

      // check containers of node
      final IFeatureBindingCollection containers = node.getContainers();

      for( final Object container : containers )
      {
        // handle edges
        if( container instanceof FE1D2DEdge )
        {
          final FE1D2DEdge edge = (FE1D2DEdge) container;

          // get containers of edge
          final IFeatureBindingCollection<IFE1D2DElement> edgeElements = edge.getContainers();

          // get nodes of edge
          final IFeatureBindingCollection<IFE1D2DNode> edgeNodes = edge.getNodes();

          // TODO: this check was done above already
          if( edgeNodes.size() != 2 )
            log.add( IStatus.WARNING, "edge with id " + edge.getId() + "has " + edgeNodes.size() + " nodes." );

          // check elements of edge
          final IFE1D2DElement[] elementArray = edgeElements.toArray( new IFE1D2DElement[edgeElements.size()] );

          // check number of elements at edge
          if( elementArray.length > 2 )
            log.add( IStatus.WARNING, "edge with id " + edge.getId() + " has more than two elements adjecting. " );

          // check for non-existing elements at edge
          for( int i = 0; i < elementArray.length; i++ )
          {
            final IFE1D2DElement edgeElement = elementArray[i];
            if( edgeElement == null )
            {
              log.add( IStatus.WARNING, "edge with id " + edge.getId() + " has invalid element. Element will be removed" );
              removeEdgeElement( edge, i );
            }
          }
        }
      }
    }

    /**
     * checking edge <-> element relations
     *
     * every edge should have the information about its elements. for some reason there occur missing element references
     * at the edge. in order to find the real relation between edges and elements, we loop over all elements, collect
     * the edges of that element and store the relationship in a Map<edge,List<element>>. Later we loop over all edges
     * and compare the calculated number of elements with the number of edges stored at the edge itself.
     */

    log.add( IStatus.INFO, "validate edge / element relations" );

    elements = m_discModel.getElements();
    final Map<IFE1D2DEdge, List<IFE1D2DElement>> edgeMap = new HashMap<IFE1D2DEdge, List<IFE1D2DElement>>();

    // calculate element / edge relations
    checkEdgeEntries( elements, edgeMap, log );

    // compare the number of edges
    for( final IFE1D2DEdge edge : edges )
    {
      final int numOfElementsAtEdge = getNumOfElementsAtEdge( edge );
      final List<IFE1D2DElement> list = edgeMap.get( edge );

      if( list == null )
      {
        //bad element or edge???
        continue;
      }
      if( list.size() != numOfElementsAtEdge )
      {
        log.add( IStatus.WARNING, "edge with id " + edge.getId() + " has only " + numOfElementsAtEdge + " element defined. It should be " + list.size() );
        // TODO: add missing element to edge
      }
    }

    log.add( IStatus.OK, "finished" );

    return log.asMultiStatusOrOK( "Model Validation has warnings", "Validation terminated without warnings" );
  }

  private void checkEdgeEntries( final List<IFE1D2DElement> elements, final Map<IFE1D2DEdge, List<IFE1D2DElement>> edgeMap, final IStatusCollector log )
  {
    for( int i = 0; i < elements.size(); i++ )
    {
      final IFE1D2DElement element = elements.get( i );

      final List elementNodes = element.getNodes();
      if( elementNodes == null )
      {
        log.add( IStatus.WARNING, "element with id " + element.getId() + " has no nodes" );
        continue;
      }

      for( int j = 0; j < elementNodes.size(); j++ )
      {
        final IFE1D2DNode feNode = (IFE1D2DNode) elementNodes.get( j );
        final IFeatureBindingCollection nodeContainers = feNode.getContainers();
        for( final Object nodeContainer : nodeContainers )
        {
          if( nodeContainer instanceof IFE1D2DEdge )
          {
            final IFE1D2DEdge edge = (IFE1D2DEdge) nodeContainer;

            // be sure, that we use only edges of the current element!

            if( !edgeIsPartOfCurrentElement( element, edge ) )
              continue;

            if( edgeMap.containsKey( edge ) )
            {
              final List<IFE1D2DElement> list = edgeMap.get( edge );
              if( !list.contains( element ) )

                list.add( element );
            }
            else
            {
              final List<IFE1D2DElement> elementList = new ArrayList<IFE1D2DElement>();
              elementList.add( element );
              edgeMap.put( edge, elementList );
            }
          }
        }
      }
    }
  }

  private boolean edgeIsPartOfCurrentElement( final IFE1D2DElement element, final IFE1D2DEdge edge )
  {
    // get nodes of the edge
    final IFeatureBindingCollection edgeNodes = edge.getNodes();

    // get nodes of the IFE1d2dElement
    final List elementNodes = element.getNodes();
    if( elementNodes == null || elementNodes.size() == 0 )
      return false;

    // check, if all nodes of the edge are in the nodes list of the element => edge is part of the element
    for( int i = 0; i < edgeNodes.size(); i++ )
    {
      final IFE1D2DNode edgeNode = (IFE1D2DNode) edgeNodes.get( i );
      if( !elementNodes.contains( edgeNode ) )
        return false;
    }

    return true;
  }

  private int getNumOfElementsAtEdge( final IFE1D2DEdge edge )
  {
    int numOfContainers = 0;
    final IFeatureBindingCollection edgeContainers = edge.getContainers();
    for( int i = 0; i < edgeContainers.size(); i++ )
    {
      final Object object = edgeContainers.get( i );
      if( object == null )
        continue;
      if( object instanceof IFE1D2DElement )
      {
        numOfContainers++;
      }
    }
    return numOfContainers;
  }

  private void removeEdgeElement( final FE1D2DEdge edge, final int index )
  {
    final FeatureList fList = (FeatureList) edge.getProperty( IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );
    fList.remove( index );
  }

  public ICommand getValidationFix( )
  {
    // FIXME: is this enough or do we need to fire workspace events

    return m_commands;
  }
}