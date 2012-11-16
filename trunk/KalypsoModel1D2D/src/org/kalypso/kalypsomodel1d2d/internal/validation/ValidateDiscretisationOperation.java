/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author Thomas Jung
 */
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

    monitor.beginTask( "Validating mesh", 4 );

    monitor.subTask( "Elements" );
    log.add( validateElements() );
    ProgressUtilities.worked( monitor, 1 );

    monitor.subTask( "Edges" );
    log.add( validateEdges() );
    ProgressUtilities.worked( monitor, 1 );

    monitor.subTask( "Nodes" );
    log.add( validateNodes() );
    ProgressUtilities.worked( monitor, 1 );

    monitor.subTask( "Relation between edges and elements" );
    log.add( validateEdgeElementRelations() );
    ProgressUtilities.worked( monitor, 1 );

    monitor.done();

    return log.asMultiStatusOrOK( "Model validation has warnings", "Validation terminated without warnings" );
  }

  private IStatus validateNodes( )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

    // get nodes from discModel
    final IFE1D2DNode[] nodes = m_discModel.getNodes();

    for( final IFE1D2DNode node : nodes )
    {
      // check geometry of node
      final GM_Point point = node.getPoint();
      if( point == null )
      {
        // remove point with no geometry
        final String msg = String.format( "Node '%s': missing geometry", node.getId() );
        log.add( IStatus.WARNING, msg );
      }

      // check if node has any elements
      final IFE1D2DElement[] nodeElems = node.getAdjacentElements();
      if( nodeElems.length == 0 )
      {
        // delete node
        final String msg = String.format( "Node '%s': no reference to an element", node.getId() );
        log.add( IStatus.WARNING, msg );
      }
    }

    return log.asMultiStatus( "Nodes" );
  }

  private IStatus validateEdges( )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

    // get edges from discModel
    final IFE1D2DEdge[] edges = m_discModel.getEdges();

    for( final IFE1D2DEdge edge : edges )
    {
      // get nodes of the edge
      final IFE1D2DNode[] nodes = edge.getNodes();

      // check number of nodes
      if( nodes.length != 2 )
      {
        log.add( IStatus.WARNING, "edge with id " + edge.getId() + "has " + nodes.length + " nodes." );
        // delete node
        // log.add( IStatus.WARNING, "node with id " + node.getGmlID() +
        // "has corresponding element and will be removed." );
      }

      // get containers of edge
      final IFE1D2DElement[] elementArray = edge.getLinkedElements();

      // check number of elements at edge
      if( elementArray.length > 2 )
      {
        final String msg = String.format( "Edge '%s': more than two adjacent elements", edge.getId() );
        log.add( IStatus.WARNING, msg );
      }

      // check for non-existing elements at edge
      for( final IFE1D2DElement edgeElement : elementArray )
      {
        if( edgeElement == null )
        {
          final String msg = String.format( "Edge '%s': contains invalid element reference", edge.getId() );
          log.add( IStatus.WARNING, msg );
          // removeEdgeElement( edge, i );
        }
      }
    }

    return log.asMultiStatus( "Edges" );
  }

  private IStatus validateElements( )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

    // get the elements from discModel
    final IFE1D2DElement[] elements = m_discModel.getElements();

    for( final IFE1D2DElement element : elements )
    {
      // check geometry of 2D-Elements
      if( element instanceof IPolyElement )
      {
        // get surface
        final IPolyElement element2D = (IPolyElement)element;
        final GM_Polygon eleGeom = element2D.getGeometry();

        if( eleGeom == null )
        {
          // delete element with no geometry
          final String msg = String.format( "Element '%s': geometry missing", element2D.getId() );
          log.add( IStatus.WARNING, msg );
        }
      }

      /* edge references */
      final IFE1D2DEdge[] edges = element.getEdges();
      for( final IFE1D2DEdge edge : edges )
      {
        if( edge == null )
        {
          final String msg = String.format( "Element '%s': contains invalid edge reference", element.getId() );
          log.add( IStatus.WARNING, msg );
        }
      }
    }

    return log.asMultiStatus( "Elements" );
  }

  /**
   * checking edge <-> element relations
   * every edge should have the information about its elements. for some reason there occur missing element references
   * at the edge. in order to find the real relation between edges and elements, we loop over all elements, collect
   * the edges of that element and store the relationship in a Map<edge,List<element>>. Later we loop over all edges
   * and compare the calculated number of elements with the number of edges stored at the edge itself.
   */
  private IStatus validateEdgeElementRelations( )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

    final IFE1D2DElement[] elements = m_discModel.getElements();
    final IFE1D2DEdge[] edges = m_discModel.getEdges();

    final Map<IFE1D2DEdge, List<IFE1D2DElement>> edgeMap = new HashMap<>();

    // calculate element / edge relations
    checkEdgeEntries( elements, edgeMap, log );

    // compare the number of edges
    for( final IFE1D2DEdge edge : edges )
    {
      final int numOfElementsAtEdge = getNumOfElementsAtEdge( edge );
      final List<IFE1D2DElement> list = edgeMap.get( edge );

      if( list == null )
      {
        // bad element or edge???
        continue;
      }

      if( list.size() != numOfElementsAtEdge )
      {
        log.add( IStatus.WARNING, "edge with id " + edge.getId() + " has only " + numOfElementsAtEdge + " element defined. It should be " + list.size() );
        // TODO: add missing element to edge
      }
    }

    return log.asMultiStatus( "Edge / Element relations" );
  }

  private void checkEdgeEntries( final IFE1D2DElement[] elements, final Map<IFE1D2DEdge, List<IFE1D2DElement>> edgeMap, final IStatusCollector log )
  {
    for( final IFE1D2DElement element2 : elements )
    {
      final IFE1D2DElement element = element2;

      final IFE1D2DNode[] elementNodes = element.getNodes();
      if( elementNodes == null )
      {
        log.add( IStatus.WARNING, "element with id " + element.getId() + " has no nodes" );
        continue;
      }

      for( final IFE1D2DNode elementNode : elementNodes )
      {
        final IFE1D2DEdge[] nodeContainers = elementNode.getLinkedEdges();
        for( final IFE1D2DEdge edge : nodeContainers )
        {
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
            final List<IFE1D2DElement> elementList = new ArrayList<>();
            elementList.add( element );
            edgeMap.put( edge, elementList );
          }
        }
      }
    }
  }

  private boolean edgeIsPartOfCurrentElement( final IFE1D2DElement element, final IFE1D2DEdge edge )
  {
    // get nodes of the edge
    final IFE1D2DNode[] edgeNodes = edge.getNodes();

    // get nodes of the IFE1d2dElement
    final IFE1D2DNode[] elementNodes = element.getNodes();
    if( elementNodes == null || elementNodes.length == 0 )
      return false;

    // check, if all nodes of the edge are in the nodes list of the element => edge is part of the element
    for( final IFE1D2DNode edgeNode : edgeNodes )
    {
      boolean contained = false;
      for( final IFE1D2DNode node : elementNodes )
        if( node.equals( edgeNode ) )
          contained = true;
      if( !contained )
        return false;
    }
    return true;
  }

  private int getNumOfElementsAtEdge( final IFE1D2DEdge edge )
  {
    int numOfContainers = 0;
    final IFE1D2DElement[] edgeContainers = edge.getLinkedElements();
    for( final IFE1D2DElement object : edgeContainers )
    {
      if( object == null )
        continue;

      numOfContainers++;
    }
    return numOfContainers;
  }

//  private void removeEdgeElement( final FE1D2DEdge edge, final int index )
//  {
//    final FeatureList fList = (FeatureList)edge.getProperty( IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );
//    fList.remove( index );
//  }

  public boolean hasValidationFixes( )
  {
    return m_commands.getCommands().length > 0;
  }

  public ICommand getValidationFix( )
  {
    // FIXME: is this enough or do we need to fire workspace events
    return m_commands;
  }
}