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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;

/**
 * General command that adds 2D-Elements into the net.<br/>
 * FIXME: clean up this ugly code!
 * 
 * @author Gernot Belger
 */
public class Add2DElementsCommand implements ICommand
{
  private static final double SNAP_DISTANCE = 0.02;

  private final IStatusCollector m_log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

  private final Collection<String> m_newNodeIDs = new ArrayList<>();

  private final Collection<String> m_newEdgeIDs = new ArrayList<>();

  private final Collection<String> m_newElementIDs = new ArrayList<>();

  private final Map<GM_Position, IFE1D2DNode> m_snappedNodes = new HashMap<>();

  private final GMLWorkspace m_discretisationModel;

  private final List<GM_PolygonPatch> m_elements;

  public Add2DElementsCommand( final GMLWorkspace discretisationModel, final List<GM_PolygonPatch> elements )
  {
    m_discretisationModel = discretisationModel;
    m_elements = elements;
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void redo( )
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
    return null;
  }

  @Override
  public void process( ) throws Exception
  {
    final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d)m_discretisationModel.getRootFeature();

    for( final GM_PolygonPatch ring : m_elements )
      createElementFromRing( discModel, ring );

    fireEvents( discModel, m_newNodeIDs );
    fireEvents( discModel, m_newEdgeIDs );
    fireEvents( discModel, m_newElementIDs );
  }

  private void fireEvents( final IFEDiscretisationModel1d2d discModel, final Collection<String> newFeatureIDs )
  {
    final GMLWorkspace workspace = discModel.getWorkspace();
    final Collection<Feature> newFeatures = new ArrayList<>();

    for( final String id : newFeatureIDs )
      newFeatures.add( workspace.getFeature( id ) );

    final Feature[] newFeatureArray = newFeatures.toArray( new Feature[newFeatures.size()] );

    final FeatureStructureChangeModellEvent changeEvent = new FeatureStructureChangeModellEvent( m_discretisationModel, discModel, newFeatureArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
    m_discretisationModel.fireModellEvent( changeEvent );
  }

  private void createElementFromRing( final IFEDiscretisationModel1d2d discModel, final GM_PolygonPatch ring ) throws GM_Exception
  {
    final GM_Position[] positions = ring.getExteriorRing();
    final GM_Position[] cleanPositions = removeDuplicates( positions );

    final String srsName = ring.getCoordinateSystem();

    /* Check if positions overlap existing elements */
    if( !checkPositions( discModel, cleanPositions, srsName ) )
      return;

    /* Check if ring overlaps existing elements */
    if( !checkElements( discModel, cleanPositions, srsName ) )
      return;

    createElement( discModel, cleanPositions, srsName );
  }

  private GM_Position[] removeDuplicates( final GM_Position[] positions )
  {
    final Coordinate[] crds = JTSAdapter.export( positions );
    final CoordinateList list = new CoordinateList( crds, false );
    list.closeRing();

    final Coordinate[] cleanCrds = list.toCoordinateArray();
    return JTSAdapter.wrap( cleanCrds );
  }

  private boolean checkElements( final IFEDiscretisationModel1d2d discModel, final GM_Position[] positions, final String srsName ) throws GM_Exception
  {
    // Probably for performance reasong: fast check with centroid
    final IPolyElement element2d = discModel.find2DElement( GeometryUtilities.centroidFromRing( positions, srsName ), 0.0 );
    if( element2d != null )
      return false;

    // REMARK: test with surface, because ring has no interior (being a hole)
    final GM_Polygon newSurface = GeometryFactory.createGM_Surface( positions, null, srsName );
    final Geometry newPolygon = JTSAdapter.export( newSurface );

    final List<IFE1D2DElement> foundElements = discModel.queryElements( newSurface.getEnvelope(), null );
    for( final IFE1D2DElement foundElement : foundElements )
    {
      if( foundElement instanceof IPolyElement )
      {
        final GM_Polygon foundGeometry = ((IPolyElement)foundElement).getGeometry();
        final Geometry foundPolygon = JTSAdapter.export( foundGeometry );
        if( foundPolygon.overlaps( newPolygon ) )
        {
          m_log.add( IStatus.WARNING, Messages.getString( "Add2DElementsCommand_0" ), null, (Object)positions ); //$NON-NLS-1$
          return false;
        }
      }
    }

    return true;
  }

  private boolean checkPositions( final IFEDiscretisationModel1d2d discModel, final GM_Position[] positions, final String srsName )
  {
    /* Do not check last point, must be equal to first point */
    for( int i = 0; i < positions.length - 1; i++ )
    {
      final GM_Position position = positions[i];

      final GM_Point nodeLocation = GeometryFactory.createGM_Point( position, srsName );
      final IFE1D2DNode node = discModel.findNode( nodeLocation, SNAP_DISTANCE );
      if( node != null )
      {
        /* Remember found nodes for later reuse */
        m_snappedNodes.put( position, node );
      }
      else
      {
        final IPolyElement foundElement = discModel.find2DElement( nodeLocation, SNAP_DISTANCE );
        if( foundElement != null )
        {
          final GM_Polygon geometry = foundElement.getGeometry();
          if( geometry.contains( nodeLocation ) )
          {
            // do not insert nodes that are placed on existing model(overlapped elements)
            m_log.add( IStatus.WARNING, Messages.getString( "Add2DElementsCommand_1" ), null, position ); //$NON-NLS-1$
            return false;
          }
        }
      }
    }

    return true;
  }

  private void createElement( final IFEDiscretisationModel1d2d discModel, final GM_Position[] positions, final String srsName )
  {
    final IFE1D2DNode[] nodes = createNodes( discModel, positions, srsName );

    final IFE1D2DEdge[] edges = createEdges( discModel, nodes );

    /* final IPolyElement element = */createElement( discModel, edges );
  }

  private IFE1D2DNode[] createNodes( final IFEDiscretisationModel1d2d discModel, final GM_Position[] positions, final String srsName )
  {
    final IFE1D2DNode[] nodes = new IFE1D2DNode[positions.length - 1];

    for( int i = 0; i < nodes.length; i++ )
    {
      nodes[i] = m_snappedNodes.get( positions[i] );
      if( nodes[i] == null )
      {
        /* no existing node, create a new one */
        final GM_Point point = GeometryFactory.createGM_Point( positions[i], srsName );
        nodes[i] = discModel.createNode( point );

        m_newNodeIDs.add( nodes[i].getId() );
      }
    }

    return nodes;
  }

  private IFE1D2DEdge[] createEdges( final IFEDiscretisationModel1d2d discModel, final IFE1D2DNode[] nodes )
  {
    final IFE1D2DEdge[] edges = new IFE1D2DEdge[nodes.length];

    /* Create edges */
    for( int i = 0; i < nodes.length; i++ )
    {
      final IFE1D2DNode node1 = nodes[i];
      final IFE1D2DNode node2 = nodes[(i + 1) % nodes.length];

      edges[i] = discModel.findEdge( node1, node2 );
      if( edges[i] == null )
      {
        /* Needs to create a new edge */
        edges[i] = discModel.createEdge( node1, node2 );
        m_newEdgeIDs.add( edges[i].getId() );
      }
    }

    return edges;
  }

  private IPolyElement createElement( final IFEDiscretisationModel1d2d discModel, final IFE1D2DEdge[] edges )
  {
    final IPolyElement element = discModel.createElement2D( edges );
    final String elementId = element.getId();
    m_newElementIDs.add( elementId );

    return element;
  }
}