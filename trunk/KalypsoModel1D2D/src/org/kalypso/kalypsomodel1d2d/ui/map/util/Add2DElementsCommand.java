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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Location;
import com.vividsolutions.jts.geom.Polygon;

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

  private final List<GM_PolygonPatch> m_elements;

  private final IFEDiscretisationModel1d2d m_discModel;

  // FIXME: not good using patches instead of rings. We only use the outer ring anyways so this is obscure to the user of this class
  public Add2DElementsCommand( final GMLWorkspace discretisationModel, final List<GM_PolygonPatch> elements )
  {
    m_discModel = (IFEDiscretisationModel1d2d)discretisationModel.getRootFeature();
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
    for( final GM_PolygonPatch ring : m_elements )
      createElementFromRing( ring );

    fireEvents( m_newNodeIDs );
    fireEvents( m_newEdgeIDs );
    fireEvents( m_newElementIDs );
  }

  private void fireEvents( final Collection<String> newFeatureIDs )
  {
    final GMLWorkspace discWorkspace = m_discModel.getWorkspace();
    final Collection<Feature> newFeatures = new ArrayList<>();

    for( final String id : newFeatureIDs )
      newFeatures.add( discWorkspace.getFeature( id ) );

    final Feature[] newFeatureArray = newFeatures.toArray( new Feature[newFeatures.size()] );

    final FeatureStructureChangeModellEvent changeEvent = new FeatureStructureChangeModellEvent( discWorkspace, m_discModel, newFeatureArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
    discWorkspace.fireModellEvent( changeEvent );
  }

  private void createElementFromRing( final GM_PolygonPatch ring ) throws GM_Exception
  {
    final GM_Position[] positions = ring.getExteriorRing();
    final String srsName = ring.getCoordinateSystem();

    // snap to mesh first, else we get problems with the checks later on...
    final GM_Position[] snappedPositions = snapPositions( positions, srsName );

    final GM_Position[] cleanPositions = removeDuplicates( snappedPositions );

    /* Check if positions overlap existing elements */
    if( !checkPositions( cleanPositions, srsName ) )
      return;

    /* Check if ring overlaps existing elements */
    if( !checkElements( cleanPositions, srsName ) )
      return;

    createElement( cleanPositions, srsName );
  }

  private GM_Position[] snapPositions( final GM_Position[] positions, final String srsName )
  {
    final GM_Position[] snappedPositions = new GM_Position[positions.length];

    for( int i = 0; i < snappedPositions.length; i++ )
    {
      final GM_Position position = positions[i];

      final GM_Point nodeLocation = GeometryFactory.createGM_Point( position, srsName );

      final IFE1D2DNode node = m_discModel.findNode( nodeLocation, SNAP_DISTANCE );
      if( node != null )
        snappedPositions[i] = node.getPoint().getPosition();
      else
        snappedPositions[i] = position;
    }

    return snappedPositions;
  }

  private GM_Position[] removeDuplicates( final GM_Position[] positions )
  {
    final Coordinate[] crds = JTSAdapter.export( positions );
    final CoordinateList list = new CoordinateList( crds, false );
    list.closeRing();

    final Coordinate[] cleanCrds = list.toCoordinateArray();
    return JTSAdapter.wrap( cleanCrds );
  }

  private boolean checkElements( final GM_Position[] positions, final String srsName ) throws GM_Exception
  {
    // Probably for performance reason: fast check with centroid
    final IPolyElement element2d = m_discModel.find2DElement( GeometryUtilities.centroidFromRing( positions, srsName ), 0.0 );
    if( element2d != null )
      return false;

    // REMARK: test with surface, because ring has no interior (being a hole)
    final GM_Polygon newSurface = GeometryFactory.createGM_Surface( positions, null, srsName );
    final Geometry newPolygon = JTSAdapter.export( newSurface );

    final List<IFE1D2DElement> foundElements = m_discModel.queryElements( newSurface.getEnvelope(), null );
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

  private boolean checkPositions( final GM_Position[] positions, final String srsName )
  {
    try
    {
      /* Do not check last point, must be equal to first point */
      for( int i = 0; i < positions.length - 1; i++ )
      {
        final GM_Position position = positions[i];
        final Coordinate location = JTSAdapter.export( position );
        final GM_Point nodeLocation = GeometryFactory.createGM_Point( position, srsName );

        /* check if point is inside an element */
        final IPolyElement foundElement = m_discModel.find2DElement( nodeLocation, SNAP_DISTANCE );
        if( foundElement != null )
        {
          final GM_Polygon geometry = foundElement.getGeometry();

          final Polygon polygon = (Polygon)JTSAdapter.export( geometry );

          if( polygon.contains( polygon ) )
          {
            final Coordinate[] exterior = polygon.getExteriorRing().getCoordinates();

            final int locate = CGAlgorithms.locatePointInRing( location, exterior );
            if( locate == Location.INTERIOR )
            {
              // do not insert nodes that are placed on existing model(overlapped elements)
              m_log.add( IStatus.WARNING, Messages.getString( "Add2DElementsCommand_1" ), null, position ); //$NON-NLS-1$
              return false;
            }
          }
        }
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    return true;
  }

  private void createElement( final GM_Position[] positions, final String srsName )
  {
    final IFE1D2DNode[] nodes = createNodes( positions, srsName );

    final IFE1D2DEdge[] edges = createEdges( nodes );

    /* final IPolyElement element = */createElement( edges );
  }

  private IFE1D2DNode[] createNodes( final GM_Position[] positions, final String srsName )
  {
    final IFE1D2DNode[] nodes = new IFE1D2DNode[positions.length - 1];

    for( int i = 0; i < nodes.length; i++ )
    {
      /* no existing node, create a new one */
      final GM_Point point = GeometryFactory.createGM_Point( positions[i], srsName );
      nodes[i] = m_discModel.createNode( point );

      // REMARK: always add node, regardless if it was created, this only leads to a bit more events later on which is not a problem
      m_newNodeIDs.add( nodes[i].getId() );
    }

    return nodes;
  }

  private IFE1D2DEdge[] createEdges( final IFE1D2DNode[] nodes )
  {
    final IFE1D2DEdge[] edges = new IFE1D2DEdge[nodes.length];

    /* Create edges */
    for( int i = 0; i < nodes.length; i++ )
    {
      final IFE1D2DNode node1 = nodes[i];
      final IFE1D2DNode node2 = nodes[(i + 1) % nodes.length];

      edges[i] = m_discModel.findEdge( node1, node2 );
      if( edges[i] == null )
      {
        /* Needs to create a new edge */
        edges[i] = m_discModel.createEdge( node1, node2 );
        m_newEdgeIDs.add( edges[i].getId() );
      }
    }

    return edges;
  }

  private IPolyElement createElement( final IFE1D2DEdge[] edges )
  {
    final IPolyElement element = m_discModel.createElement2D( edges );

    final String elementId = element.getId();
    m_newElementIDs.add( elementId );

    return element;
  }
}