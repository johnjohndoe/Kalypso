/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.jts.JTSEnvelopeProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ISegmentData;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.SplitSort;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
class ChannelEditInfoWidget extends AbstractWidget
{
  private static final int SNAP_RADIUS = 10; //$NON-NLS-1$

  private final SLDPainter2 m_heightVertexPainter = new SLDPainter2( getClass().getResource( "resources/meshInfoWithHeight.sld" ) );

  private final SLDPainter2 m_missingHeightVertexPainter = new SLDPainter2( getClass().getResource( "resources/meshInfoNoHeight.sld" ) );

  private final Collection<QuadMesh> m_indexedMeshes = new HashSet<>();

  private final FeatureList m_index = new SplitSort( null, null, new JTSEnvelopeProvider() );

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ChannelEditData m_data;

  private GM_Point m_snappedVertex;

  private Point m_currentPoint;

  public ChannelEditInfoWidget( final ChannelEditData data )
  {
    super( "Info", "Info" ); //$NON-NLS-1$ //$NON-NLS-2$

    m_data = data;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    updateIndex();
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    updateIndex();

    m_currentPoint = event.getPoint();
    final GM_Point currentLocation = MapUtilities.transform( getMapPanel(), m_currentPoint );

    final double snapRadius = MapUtilities.calculateWorldDistance( getMapPanel(), SNAP_RADIUS );

    m_snappedVertex = snapVertex( currentLocation, snapRadius );

    repaintMap();
  }

  /* update index with current meshes */
  private void updateIndex( )
  {
    /* theses meshes need to be remove from our index */
    final Collection<QuadMesh> meshesToRemoveFromIndex = new HashSet<>( m_indexedMeshes );

    /* theses meshes need to be added to our index */
    final Collection<QuadMesh> meshesToAddToIndex = new HashSet<>();

    /* check which ones to add/remove */
    final ISegmentData[] segments = m_data.getSegments();
    for( final ISegmentData segment : segments )
    {
      final QuadMesh mesh = segment.getMesh();
      if( m_indexedMeshes.contains( mesh ) )
        meshesToRemoveFromIndex.remove( mesh );
      else if( mesh != null )
        meshesToAddToIndex.add( mesh );
    }

    /* remove obsolete meshes */
    for( final QuadMesh mesh : meshesToRemoveFromIndex )
      removeMeshFromIndex( mesh );

    /* add new meshes */
    for( final QuadMesh mesh : meshesToAddToIndex )
      addMeshToIndex( mesh );
  }

  private void removeMeshFromIndex( final QuadMesh mesh )
  {
    final Coordinate[][] grid = mesh.getGrid();
    for( final Coordinate[] line : grid )
    {
      for( final Coordinate point : line )
        m_index.remove( point );
    }

    m_indexedMeshes.remove( mesh );
  }

  private void addMeshToIndex( final QuadMesh mesh )
  {
    final Coordinate[][] grid = mesh.getGrid();
    for( final Coordinate[] line : grid )
    {
      for( final Coordinate point : line )
        m_index.add( point );
    }

    m_indexedMeshes.add( mesh );
  }

  private GM_Point snapVertex( final GM_Point currentLocation, final double snapRadius )
  {
    final GM_Position currentPosition = currentLocation.getPosition();
    final GM_Envelope currentRect = GeometryFactory.createGM_Envelope( currentPosition, currentPosition, currentLocation.getCoordinateSystem() );
    final GM_Envelope searchRect = currentRect.getBuffer( snapRadius );
    final Coordinate currentCrd = JTSAdapter.export( currentPosition );

    final List<Coordinate> query = m_index.query( searchRect, null );

    /* find and return nearest point */
    double minDistance = Double.MAX_VALUE;
    Coordinate snappedPoint = null;

    for( final Coordinate point : query )
    {
      final double distance = point.distance( currentCrd );
      if( distance < minDistance )
      {
        minDistance = distance;
        snappedPoint = point;
      }
    }

    if( snappedPoint == null )
      return null;

    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    return GeometryFactory.createGM_Point( snappedPoint.x, snappedPoint.y, snappedPoint.z, kalypsoSRS );
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel panel = getMapPanel();
    if( panel == null )
      return;

    final GeoTransform projection = panel.getProjection();
    final Rectangle screenRect = projection.getDestRect();

    m_toolTipRenderer.setTooltip( null );

    if( m_snappedVertex == null )
    {
      if( m_index.size() == 0 && m_currentPoint != null )
      {
        m_toolTipRenderer.setTooltip( "<no mesh available>" );
        m_toolTipRenderer.paintToolTip( new Point( m_currentPoint.x + 5, m_currentPoint.y - 5 ), g, screenRect );
      }

      return;
    }

    final double z = m_snappedVertex.getZ();
    if( Double.isNaN( z ) )
    {
      m_missingHeightVertexPainter.paint( g, projection, m_snappedVertex );
      m_toolTipRenderer.setTooltip( "<missing z value>" );
    }
    else
    {
      m_heightVertexPainter.paint( g, projection, m_snappedVertex );
      m_toolTipRenderer.setTooltip( String.format( "z = %.2f [mNN]", z ) );
    }

    final Point snappedLocation = MapUtilities.retransform( panel, m_snappedVertex );

    m_toolTipRenderer.paintToolTip( new Point( snappedLocation.x + 5, snappedLocation.y - 5 ), g, screenRect );
  }
}