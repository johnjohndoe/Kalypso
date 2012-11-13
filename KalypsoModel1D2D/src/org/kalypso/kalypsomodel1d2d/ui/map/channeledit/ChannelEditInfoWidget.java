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

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ISegmentData;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
class ChannelEditInfoWidget extends AbstractWidget
{
  private static final int SNAP_RADIUS = 10; //$NON-NLS-1$

  private final SLDPainter2 m_heightVertexPainter = new SLDPainter2( getClass().getResource( "resources/meshInfoWithHeight.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_missingHeightVertexPainter = new SLDPainter2( getClass().getResource( "resources/meshInfoNoHeight.sld" ) ); //$NON-NLS-1$

  private CoordinateIndex m_index = new CoordinateIndex();

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
    m_index = new CoordinateIndex();

    /* add all to the index */
    final ISegmentData[] segments = m_data.getSegments();
    for( final ISegmentData segment : segments )
    {
      final QuadMesh mesh = segment.getMesh();
      addMeshToIndex( mesh );
    }
  }

  private void addMeshToIndex( final QuadMesh mesh )
  {
    if( mesh == null )
      return;

    final Coordinate[][] grid = mesh.getGrid();
    for( final Coordinate[] line : grid )
    {
      for( final Coordinate point : line )
        m_index.add( point );
    }
  }

  private GM_Point snapVertex( final GM_Point currentLocation, final double snapRadius )
  {
    final GM_Position currentPosition = currentLocation.getPosition();
    final Coordinate currentCrd = JTSAdapter.export( currentPosition );

    final Coordinate snappedPoint = m_index.findNearestPoint( currentCrd, (float)snapRadius );
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
        m_toolTipRenderer.setTooltip( Messages.getString( "ChannelEditInfoWidget_2" ) ); //$NON-NLS-1$
        m_toolTipRenderer.paintToolTip( new Point( m_currentPoint.x + 5, m_currentPoint.y - 5 ), g, screenRect );
      }

      return;
    }

    final double z = m_snappedVertex.getZ();
    if( Double.isNaN( z ) )
    {
      m_missingHeightVertexPainter.paint( g, projection, m_snappedVertex );
      m_toolTipRenderer.setTooltip( Messages.getString( "ChannelEditInfoWidget_3" ) ); //$NON-NLS-1$
    }
    else
    {
      m_heightVertexPainter.paint( g, projection, m_snappedVertex );
      m_toolTipRenderer.setTooltip( String.format( Messages.getString( "ChannelEditInfoWidget_4" ), z ) ); //$NON-NLS-1$
    }

    final Point snappedLocation = MapUtilities.retransform( panel, m_snappedVertex );

    m_toolTipRenderer.paintToolTip( new Point( snappedLocation.x + 5, snappedLocation.y - 5 ), g, screenRect );
  }
}