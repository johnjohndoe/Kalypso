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
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Location;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.PrecisionModel;
import com.vividsolutions.jts.geomgraph.Label;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;
import com.vividsolutions.jts.noding.NodedSegmentString;
import com.vividsolutions.jts.noding.SegmentString;
import com.vividsolutions.jts.noding.snapround.MCIndexSnapRounder;
import com.vividsolutions.jts.operation.linemerge.LineMerger;
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier;

/**
 * @author kurzbach
 */
public class GenerateDikeOrDitchWidget extends AbstractWidget implements IWidgetWithOptions
{
//  private enum WidgetState
//  {
//    DRAW_NETWORK
//  }

  private IFEDiscretisationModel1d2d m_discModel;

  private PointSnapper m_pointSnapper;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private LineGeometryBuilder m_networkBuilder;

  private Point m_currentPoint;

  final TriangulationBuilder m_tinBuilder = new TriangulationBuilder();

  private Geometry m_network;

  private GM_TriangulatedSurface m_tin;

  private double m_currentZ = Double.NaN;

  public GenerateDikeOrDitchWidget( )
  {
    super( "org.kalypso.model.1d2d.workflow.DikeDitchGen" );
    m_tinBuilder.setMinAngle( 25 );
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    final IKalypsoLayerModell mapModell = mapPanel.getMapModell();
    final String coordinateSystem = mapModell.getCoordinatesSystem();
    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );
    m_networkBuilder = new LineGeometryBuilder( 0, coordinateSystem );
    reinit();
  }

  private void reinit( )
  {
    final String mode = "Graben oder Deich zeichnen";
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_toolTipRenderer.setTooltip( mode );
    m_networkBuilder.reset();
    m_tinBuilder.reset();
    m_network = JTSAdapter.jtsFactory.createMultiLineString( new LineString[0] );
    repaintMap();
  }

  private GM_Point snapTo( final Point point )
  {
    final IMapPanel mapPanel = getMapPanel();

    GM_Point currentOrSnappedPoint = null;
    final GM_Point currentPoint = MapUtilities.transform( mapPanel, point );
    if( m_network != null && !m_network.isEmpty() )
    {
      try
      {
        final Coordinate jtsPoint = JTSAdapter.export( currentPoint.getPosition() );
        final double buffer = MapUtilities.calculateWorldDistance( mapPanel, currentPoint, 10 );
        final LocationIndexedLine locationIndexedLine = new LocationIndexedLine( m_network );
        final LinearLocation projected = locationIndexedLine.project( jtsPoint );
        projected.snapToVertex( m_network, buffer );
        if( projected.isValid( m_network ) )
        {
          final Coordinate extractPoint = locationIndexedLine.extractPoint( projected );
          if( extractPoint.distance( jtsPoint ) < buffer )
            currentOrSnappedPoint = GeometryFactory.createGM_Point( JTSAdapter.wrap( extractPoint ), currentPoint.getCoordinateSystem() );
        }
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        reinit();
      }
    }

    if( m_pointSnapper != null )
    {
      final IFE1D2DNode snapNode = m_pointSnapper.moved( currentPoint );
      if( snapNode != null )
        currentOrSnappedPoint = snapNode.getPoint();
    }

    mapPanel.setCursor( Cursor.getDefaultCursor() );
    if( currentOrSnappedPoint == null )
    {
      m_currentPoint = point;
      currentOrSnappedPoint = currentPoint;
    }
    else
    {
      m_currentPoint = MapUtilities.retransform( mapPanel, currentOrSnappedPoint );
      mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    }

    return currentOrSnappedPoint;
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    final GM_Point snapPoint = snapTo( event.getPoint() );
    if( m_tin != null )
    {
      m_currentZ = m_tin.getValue( snapPoint );
    }
    repaintMap();
  }

  @Override
  public void mousePressed( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 || event.getClickCount() > 1 )
      return;

    try
    {
      final GM_Point snapPoint = snapTo( event.getPoint() );
      m_networkBuilder.addPoint( snapPoint );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      reinit();
    }
  }

  @Override
  public void mouseClicked( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 || event.getClickCount() < 2 )
      return;

    try
    {
      final GM_Curve curve = (GM_Curve)m_networkBuilder.finish();
      if( curve == null )
        return;

      m_network = m_network.union( JTSAdapter.export( curve ) );

      final int networkSize = m_network.getNumGeometries();
      final List<SegmentString> segmentList = new ArrayList<>( networkSize );
      for( int i = 0; i < networkSize; i++ )
      {
        final LineString linestring = (LineString)m_network.getGeometryN( i );
        segmentList.add( new NodedSegmentString( linestring.getCoordinates(), new Label( 0, Location.BOUNDARY, Location.EXTERIOR, Location.INTERIOR ) ) );
      }

      // from now on work in reduced precision
      final PrecisionModel pm = new PrecisionModel( 1000 );

      final MCIndexSnapRounder snapRounder = new MCIndexSnapRounder( pm );
      snapRounder.computeNodes( segmentList );
      snapRounder.computeVertexSnaps( segmentList );
      final Collection<SegmentString> nodedSubstrings = snapRounder.getNodedSubstrings();

      final LineMerger merger = new LineMerger();
      for( SegmentString segmentString : nodedSubstrings )
      {
        final Coordinate[] coordinates = segmentString.getCoordinates();
        if( coordinates.length > 1 )
          merger.add( JTSAdapter.jtsFactory.createLineString( coordinates ) );
      }

      m_network = JTSAdapter.jtsFactory.buildGeometry( merger.getMergedLineStrings() );

      JTSAdapter.jtsFactory = new com.vividsolutions.jts.geom.GeometryFactory( pm );
      createDikeOrDitch();
      JTSAdapter.jtsFactory = new com.vividsolutions.jts.geom.GeometryFactory();
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      reinit();
    }
    finally
    {
//      JTSAdapter.jtsFactory = new com.vividsolutions.jts.geom.GeometryFactory();
      m_networkBuilder.reset();
      repaintMap();
    }
  }

  private void createDikeOrDitch( )
  {
    try
    {
      m_tinBuilder.reset();

      final int networkSize = m_network.getNumGeometries();
      if( networkSize == 0 )
        return;

      final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      // width parameters
      final double outerLeftWidth = 30;
      final double outerRightWidth = 62;
//      final double maxOuterWidth = Math.max( outerLeftWidth, outerRightWidth );
//      final double innerOuterFrac = 0.01; // inner width as fraction of outer width
//      final double innerWidth = (outerWidth * innerOuterFrac) / 2;
      final double innerWidth = 2; // same left and right
//      final double innerOuterFrac = innerWidth / maxOuterWidth;

      // elevations
//      final double innerElevation = 8; // TODO: minimum depth/height
//      final double outerElevation = 0;
//      final double elevationDifference = outerElevation - innerElevation;
//      final double maximumElevationDifferencePerRing = 3;

      final double[] ringDistancesLeft = getRingDistances( innerWidth, outerLeftWidth );
      final int ringCountLeft = ringDistancesLeft.length;
      final double[] ringDistancesRight = getRingDistances( innerWidth, outerRightWidth );
      final int ringCountRight = ringDistancesRight.length;
      final int ringCount = ringCountLeft < ringCountRight ? ringCountLeft : ringCountRight;

      // get outer boundary
      final Polygon outer = getRingPoly( outerLeftWidth, outerRightWidth );
      final double outerDensifyTol = innerWidth * 2 * Math.pow( 2, ringCount );
      final Polygon outerDensified = (Polygon)Densifier.densify( outer, outerDensifyTol );
      final GM_Polygon outerRing = (GM_Polygon)JTSAdapter.wrap( outerDensified, coordinateSystem );
      m_tinBuilder.setBoundary( outerRing, false );

      // add network segments as breaklines
      double innerDensifyTolerance = innerWidth * 2;
      final boolean addNetworkAsBreakline = false;
      if( addNetworkAsBreakline )
      {
        final Geometry densifiedNetwork = Densifier.densify( m_network, innerDensifyTolerance );
        for( int i = 0; i < networkSize; i++ )
        {
          final LineString linestring = (LineString)densifiedNetwork.getGeometryN( i );
          final GM_Curve edgeCurve = (GM_Curve)JTSAdapter.wrap( linestring, coordinateSystem );
          m_tinBuilder.addBreakLine( edgeCurve, false );
        }
      }
      else
        innerDensifyTolerance = innerDensifyTolerance * 2;

      // add inner polygon rings
      for( int j = 0; j < ringCount; j++ )
      {
        // add all inner polygon rings as breaklines
        final double ringLeftWidth = ringDistancesLeft[j];
        final double ringRightWidth = ringDistancesRight[j];
        final Polygon inner = getRingPoly( ringLeftWidth, ringRightWidth );
        final Polygon innerDensified = (Polygon)Densifier.densify( inner, innerDensifyTolerance );
        final GM_Polygon innerRing = (GM_Polygon)JTSAdapter.wrap( innerDensified, coordinateSystem );
        final GM_PolygonPatch surfacePatch = innerRing.getSurfacePatch();
        final GM_Position[] exteriorRing = surfacePatch.getExteriorRing();
        final GM_Curve innerRingExteriorAsCurve = GeometryFactory.createGM_Curve( exteriorRing, coordinateSystem );
        m_tinBuilder.addBreakLine( innerRingExteriorAsCurve, false );
        final GM_Position[][] interiorRings = surfacePatch.getInteriorRings();
        if( interiorRings != null )
        {
          for( final GM_Position[] ring : interiorRings )
          {
            final GM_Curve innerRingHoleAsCurve = GeometryFactory.createGM_Curve( ring, coordinateSystem );
            m_tinBuilder.addBreakLine( innerRingHoleAsCurve, false );
          }
        }
        // double densification distance with each inner ring
        innerDensifyTolerance = innerDensifyTolerance * 2;
      }

      // finalize mesh
      m_tinBuilder.finish();
      m_tin = m_tinBuilder.getTin();
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  private double[] getRingDistances( final double innerWidth, final double outerWidth )
  {
    final double dieoffLeft = new BigDecimal( Math.log( 1 + outerWidth / innerWidth ) ).divide( new BigDecimal( Math.log( 2 ) ), RoundingMode.HALF_UP ).doubleValue() - 1;
    final int ringCount = Math.max( (int)Math.floor( dieoffLeft ), 1 );
    final double[] ringDistancesLeft = new double[ringCount];
    ringDistancesLeft[0] = innerWidth;
    for( int j = 1; j < ringCount; j++ )
      ringDistancesLeft[j] = ringCount / dieoffLeft * innerWidth * (Math.pow( 2, j + 1 ) - 1);
    return ringDistancesLeft;
  }

  private Polygon getRingPoly( final double leftWidth, final double rightWidth )
  {
    final Geometry buffer = LineStringBufferBuilder.buffer( m_network, leftWidth, rightWidth );
    if( buffer.getNumGeometries() == 1 )
      return (Polygon)DouglasPeuckerSimplifier.simplify( buffer.getGeometryN( 0 ), Math.max( leftWidth, rightWidth ) / 10 );
    else
      throw new IllegalStateException( "Network is not simply-connected." );
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
      reinit();
    else if( e.getKeyCode() == KeyEvent.VK_BACK_SPACE )
      m_networkBuilder.removeLastPoint();
    repaintMap();
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    final GeoTransform projection = mapPanel.getProjection();

    m_tinBuilder.paint( g, projection );

    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );

    if( m_networkBuilder != null )
      m_networkBuilder.paint( g, projection, m_currentPoint );

    if( m_currentPoint != null )
    {
      final int lowX = (int)m_currentPoint.getX() - 5;
      final int lowY = (int)m_currentPoint.getY() - 5;
      final Color color = g.getColor();
      final Color preViewColor = new Color( 50, 50, 255 );
      g.setColor( preViewColor );
      g.drawRect( lowX, lowY, 10, 10 );
      g.setColor( color );

      if( !Double.isNaN( m_currentZ ) )
      {
        m_toolTipRenderer.setTooltip( String.format( "%.2f", m_currentZ ) );
        m_toolTipRenderer.paintToolTip( m_currentPoint, g, mapPanel.getScreenBounds() );
      }
    }
  }

  @Override
  public void disposeControl( )
  {
    // TODO Auto-generated method stub

  }

  @Override
  public String getPartName( )
  {
    // TODO Auto-generated method stub
    return null;
  }
}
