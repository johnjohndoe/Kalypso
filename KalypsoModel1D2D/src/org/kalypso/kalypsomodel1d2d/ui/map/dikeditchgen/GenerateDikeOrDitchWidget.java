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
import java.nio.charset.Charset;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.afgui.model.Util;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.TriangulateGeometryComposite;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.ShapeWriter;
import org.kalypso.shape.data.SimpleShapeData;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.deegree.GM_Object2Shape;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;

/**
 * @author kurzbach
 */
public class GenerateDikeOrDitchWidget extends AbstractWidget implements IWidgetWithOptions
{
  private static final CreateStructuredNetworkStrategy DEFAULT_STRATEGY = new CreateDitchStrategy();

  private CommandableWorkspace m_discModelWorkspace;

  private PointSnapper m_pointSnapper;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private PolygonGeometryBuilder m_networkBuilder;

  private Point m_currentPoint;

  private TriangulationBuilder m_tinBuilder;

  private double m_currentZ = Double.NaN;

  private final CreateStructuredNetworkStrategy m_strategy = DEFAULT_STRATEGY;

  public GenerateDikeOrDitchWidget( )
  {
    super( "org.kalypso.model.1d2d.workflow.DikeDitchGen" ); //$NON-NLS-1$

    // parameters
//  final double outerLeftWidth = 60;
//  final double outerRightWidth = 12;
//  final double innerWidth = 4;
//  final double innerElevation = 5;
//  m_strategy = new CreateDikeStrategy( features, outerLeftWidth, outerRightWidth, innerWidth, innerElevation, terrainElevationModelSystem );

  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final ScrolledForm form = toolkit.createScrolledForm( parent );
    final DatabindingForm binding = new DatabindingForm( form, toolkit );
    final Composite body = form.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Section section = toolkit.createSection( body, ExpandableComposite.EXPANDED | ExpandableComposite.TITLE_BAR );
      section.setText( "Network options" );
      section.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

      final Composite sectionComposite = toolkit.createComposite( section, SWT.NONE );
      GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( sectionComposite );
      section.setClient( sectionComposite );

      // network layer
      final IKalypsoLayerModell mapModell = mapPanel.getMapModell();
      m_strategy.createControl( sectionComposite, toolkit, mapModell );

      final CreateStructuredNetworkAction runStrategyAction = new CreateStructuredNetworkAction( m_tinBuilder, m_strategy );
      final Button buttonRunStrategy = ActionButton.createButton( toolkit, sectionComposite, runStrategyAction );
      buttonRunStrategy.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    }

    // triangulation parameters
    final TriangulateGeometryComposite triangulateGeometryComposite = new TriangulateGeometryComposite( toolkit, binding, m_tinBuilder, m_discModelWorkspace );
    triangulateGeometryComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    return body;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    m_discModelWorkspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );
    m_tinBuilder = new TriangulationBuilder( mapPanel );
    final IKalypsoLayerModell mapModell = mapPanel.getMapModell();
    final String coordinateSystem = mapModell.getCoordinatesSystem();
    final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d)m_discModelWorkspace.getRootFeature();
    m_pointSnapper = new PointSnapper( discModel, mapPanel );
    m_networkBuilder = new PolygonGeometryBuilder( 0, coordinateSystem );
    reinit();
  }

  private void reinit( )
  {
    final String mode = "Graben oder Deich zeichnen";
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_toolTipRenderer.setTooltip( mode );
    m_networkBuilder.reset();
    m_tinBuilder.setMinAngle( 22.5 );
    m_tinBuilder.setNoSteinerOnBoundary( true );
    m_tinBuilder.reset();
    repaintMap();
  }

  private GM_Point snapTo( final Point point )
  {
    final IMapPanel mapPanel = getMapPanel();

    GM_Point currentOrSnappedPoint = null;
    final GM_Point currentPoint = MapUtilities.transform( mapPanel, point );

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
    final GM_TriangulatedSurface tin = m_tinBuilder.getTin();
    if( snapPoint != null && tin != null )
    {
      m_currentZ = tin.getValue( snapPoint );
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
      final GM_Polygon boundary = (GM_Polygon)m_networkBuilder.finish();
      if( boundary == null )
        return;
      m_tinBuilder.addBoundary( boundary );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      reinit();
    }
    finally
    {
      m_networkBuilder.reset();
      repaintMap();
    }
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
      reinit();
    else if( e.getKeyCode() == KeyEvent.VK_E )
    {
      try
      {
        final GM_TriangulatedSurface tin = m_tinBuilder.getTin();
        final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
        final GM_Object2Shape gm_Object2Shape = new GM_Object2Shape( ShapeType.POLYGONZ, coordinateSystem );
        final ISHPGeometry shapeTin = gm_Object2Shape.convert( tin );
        final SimpleShapeData dataProvider = new SimpleShapeData( Charset.defaultCharset(), coordinateSystem, ShapeType.POLYGONZ, new IDBFField[0] );
        dataProvider.addRow( shapeTin, new Object[0] );
        final ShapeWriter shapeWriter = new ShapeWriter( dataProvider );
        shapeWriter.write( "d:/scratch/polder_tin", null ); //$NON-NLS-1$
      }
      catch( final Exception ex )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( ex );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        reinit();
      }
    }
    else if( e.getKeyCode() == KeyEvent.VK_BACK_SPACE )
      m_networkBuilder.removeLastPoint();
    repaintMap();
  }

  @Override
  public void paint( final Graphics g, final GeoTransform projection, final IProgressMonitor monitor )
  {
    if( m_tinBuilder != null )
      try
      {
        m_tinBuilder.paint( g, projection, monitor );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();

    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );

    final GeoTransform projection = mapPanel.getProjection();
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
        m_toolTipRenderer.setTooltip( String.format( "%.2f", m_currentZ ) ); //$NON-NLS-1$
        m_toolTipRenderer.paintToolTip( m_currentPoint, g, mapPanel.getScreenBounds() );
      }
    }
  }

  @Override
  public void disposeControl( )
  {
    // make sure that the tin is disposed
    m_tinBuilder.reset();
  }

  @Override
  public String getPartName( )
  {
    return getName();
  }
}
