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
package org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.DeprecatedMouseWidget;
import org.kalypso.ui.editor.gmleditor.part.GMLLabelProvider;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Polygon;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Stefan Kurzbach
 * @author Dejan Antanaskovic
 *
 *         Wraps a {@link CreateGeometeryWidget2} for 0-argument constructor instantiation
 */
public class CreatePolygonWidgetWrapper extends DeprecatedMouseWidget
{
  private final ToolTipRenderer m_warningRenderer = ToolTipRenderer.createWarningTooltip();

  private final ToolTipRenderer m_toolTipRenderer = ToolTipRenderer.createWarningTooltip();

  private PolygonGeometryBuilder m_builder = null;

  private Point m_currentPoint = null;

  private IRoughnessLayer m_roughnessLayer = null;

  private CommandableWorkspace m_workspace;

  public CreatePolygonWidgetWrapper( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.CreatePolygonWidgetWrapper.0" ), StringUtils.EMPTY ); //$NON-NLS-1$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    reset();
  }

  private void reset( )
  {
    m_builder = null;
    m_roughnessLayer = null;
    m_workspace = null;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return;

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( !(activeTheme instanceof IKalypsoFeatureTheme) )
      return;

    final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) activeTheme;
    final FeatureList featureList = featureTheme.getFeatureList();
    final Feature parentFeature = featureList.getOwner();
    if( parentFeature == null )
      return;

    m_workspace = featureTheme.getWorkspace();
    m_roughnessLayer = (IRoughnessLayer) parentFeature.getAdapter( IRoughnessLayer.class );
    if( m_roughnessLayer == null )
      return;

    m_builder = new PolygonGeometryBuilder( 0, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

    repaintMap();
  }

  private boolean check( )
  {
    return m_builder != null;
  }

  @Override
  public void paint( final Graphics g )
  {
    if( !check() )
      return;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();
    if( projection == null )
      return;

    if( m_currentPoint == null )
      return;

    if( m_builder.getPointCount() == 0 )
    {
      PolygonGeometryBuilder.drawHandle( g, m_currentPoint.x, m_currentPoint.y );
    }

    final Rectangle screenBounds = mapPanel.getScreenBounds();
    m_toolTipRenderer.setTooltip( Messages.getString( "CreatePolygonWidgetWrapper.3" ) ); //$NON-NLS-1$
    m_toolTipRenderer.paintToolTip( new Point( 5, screenBounds.height - 5 ), g, screenBounds );

    final GM_Point pos = MapUtilities.transform( getMapPanel(), m_currentPoint );
    final String warning = validateGeometry( pos );

    if( warning == null )
      g.setColor( Color.GREEN );
    else
    {
      final Point bottomLeft = new Point( 5, screenBounds.height - 92 );
      m_warningRenderer.setTooltip( warning );
      m_warningRenderer.paintToolTip( bottomLeft, g, screenBounds );

      g.setColor( Color.RED );
    }

    m_builder.paint( g, projection, m_currentPoint );
  }

  private String validateGeometry( final GM_Point pos )
  {
    try
    {
      if( pos != null )
        m_builder.addPoint( pos );

      try
      {
        final int pointCount = m_builder.getPointCount();
        if( pointCount < 3 && pointCount > 1 )
          return Messages.getString( "CreatePolygonWidgetWrapper.0" ); //$NON-NLS-1$

        final GM_Object geom = m_builder.finish();
        final Polygon export = (Polygon) JTSAdapter.export( geom );
        if( !export.isValid() )
          return Messages.getString( "CreatePolygonWidgetWrapper.1" ); //$NON-NLS-1$

        return null;
      }
      finally
      {
        if( pos != null )
          m_builder.removeLastPoint();
      }
    }
    catch( final Exception e )
    {
      return e.getLocalizedMessage();
    }
  }

  @Override
  public void moved( final Point p )
  {
    if( !check() )
      return;

    m_currentPoint = p;

    repaintMap();
  }

  @Override
  public void leftPressed( final Point p )
  {
    if( !check() )
      return;

    try
    {
      final GM_Point pos = MapUtilities.transform( getMapPanel(), p );
      m_builder.addPoint( pos );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    repaintMap();
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    switch( e.getKeyCode() )
    {
      case KeyEvent.VK_BACK_SPACE:
        m_builder.removeLastPoint();
        repaintMap();
        break;

      case KeyEvent.VK_ESCAPE:
        reset();
        break;
    }
  }

  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( !check() )
      return;

    final String warning = validateGeometry( null );
    if( warning == null )
      createRoughnessZone();
    else
    {
      // We do not reset in order to let the user recover from the problem
      SWT_AWT_Utilities.showSwtMessageBoxWarning( getName(), warning );
    }
  }

  private void createRoughnessZone( )
  {
    try
    {
      final GM_Object poly = m_builder.finish();

      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final ITerrainModel model = dataProvider.getModel( ITerrainModel.class.getName() );

      final IRoughnessClsCollection clsCollection = dataProvider.getModel( IRoughnessClsCollection.class.getName() );
      final Object[] classes = clsCollection.getRoughnessClasses().toArray();

      /* let user choose roughness class */
      final Shell shell = SWT_AWT_Utilities.findActiveShell();
      final ElementListSelectionDialog dialog = new ElementListSelectionDialog( shell, new GMLLabelProvider() );
      dialog.setElements( classes );
      dialog.setMessage( Messages.getString( "CreatePolygonWidgetWrapper.2" ) ); //$NON-NLS-1$
      dialog.setMultipleSelection( false );
      if( SWT_AWT_Utilities.openSwtWindow( dialog ) != Window.OK )
        return;

      final IRoughnessCls selectedClass = (IRoughnessCls) dialog.getFirstResult();

      /* Add new polygon */
      final IRoughnessPolygonCollection roughnessPolygonCollection = model.getRoughnessPolygonCollection( m_roughnessLayer );
      final IRoughnessPolygon newZone = roughnessPolygonCollection.addNew( IRoughnessPolygon.QNAME );

      newZone.setSurface( (GM_Polygon) poly );
      newZone.setRoughnessClass( selectedClass );

      /* Inform workspace */
      final GMLWorkspace workspace = newZone.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_roughnessLayer, newZone, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

      /* Make model dirty */
      m_workspace.postCommand( new EmptyCommand( StringUtils.EMPTY, false ) );

      // TODO Optional: select feature and open feature view
    }
    catch( final Exception e )
    {
      // TODO error handling...
      e.printStackTrace();
    }
    finally
    {
      reset();
    }
  }
}