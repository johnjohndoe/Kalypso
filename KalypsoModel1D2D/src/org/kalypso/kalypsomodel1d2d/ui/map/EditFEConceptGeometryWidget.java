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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * {@link IWidget} that provide the mechanism for edition the geometry of finite element concepts (Node, Edge, elements,
 * and Complex elements) This class decorate the {@link EditGeometryWidget} with the capability to :
 * <ul>
 * <li/>find all feature affected by a geometric change in the edited fe concepts;
 * <li/>invalidate the envelops of the found feature
 * <li/>and fire feature change event holding the affected feature
 * </ul>
 * This widget rely on the assumption that the map to edit has layer holding feature with the QName
 * {@link Kalypso1D2DSchemaConstants#WB1D2D_F_NODE}
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * @author Thomas Jung
 */
public class EditFEConceptGeometryWidget extends AbstractWidget
{
  /** Snapping radius in screen-pixels. */
  public static final int SNAPPING_RADIUS = 20;

  private IKalypsoFeatureTheme m_nodeTheme;

  private IFEDiscretisationModel1d2d m_discModel;

  private IKalypsoFeatureTheme m_flowTheme = null;

  private CommandableWorkspace m_flowWorkspace = null;

  private IFlowRelationshipModel m_flowRelModel = null;

  private Point m_currentMapPoint;

  private PointSnapper m_pointSnapper;

  private IFE1D2DNode m_snapNode;

  private boolean m_snappingActive;

  private ElementGeometryEditor m_editor;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private boolean m_warning;

  private Map<String, IFlowRelationship> m_mapElementWithFlowRelationship;

  public EditFEConceptGeometryWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.EditFEConceptGeometryWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.EditFEConceptGeometryWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    super.activate( commandPoster, mapPanel );
    m_nodeTheme = UtilMap.findEditableTheme( mapPanel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );

    m_flowTheme = UtilMap.findEditableTheme( mapPanel, IFlowRelation2D.QNAME );
    if( m_flowTheme == null )
      m_flowTheme = UtilMap.findEditableTheme( mapPanel, IFlowRelationship.QNAME );
    if( m_flowTheme == null || m_discModel == null )
      return;

    final FeatureList featureList = m_flowTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_flowRelModel = (IFlowRelationshipModel) parentFeature.getAdapter( IFlowRelationshipModel.class );
    m_flowWorkspace = m_flowTheme.getWorkspace();
    reinit();
    m_snappingActive = true;

  }

  private void reinit( )
  {
    m_editor = null;

    m_mapElementWithFlowRelationship = new HashMap<String, IFlowRelationship>();

    if( m_nodeTheme != null )
      m_editor = new ElementGeometryEditor( getMapPanel(), m_nodeTheme );

    m_snappingActive = true;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    if( p == null )
      return;

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();

    if( m_editor.getStartNode() == null )
    {
      final GM_Point currentPosition = MapUtilities.transform( panel, p );
      final double snapRadius = MapUtilities.calculateWorldDistance( panel, currentPosition, SNAPPING_RADIUS );
      m_editor.setStartNode( m_discModel.findNode( currentPosition, snapRadius ) );
      collectFlowrelationsInformation();
    }
    else
    {
      if( m_editor.isValid() )
      {
        try
        {
          m_editor.finish();
          setNewPositionsOfFlowrelations();
          getMapPanel().repaintMap();
          reinit();

        }
        catch( final Exception e )
        {
          e.printStackTrace();
          KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
        }
        finally
        {
          repaintMap();
        }
      }
    }
    super.leftPressed( p );
  }

  private void collectFlowrelationsInformation( )
  {
    final IFE1D2DNode startNode = m_editor.getStartNode();
    if( startNode == null )
      return;

    for( final IFE1D2DElement element : startNode.getElements() )
    {
      if( element instanceof IPolyElement )
      {
        final IFlowRelationship lBuilding = FlowRelationUtilitites.findBuildingElement2D( (IPolyElement) element, m_flowRelModel );
        if( lBuilding != null )
        {
          m_mapElementWithFlowRelationship.put( element.getGmlID(), lBuilding );
        }
      }
    }
  }

  @SuppressWarnings({})
  private void setNewPositionsOfFlowrelations( )
  {

    for( final IFE1D2DElement element : m_editor.getStartNode().getElements() )
    {
      if( element instanceof IPolyElement )
      {
        final IFlowRelationship lBuilding = m_mapElementWithFlowRelationship.get( element.getGmlID() );
        if( lBuilding != null )
        {
          final GM_Position lFlowPositionFromElement = FlowRelationUtilitites.getFlowPositionFromElement( element );
          final String crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
          final ChangeFeatureCommand lChangeFeatureCommand = new ChangeFeatureCommand( lBuilding.getFeature(), lBuilding.getFeature().getFeatureType().getProperty( IFlowRelationship.QNAME_PROP_POSITION ), GeometryFactory.createGM_Point( lFlowPositionFromElement, crs ) );
          try
          {
            m_flowWorkspace.postCommand( lChangeFeatureCommand );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.SnapToGeometryWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
      reinit();
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = false;
    else
      super.keyPressed( e );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = true;

    super.keyReleased( e );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.SnapToGeometryWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();

    /* always paint a small rectangle of current position */
    if( m_currentMapPoint != null )
    {
      final int[][] posPoints = UtilMap.getPointArrays( m_currentMapPoint );

      final int[] arrayX = posPoints[0];
      final int[] arrayY = posPoints[1];

      /* Paint as linestring. */
      g.drawPolygon( arrayX, arrayY, arrayX.length );
      UtilMap.drawHandles( g, arrayX, arrayY );

      /* paint the snap */
      if( m_pointSnapper != null )
        m_pointSnapper.paint( g );

      /* paint the preview */
      if( m_editor != null )
        m_editor.paint( g, projection, m_currentMapPoint );
    }

    super.paint( g );

    final Rectangle bounds = mapPanel.getScreenBounds();

    String tooltipMsg = ""; //$NON-NLS-1$
    if( m_snappingActive == true )
      tooltipMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.EditFEConceptGeometryWidget.3" ); //$NON-NLS-1$
    else
      tooltipMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.EditFEConceptGeometryWidget.4" ); //$NON-NLS-1$

    m_toolTipRenderer.setTooltip( tooltipMsg );
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    if( m_warning == true )
      m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 80 ), g, bounds );

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    if( p == null )
      return;

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();

    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme == null || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return;

  }

  private Object checkNewNode( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );

    if( m_snappingActive )
      m_snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );

    final Object newNode = m_snapNode == null ? currentPoint : m_snapNode;

    if( m_editor == null )
      return null;

    if( m_editor.getStartNode() != null )
    {
      final IStatus status = m_editor.checkNewNode( newNode );
      if( status.isOK() )
        m_warning = false;
      else
      {
        m_warning = true;
        m_warningRenderer.setTooltip( status.getMessage() );
      }

      if( status.isOK() )
        return newNode;
    }
    else
      return newNode;

    return null;
  }
}
