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
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.internal.validation.ValidateDiscretisationOperation;
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
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.KalypsoDeegreePlugin;
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
 * This widget rely on the assumption that the map to edit has layer holding feature with the QName {@link Kalypso1D2DSchemaConstants#WB1D2D_F_NODE}
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

  private ElementGeometryEditor m_editor;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  public EditFEConceptGeometryWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.EditFEConceptGeometryWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.EditFEConceptGeometryWidget.0" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    final String tooltipMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.EditFEConceptGeometryWidget.3" ); //$NON-NLS-1$
    m_toolTipRenderer.setTooltip( tooltipMsg );
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
    final Feature parentFeature = featureList.getOwner();

    m_flowRelModel = (IFlowRelationshipModel)parentFeature.getAdapter( IFlowRelationshipModel.class );
    m_flowWorkspace = m_flowTheme.getWorkspace();

    reinit();
  }

  private void reinit( )
  {
    m_editor = null;

    if( m_nodeTheme != null )
      m_editor = new ElementGeometryEditor( getMapPanel(), m_nodeTheme );
    else
      m_editor = null;

    m_warningRenderer.setTooltip( null );

    /* show tooltip after activation, directly clean after ESC */
    repaintMap();
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    if( event.getButton() != 0 )
      return;

    event.consume();

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_editor == null )
      return;

    // REMARK: the first point must always snap: we want a node!
    // the second point never snaps, makes no real sense, because it is not possible to collapse elements by this action
    final boolean snappingActive = m_editor.getStartNode() == null;

    final GM_Point snappedPoint = snapToNode( mapPanel, event.getPoint(), snappingActive );
    if( snappedPoint == null )
    {
      mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
      return;
    }

    m_currentMapPoint = MapUtilities.retransform( mapPanel, snappedPoint );

    mapPanel.setCursor( Cursor.getDefaultCursor() );

    repaintMap();
  }

  @Override
  public void mousePressed( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;

    event.consume();

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final boolean snappingActive = !event.isShiftDown();
    final GM_Point snappedPoint = snapToNode( mapPanel, event.getPoint(), snappingActive );
    if( snappedPoint == null )
    {
      mapPanel.setCursor( Cursor.getDefaultCursor() );
      return;
    }

    m_currentMapPoint = MapUtilities.retransform( mapPanel, snappedPoint );

    mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );

    if( m_editor.getStartNode() == null )
    {
      final GM_Point currentPosition = MapUtilities.transform( mapPanel, event.getPoint() );
      final double snapRadius = MapUtilities.calculateWorldDistance( mapPanel, currentPosition, SNAPPING_RADIUS );
      final IFE1D2DNode startNode = m_discModel.findNode( currentPosition, snapRadius );
      m_editor.setStartNode( startNode );
    }
    else
    {
      if( m_editor.isValid() )
        doMoveNode();
    }

    repaintMap();
  }

  private void doMoveNode( )
  {
    try
    {
      final IFE1D2DNode startNode = m_editor.getStartNode();
      if( startNode == null )
        return;

      final Map<String, IFlowRelationship> elementWithFlowRelationship = collectFlowrelationsInformation( startNode );
      m_editor.finish( m_discModel );
      setNewPositionsOfFlowrelations( elementWithFlowRelationship );

      reinit();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  private Map<String, IFlowRelationship> collectFlowrelationsInformation( final IFE1D2DNode startNode )
  {
    final Map<String, IFlowRelationship> elementWithFlowRelationship = new HashMap<>();

    for( final IFE1D2DElement element : startNode.getAdjacentElements() )
    {
      if( element instanceof IPolyElement )
      {
        final IFlowRelationship lBuilding = FlowRelationUtilitites.findBuildingElement2D( (IPolyElement)element, m_flowRelModel );
        if( lBuilding != null )
        {
          elementWithFlowRelationship.put( element.getId(), lBuilding );
        }
      }
    }

    return elementWithFlowRelationship;
  }

  private void setNewPositionsOfFlowrelations( final Map<String, IFlowRelationship> elementWithFlowRelationship )
  {
    for( final IFE1D2DElement element : m_editor.getStartNode().getAdjacentElements() )
    {
      if( element instanceof IPolyElement )
      {
        final IFlowRelationship lBuilding = elementWithFlowRelationship.get( element.getId() );
        if( lBuilding != null )
        {
          final GM_Position lFlowPositionFromElement = FlowRelationUtilitites.getFlowPositionFromElement( element );
          final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
          final ChangeFeatureCommand lChangeFeatureCommand = new ChangeFeatureCommand( lBuilding, lBuilding.getFeatureType().getProperty( IFlowRelationship.QNAME_PROP_POSITION ), GeometryFactory.createGM_Point( lFlowPositionFromElement, crs ) );
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

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
      reinit();
    else if( e.getKeyCode() == KeyEvent.VK_V )
      validateModel();

  }

  private void validateModel( )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final IKalypsoFeatureTheme discModelTheme = UtilMap.findEditableTheme( getMapPanel(), IFE1D2DElement.QNAME );

    final ValidateDiscretisationOperation operation = new ValidateDiscretisationOperation( m_discModel );

    display.syncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        final Shell shell = display.getActiveShell();

        final IStatus status = ProgressUtilities.busyCursorWhile( operation );

        final String dialogTitle = Messages.getString( "EditFEConceptGeometryWidget.0" ); //$NON-NLS-1$

        StatusDialog.open( shell, status, dialogTitle );
        if( status.isOK() )
          return;

        if( !operation.hasValidationFixes() )
          return;

        if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( dialogTitle, Messages.getString( "EditFEConceptGeometryWidget.1" ) ) ) //$NON-NLS-1$
          return;

        final ICommand command = operation.getValidationFix();
        discModelTheme.postCommand( command, null );
      }
    } );
  }

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
    }

    /* paint the snap */
    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );

    /* paint the preview */
    if( m_editor != null )
      m_editor.paint( g, projection, m_currentMapPoint );

    final Rectangle bounds = mapPanel.getScreenBounds();

    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 80 ), g, bounds );
  }

  private GM_Point snapToNode( final IMapPanel mapPanel, final Point p, final boolean snappingActive )
  {
    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );

    if( m_pointSnapper == null || m_editor == null )
      return null;

    m_pointSnapper.activate( snappingActive );

    final IFE1D2DNode snapNode = m_pointSnapper.moved( currentPoint );
    final GM_Point result = snapNode == null ? currentPoint : snapNode.getPoint();
    final Object snapObject = snapNode == null ? currentPoint : snapNode;

    // TODO: wrong place to do this!
    if( m_editor.getStartNode() != null )
    {
      final IStatus status = m_editor.checkNewNode( snapObject );

      if( status.isOK() )
        m_warningRenderer.setTooltip( null );
      else
        m_warningRenderer.setTooltip( status.getMessage() );
    }

    return result;
  }
}