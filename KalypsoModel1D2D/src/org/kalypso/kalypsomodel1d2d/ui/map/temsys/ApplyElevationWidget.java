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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
public class ApplyElevationWidget extends AbstractDelegateWidget implements IWidgetWithOptions
{
  private final ApplyElevationWidgetDataModel m_dataModel = new ApplyElevationWidgetDataModel();

  private final ApplyElevationWidgetFace m_widgetFace = new ApplyElevationWidgetFace( m_dataModel );

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_toolTipRendererDesc = new ToolTipRenderer();

  private Point m_point;

  private final SelectFeatureWidget m_selDelegateWidget;

  public ApplyElevationWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.1" ), new SelectFeatureWidget( "", "", new QName[] { IFE1D2DNode.FEATURE_1D2DNODE }, IFE1D2DNode.PROPERTY_POINT ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    m_toolTipRendererDesc.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.14" ) ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );

    m_selDelegateWidget = (SelectFeatureWidget) getDelegate();
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    /* set data to data model */
    if( mapPanel == null )
      return;

    m_dataModel.setMapPanel( mapPanel );

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return;

    m_dataModel.setMapModell( mapModell );

    final IKalypsoTheme[] themes = mapModell.getAllThemes();
    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final IFeatureType ftp = ft.getFeatureType();
        final QName qName = ftp == null ? null : ftp.getQName();
        if( IFE1D2DNode.FEATURE_1D2DNODE.equals( qName ) )
        {
          final IKalypsoFeatureTheme[] fts = new IKalypsoFeatureTheme[1];
          fts[0] = ft;
          m_selDelegateWidget.setThemes( fts );
        }
      }
    }

    // find and set Elevation model system
    final IKalypsoFeatureTheme terrainElevationTheme = UtilMap.findEditableTheme( mapPanel, NativeTerrainElevationModelWrapper.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL );
    if( terrainElevationTheme != null )
    {
      final Feature eleSystemFeature = terrainElevationTheme.getFeatureList().getOwner();
      final ITerrainElevationModelSystem system = (ITerrainElevationModelSystem) eleSystemFeature.getAdapter( ITerrainElevationModelSystem.class );

      m_dataModel.setElevationModelSystem( system );
      m_dataModel.setElevationTheme( terrainElevationTheme );
    }
    final IKalypsoFeatureTheme elevationTheme = UtilMap.findEditableTheme( mapPanel, NativeTerrainElevationModelWrapper.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL );
    if( elevationTheme != null )
      m_dataModel.setData( ApplyElevationWidgetDataModel.NODE_THEME, elevationTheme );
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    return m_widgetFace.createControl( parent );
  }

  @Override
  public void disposeControl( )
  {
    if( m_widgetFace != null )
      m_widgetFace.disposeControl();

    m_dataModel.removeAllListeners();
  }

  @Override
  public void moved( final Point p )
  {
    super.moved( p );

    m_point = p;

    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel != null )
      mapPanel.repaintMap();
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_ENTER )
    {
      e.consume();

      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
      final EasyFeatureWrapper[] allFeatures = selectionManager.getAllFeatures();
      final Feature[] features = FeatureSelectionHelper.getFeatures( selectionManager );

      if( features.length == 0 )
        return;

      final List<IFE1D2DNode> nodeList = new ArrayList<>();
      for( final Feature feature : features )
      {
        if( feature != null )
        {
          final IFE1D2DNode node = (IFE1D2DNode) feature.getAdapter( IFE1D2DNode.class );
          if( node != null )
            nodeList.add( node );
        }
      }
      try
      {
        ApplyElevationHelper.assignElevationToSelectedNodes( m_dataModel, nodeList );
      }
      catch( final Exception e1 )
      {
        e1.printStackTrace();
        super.keyPressed( e );
      }
      selectionManager.setSelection( allFeatures );
    }

    super.keyPressed( e );
  }

  private final void paintElevationDataTooltip( final Graphics g, final Point p )
  {
    final Color color = g.getColor();
    g.setColor( Color.BLACK );
    try
    {
      if( p == null )
        return;

      final IMapPanel mapPanel = m_dataModel.getMapPanel();
      if( mapPanel == null || mapPanel.getProjection() == null )
        return;

      // find node
      final GM_Point point = MapUtilities.transform( mapPanel, p );
      if( point == null )
        return;

      final double DELTA = MapUtilities.calculateWorldDistance( mapPanel, point, 10 );
      final IFE1D2DNode node = m_dataModel.getDiscretisationModel().findNode( point, DELTA );
      GM_Point nodePoint = null;

      final StringBuffer tooltipText = new StringBuffer();
      if( node != null )
      {
        nodePoint = node.getPoint();
        if( nodePoint.getCoordinateDimension() <= 2 )
        {
          tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.2" ) ); //$NON-NLS-1$
        }
        else
        {
          tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.3" ) ); //$NON-NLS-1$
          tooltipText.append( String.format( "%.3f m", nodePoint.getZ() ) ); //$NON-NLS-1$
        }
        tooltipText.append( "\n" ); //$NON-NLS-1$
      }

      if( nodePoint == null )
        nodePoint = MapUtilities.transform( mapPanel, p );

      final IElevationModel elevationProvider = m_dataModel.getElevationProvider();
      if( elevationProvider != null )
      {
        final double elevation = elevationProvider.getElevation( nodePoint );
        if( !Double.isNaN( elevation ) )
        {
          tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.6" ) ); //$NON-NLS-1$
          tooltipText.append( String.format( "%.3f m", elevation ) ); //$NON-NLS-1$
        }
        else
          tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.9" ) ); //$NON-NLS-1$
      }
      else
        tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.10" ) ); //$NON-NLS-1$

      m_toolTipRenderer.setTooltip( tooltipText.toString() );
      m_toolTipRenderer.paintToolTip( p, g, getMapPanel().getScreenBounds() );

      return;
    }
    catch( final RuntimeException e )
    {
      e.printStackTrace();
    }
    catch( final ElevationException e )
    {
      e.printStackTrace();
    }
    finally
    {
      g.setColor( color );
    }
  }

  @Override
  public void doubleClickedLeft( final Point p )
  {
    super.doubleClickedLeft( p );
  }

  @Override
  public void paint( final Graphics g )
  {
    final Graphics2D g2 = (Graphics2D) g;
    super.paint( g2 );

    final List<IFE1D2DNode> selectedNodeList = m_dataModel.getSelectedNodeList();
    if( selectedNodeList != null )
      paintSelecetedNodes( g2, selectedNodeList );

    paintElevationDataTooltip( g2, m_point );

    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel == null )
      return;

    final Rectangle bounds = mapPanel.getScreenBounds();
    final String delegateTooltip = getDelegate().getToolTip();

    m_toolTipRendererDesc.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget.16" ) + delegateTooltip ); //$NON-NLS-1$
    m_toolTipRendererDesc.paintToolTip( new Point( 5, bounds.height - 5 ), g2, bounds );
  }

  private void paintSelecetedNodes( final Graphics2D g2, final List<IFE1D2DNode> selectedNodeList )
  {
    final IMapPanel panel = m_dataModel.getMapPanel();

    if( panel == null )
      return;

    final Color oldColor = g2.getColor();
    final Color color = new Color( 20, 20, 255 );
    g2.setColor( color );

    for( final IFE1D2DNode node : selectedNodeList )
    {
      if( node == null )
        return;
      final GM_Point point = node.getPoint();
      if( point == null )
        return;

      final int smallRect = 8;
      final Point nodePoint = MapUtilities.retransform( panel, point );

      g2.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
    }

    g2.setColor( oldColor );
  }

  @Override
  public synchronized boolean canBeActivated( final ISelection selection, final IMapPanel mapPanel )
  {
    return true;
  }

  @Override
  public String getPartName( )
  {
    return Messages.getString("ApplyElevationWidget.0"); //$NON-NLS-1$
  }
}