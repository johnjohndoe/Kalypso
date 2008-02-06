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
import java.awt.Point;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * 
 * @author Patrice Congo
 * @author Madanagopal
 * 
 */
public class ApplyElevationWidget extends FENetConceptSelectionWidget implements IWidgetWithOptions
{
  private final ApplyElevationWidgetDataModel m_dataModel = new ApplyElevationWidgetDataModel();

  private final ApplyElevationWidgetFace m_widgetFace = new ApplyElevationWidgetFace( m_dataModel );

  private final ToolTipRenderer m_tooltipRenderer = new ToolTipRenderer();

  private Point m_point;

  public ApplyElevationWidget( )
  {
    this( Messages.getString( "ApplyElevationWidget.0" ), Messages.getString( "ApplyElevationWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public ApplyElevationWidget( final String name, final String toolTip )
  {
    // super( name, toolTip );
    super( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, name, toolTip );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    m_dataModel.setMapPanel( mapPanel );

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      m_dataModel.setMapModell( mapModell );
    }

    // find and set Elevation model system
    final IKalypsoFeatureTheme terrainElevationTheme = UtilMap.findEditableTheme( mapModell, KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL );
    if( terrainElevationTheme != null )
    {
      final Feature eleSystemFeature = terrainElevationTheme.getFeatureList().getParentFeature();
      final ITerrainElevationModelSystem system = (ITerrainElevationModelSystem) eleSystemFeature.getAdapter( ITerrainElevationModelSystem.class );

      m_dataModel.setElevationModelSystem( system );
      m_dataModel.setElevationTheme( terrainElevationTheme );
    }
    final IKalypsoFeatureTheme nodeTheme = UtilMap.findEditableTheme( mapModell, KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL );
    if( nodeTheme != null )
    {
      m_dataModel.setData( ApplyElevationWidgetDataModel.NODE_THEME, nodeTheme );
    }
    m_dataModel.setMapPanel( mapPanel );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      return m_widgetFace.createControl( parent );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( m_widgetFace != null )
    {
      m_widgetFace.disposeControl();
    }
    m_dataModel.removeAllListeners();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ITerrainModelConsumer#setTerrainModel(org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel)
   */
  public void setTerrainModel( final ITerrainElevationModel terrainModel )
  {
    m_dataModel.setElevationModel( terrainModel );

  }

  // /**
  // * @see
  // org.kalypso.ogc.gml.map.widgets.IEvaluationContextConsumer#setEvaluationContext(org.eclipse.core.expressions.IEvaluationContext)
  // */
  // public void setEvaluationContext( IEvaluationContext evaluationContext )
  // {
  // // Assert.throwIAEOnNullParam( evaluationContext, "evaluationContext" );
  // //// final ISzenarioDataProvider szenarioDataProvider =
  // //// (ISzenarioDataProvider) context.getVariable(
  // //// SzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
  // //// final ITerrainModel terrainModel = (ITerrainModel) szenarioDataProvider.getModel( ITerrainModel.class );
  // // try
  // // {
  // // ITerrainModel terrainModel= refelectTerrainModel( evaluationContext );
  // // dataModel.setTerrainModel( terrainModel );
  // // }
  // // catch( Throwable e )
  // // {
  // // e.printStackTrace();
  // // }
  //
  // }

  // private final ITerrainModel refelectTerrainModel(IEvaluationContext evaluationContext) throws SecurityException,
  // NoSuchMethodException, IllegalArgumentException, IllegalAccessException, InvocationTargetException
  // {
  // Object dataProvider=evaluationContext.getVariable( "activeSzenarioDataProvider" );
  //
  // if(dataProvider==null)
  // {
  // throw new IllegalArgumentException(
  // "evaluation context does not contain an active szenarion data provider");
  // }
  // Method getModelMethod=
  // dataProvider.getClass().getMethod( "getModel", new Class[]{Class.class});
  // if(getModelMethod==null)
  // {
  // throw new IllegalArgumentException(
  // "Data provider in "+evaluationContext+" does not have getModel method");
  // }
  // try
  // {
  // ITerrainModel terrainModel=
  // (ITerrainModel) getModelMethod.invoke(
  // dataProvider, new Object[]{ITerrainModel.class});
  // return terrainModel;
  // }
  // catch (Exception e)
  // {
  // e.printStackTrace();
  // return null;
  // }
  // }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_dataModel.setIgnoreMapSelection( false );
    super.moved( p );
    this.m_point = p;

    final MapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel != null )
      mapPanel.repaint();
  }

  private final void drawElevationData( final Graphics g, final Point p )
  {
    final Color color = g.getColor();
    g.setColor( Color.BLACK );
    try
    {
      if( p == null )
      {
        return;
      }
      final MapPanel mapPanel = m_dataModel.getMapPanel();
      if( mapPanel == null )
      {
        // System.out.println("map panel is null");
        return;
      }

      // finde node
      final GM_Point point = MapUtilities.transform( mapPanel, p );
      final double DELTA = MapUtilities.calculateWorldDistance( mapPanel, point, 10 );
      final IFE1D2DNode node = m_dataModel.getDiscretisationModel().findNode( point, DELTA );
      GM_Point nodePoint = null;

      final StringBuffer tooltipText = new StringBuffer();
      if( node != null )
      {
        nodePoint = node.getPoint();
        if( nodePoint.getCoordinateDimension() <= 2 )
        {
          tooltipText.append( Messages.getString( "ApplyElevationWidget.2" ) ); //$NON-NLS-1$
        }
        else
        {
          tooltipText.append( Messages.getString( "ApplyElevationWidget.3" ) ); //$NON-NLS-1$
          tooltipText.append( String.format( "%.3f m", nodePoint.getZ() ) ); //$NON-NLS-1$
        }
        tooltipText.append( "\n" );
      }

      if( nodePoint == null )
        nodePoint = MapUtilities.transform( mapPanel, p );

      final IElevationProvider elevationProvider = getElevationProvider();
      if( elevationProvider != null )
      {
        final double elevation = elevationProvider.getElevation( nodePoint );
        if( !Double.isNaN( elevation ) )
        {
          tooltipText.append( Messages.getString( "ApplyElevationWidget.6" ) ); //$NON-NLS-1$
          tooltipText.append( String.format( "%.3f m", elevation ) ); //$NON-NLS-1$
        }
        else
          tooltipText.append( Messages.getString( "ApplyElevationWidget.9" ) ); //$NON-NLS-1$
      }
      else
        tooltipText.append( Messages.getString( "ApplyElevationWidget.10" ) ); //$NON-NLS-1$

      m_tooltipRenderer.setTooltip( tooltipText.toString() );
      m_tooltipRenderer.paintToolTip( p, g, getMapPanel().getBounds() );
      return;
    }
    catch( final RuntimeException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    finally
    {
      g.setColor( color );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    m_dataModel.setIgnoreMapSelection( false );
    super.doubleClickedLeft( p );
    if( !isPolygonSelectModus() )
    {
      assignElevationToSelectedNodes( p );
    }
    // used to propagate the selection again to the listener
    // necessary to get the map the node info refreshed
    reselectSelected();
  }

  private void reselectSelected( )
  {
    final MapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel == null )
    {
      // System.out.println("Cannot reselect Map panel is null");
      return;
    }
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final EasyFeatureWrapper[] allFeatures = selectionManager.getAllFeatures();
    if( allFeatures != null )
    {
      if( allFeatures.length != 0 )
      {
        selectionManager.setSelection( allFeatures );
      }
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#doubleClickedRight(java.awt.Point)
   */
  @Override
  public void doubleClickedRight( final Point p )
  {
    m_dataModel.setIgnoreMapSelection( false );
    super.doubleClickedRight( p );
    assignElevationToSelectedNodes( p );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    m_dataModel.setIgnoreMapSelection( false );
    super.leftClicked( p );
  }

  private void assignElevationToSelectedNodes( final Point p )
  {
    final MapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel == null )
    {
      // System.out.println("map panel is null");
      return;
    }

    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final IKalypsoFeatureTheme theme = UtilMap.findEditableTheme( m_dataModel.getMapModell(), Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
    if( theme == null )
    {
      // System.out.println("Could not get node theme");
      return;
    }
    final CommandableWorkspace workspace = theme.getWorkspace();
    final ChangeDiscretiationModelCommand modelCommand = new ChangeDiscretiationModelCommand( workspace, model1d2d );
    ChangeNodePositionCommand changeNodePosCmd = null;

    final IElevationProvider elevationProvider = getElevationProvider();
    if( elevationProvider == null )
    {
      // System.out.println("could not find elevation provider 1");
      return;
    }

    if( !selectionManager.isEmpty() )
    {
      double elevation;
      for( final EasyFeatureWrapper easyFeatureWrapper : selectionManager.getAllFeatures() )
      {
        if( easyFeatureWrapper != null )
        {
          final Feature feature = easyFeatureWrapper.getFeature();
          if( feature != null )
          {
            final IFE1D2DNode node = (IFE1D2DNode) feature.getAdapter( IFE1D2DNode.class );
            if( node != null )
            {
              elevation = elevationProvider.getElevation( node.getPoint() );
              changeNodePosCmd = new ChangeNodePositionCommand( model1d2d, node, elevation );
              modelCommand.addCommand( changeNodePosCmd );
            }
          }
        }
      }

      try
      {
        workspace.postCommand( modelCommand );
      }
      catch( final Throwable th )
      {
        th.printStackTrace();
      }

    }
    else
    {

      if( p == null )
      {
        return;
      }

      // finde node
      final GM_Point point = MapUtilities.transform( mapPanel, p );
      final double DELTA = MapUtilities.calculateWorldDistance( mapPanel, point, 10 );
      final IFE1D2DNode node = m_dataModel.getDiscretisationModel().findNode( point, DELTA );
      if( node != null )
      {
        //
        final double elevation = elevationProvider.getElevation( node.getPoint() );
        changeNodePosCmd = new ChangeNodePositionCommand( model1d2d, node, elevation );
        modelCommand.addCommand( changeNodePosCmd );
        try
        {
          workspace.postCommand( modelCommand );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
        }

      }
      return;
    }
  }

  private final IElevationProvider getElevationProvider( )
  {
    IElevationProvider elevationProvider = m_dataModel.getElevationModel();
    if( elevationProvider == null )
    {
      // System.out.println("Could not found selected elevation model; trying to use elevation model system");
      elevationProvider = m_dataModel.getElevationModelSystem();
      if( elevationProvider == null )
      {
        // System.out.println("Could not find elevation model system");
      }
    }
    return elevationProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );
    drawElevationData( g, m_point );
    drawHandle( g, m_point, 6 );
  }

  public static final void drawHandle( final Graphics g, final Point currentPoint, final int squareWidth )
  {
    if( currentPoint == null )
    {
      return;
    }

    final int pointRectSizeHalf = squareWidth / 2;
    g.drawRect( (int) currentPoint.getX() - pointRectSizeHalf, (int) currentPoint.getY() - pointRectSizeHalf, squareWidth, squareWidth );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#canBeActivated(org.eclipse.jface.viewers.ISelection,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public boolean canBeActivated( final ISelection selection, final MapPanel mapPanel )
  {
    return true;// super.canBeActivated(selection, mapPanel);
  }
}
