/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.font.LineMetrics;
import java.awt.geom.Rectangle2D;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.wizard.WizardComposite;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.MapKeyNavigator;
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
public class ApplyElevationWidget extends FENetConceptSelectionWidget// AbstractWidget
    implements IWidgetWithOptions/* , IEvaluationContextConsumer */
{

  private Composite rootComposite;

  private WizardComposite wizardComposite;

  ApplyElevationWidgetDataModel dataModel = new ApplyElevationWidgetDataModel();

  private final ApplyElevationWidgetFace widgetFace = new ApplyElevationWidgetFace( dataModel );

  private Point point;

  public ApplyElevationWidget( )
  {
    this( "Höhen zuweisen", "Höhenzuweisung starten" );
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

    final IMapModell mapModell = mapPanel.getMapModell();
    final IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme( mapModell );
    dataModel.setDiscretisationModel( model1d2d );
    dataModel.setMapModell( mapModell );
    dataModel.setMapPanel( mapPanel );

    // find and set Elevation model system
    final IKalypsoFeatureTheme terrainElevationTheme = UtilMap.findEditableTheme( mapModell, KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL );
    final Feature eleSystemFeature = terrainElevationTheme.getFeatureList().getParentFeature();
    final ITerrainElevationModelSystem system = (ITerrainElevationModelSystem) eleSystemFeature.getAdapter( ITerrainElevationModelSystem.class );

    final IKalypsoFeatureTheme nodeTheme = UtilMap.findEditableTheme( mapModell, KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL );
    dataModel.setElevationModelSystem( system );
    dataModel.setData( ApplyElevationWidgetDataModel.NODE_THEME, nodeTheme );

    dataModel.setElevationTheme( terrainElevationTheme );
    dataModel.setMapPanel( mapPanel );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      return widgetFace.createControl( parent );
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
    if( widgetFace != null )
    {
      widgetFace.disposeControl();
    }
    dataModel.removeAllListeners();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ITerrainModelConsumer#setTerrainModel(org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel)
   */
  public void setTerrainModel( final ITerrainElevationModel terrainModel )
  {
    dataModel.setElevationModel( terrainModel );

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
    dataModel.setIgnoreMapSelection( false );
    super.moved( p );
    this.point = p;
    // TODO: check if this repaint is necessary for the widget
    final MapPanel mapPanel = dataModel.getMapPanel();
    if( mapPanel != null )
      mapPanel.repaint();
  }

  public static void drawToolTip( final String tooltip, final Point p, final Graphics g )
  {
    final Rectangle2D rectangle = g.getFontMetrics().getStringBounds( tooltip, g );

    g.setColor( new Color( 255, 255, 225 ) );
    g.fillRect( (int) p.getX(), (int) p.getY() + 20, (int) rectangle.getWidth() + 10, (int) rectangle.getHeight() + 5 );

    g.setColor( Color.BLUE );
    g.drawRect( (int) p.getX(), (int) p.getY() + 20, (int) rectangle.getWidth() + 10, (int) rectangle.getHeight() + 5 );

    /* Tooltip zeichnen. */
    g.drawString( tooltip, (int) p.getX() + 5, (int) p.getY() + 35 );
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
      final MapPanel mapPanel = dataModel.getMapPanel();
      if( mapPanel == null )
      {
        // System.out.println("map panel is null");
        return;
      }

      // finde node
      final GM_Point point = MapUtilities.transform( mapPanel, p );
      final double DELTA = MapUtilities.calculateWorldDistance( mapPanel, point, 10 );
      final IFE1D2DNode node = dataModel.getDiscretisationModel().findNode( point, DELTA );
      double height = 0;
      GM_Point nodePoint = null;

      if( node != null )
      {
        nodePoint = node.getPoint();

        final StringBuffer nodeElevationText = new StringBuffer( 64 );
        if( nodePoint.getCoordinateDimension() <= 2 )
        {
          nodeElevationText.append( "Keine Höhendaten an FE-Knoten" );
        }
        else
        {
          nodeElevationText.append( "Knotenhöhe = " );
          nodeElevationText.append( String.format( "%.3f", nodePoint.getZ() ) );
          nodeElevationText.append( " m" );
        }

        // show info
        // g.drawString( nodeElevationText.toString(), (int)p.getX(), (int)p.getY() );
        drawToolTip( nodeElevationText.toString(), p, g );
        final FontMetrics fontMetrics = g.getFontMetrics();
        final LineMetrics lineMetrics = fontMetrics.getLineMetrics( nodeElevationText.toString(), g );
        height = lineMetrics.getHeight() * 1.35;
      }

      if( nodePoint == null )
      {
        nodePoint = MapUtilities.transform( mapPanel, p );
      }
      final StringBuffer modelEleText = new StringBuffer( 64 );
      final IElevationProvider elevationProvider = getElevationProvider();
      if( elevationProvider != null )
      {
        final double elevation = elevationProvider.getElevation( nodePoint );
        if( !Double.isNaN( elevation ) )
        {
          modelEleText.append( "DGM-Höhe = " );
          modelEleText.append( String.format( "%.3f", elevation ) );
          modelEleText.append( " m" );
        }
        else
          modelEleText.append( "Keine Geländehöhen vorhanden" );
      }
      else
      {
        modelEleText.append( "Geländemodell nicht gefunden" );
      }

      // g.drawString( modelEleText.toString(), (int)p.getX(), (int)(p.getY()+height) );
      final Point p2 = new Point();
      p2.setLocation( (int) p.getX(), (int) (p.getY() + height) );
      drawToolTip( modelEleText.toString(), p2, g );
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
    dataModel.setIgnoreMapSelection( false );
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
    final MapPanel mapPanel = dataModel.getMapPanel();
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
    dataModel.setIgnoreMapSelection( false );
    super.doubleClickedRight( p );
    assignElevationToSelectedNodes( p );
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    super.keyPressed( e );
    final int code = e.getKeyCode();
    final MapPanel mapPanel = dataModel.getMapPanel();

    // TODO:
    // zoom in "-"

    if( e.isActionKey() )
    {
      // System.out.println( "e:" + e );
    }

    MapKeyNavigator.navigateOnKeyEvent( mapPanel, e, true );
    // /* zoom in */
    // if( code == KeyEvent.VK_PLUS )
    // {
    // // if( e.isShiftDown() )
    // // {
    // final GM_Envelope currentBBox = mapPanel.getBoundingBox();
    //
    // GM_Envelope wishBBox = null;
    //
    // final GM_Position currentMax = currentBBox.getMax();
    // final GM_Position currentMin = currentBBox.getMin();
    //
    // final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 10;
    // final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 10;
    // final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 10;
    // final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 10;
    //
    // final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
    // final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );
    //
    // wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );
    //
    // mapPanel.setBoundingBox( wishBBox );
    // // }
    // // else
    // // {
    // // mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );
    // // }
    //
    // }
    // /* zoom out */
    // else if( code == KeyEvent.VK_MINUS )
    // {
    // final GM_Envelope currentBBox = mapPanel.getBoundingBox();
    //
    // GM_Envelope wishBBox = null;
    //
    // final GM_Position currentMax = currentBBox.getMax();
    // final GM_Position currentMin = currentBBox.getMin();
    //
    // final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 10;
    // final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 10;
    // final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 10;
    // final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 10;
    //
    // final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
    // final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );
    //
    // wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );
    //
    // mapPanel.setBoundingBox( wishBBox );
    // }
    //
    // // pan "arrows
    // else if( code == KeyEvent.VK_RIGHT )
    // {
    // final GM_Envelope currentBBox = mapPanel.getBoundingBox();
    //
    // GM_Envelope wishBBox = null;
    //
    // final GM_Position currentMax = currentBBox.getMax();
    // final GM_Position currentMin = currentBBox.getMin();
    //
    // final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 20;
    // final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 20;
    //      
    //
    // final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
    // final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );
    //
    // wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );
    //
    // mapPanel.setBoundingBox( wishBBox );
    // }
    // else if( code == KeyEvent.VK_LEFT )
    // {
    // final GM_Envelope currentBBox = mapPanel.getBoundingBox();
    //
    // GM_Envelope wishBBox = null;
    //
    // final GM_Position currentMax = currentBBox.getMax();
    // final GM_Position currentMin = currentBBox.getMin();
    //
    // final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 20;
    // final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 20;
    //      
    //
    // final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
    // final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );
    //
    // wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );
    //
    // mapPanel.setBoundingBox( wishBBox );
    // }
    // else if( code == KeyEvent.VK_UP )
    // {
    // final GM_Envelope currentBBox = mapPanel.getBoundingBox();
    //
    // GM_Envelope wishBBox = null;
    //
    // final GM_Position currentMax = currentBBox.getMax();
    // final GM_Position currentMin = currentBBox.getMin();
    //
    // final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 20;
    // final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 20;
    //      
    //
    // final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
    // final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );
    //
    // wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );
    //
    // mapPanel.setBoundingBox( wishBBox );
    // }
    // else if( code == KeyEvent.VK_DOWN )
    // {
    // final GM_Envelope currentBBox = mapPanel.getBoundingBox();
    //
    // GM_Envelope wishBBox = null;
    //
    // final GM_Position currentMax = currentBBox.getMax();
    // final GM_Position currentMin = currentBBox.getMin();
    //
    // final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 20;
    // final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 20;
    //      
    //
    // final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
    // final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );
    //
    // wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );
    //
    // mapPanel.setBoundingBox( wishBBox );
    // }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    dataModel.setIgnoreMapSelection( false );
    super.leftClicked( p );
  }

  private void assignElevationToSelectedNodes( final Point p )
  {
    final MapPanel mapPanel = dataModel.getMapPanel();
    if( mapPanel == null )
    {
      // System.out.println("map panel is null");
      return;
    }

    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final IKalypsoFeatureTheme theme = getTheme( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final IFEDiscretisationModel1d2d model1d2d = dataModel.getDiscretisationModel();
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
      final IFE1D2DNode node = dataModel.getDiscretisationModel().findNode( point, DELTA );
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
    IElevationProvider elevationProvider = dataModel.getElevationModel();
    if( elevationProvider == null )
    {
      // System.out.println("Could not found selected elevation model; trying to use elevation model system");
      elevationProvider = dataModel.getElevationModelSystem();
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
    drawElevationData( g, point );
    drawHandle( g, point, Color.BLUE, 6 );
  }

  public static final void drawHandle( final Graphics g, final Point currentPoint, final Color handleColor, final int squareWidth )
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
