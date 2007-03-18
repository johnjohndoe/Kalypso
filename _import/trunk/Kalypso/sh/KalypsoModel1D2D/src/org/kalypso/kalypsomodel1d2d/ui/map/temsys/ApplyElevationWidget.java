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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.wizard.WizardComposite;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.IEvaluationContextConsumer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * 
 * @author Patrice Congo
 * @author Madanagopal
 *
 */
public class ApplyElevationWidget 
                    extends FENetConceptSelectionWidget//AbstractWidget 
                    implements IWidgetWithOptions, IEvaluationContextConsumer
{
  
  

  private Composite rootComposite;
  
  private WizardComposite wizardComposite;
  
  
  ApplyElevationWidgetDataModel dataModel= new ApplyElevationWidgetDataModel();
  private ApplyElevationWidgetFace widgetFace = 
    new ApplyElevationWidgetFace(dataModel);

  private Point point;
  
  public ApplyElevationWidget()
  {
    this("Höhen zuweise","Höhenzuweisung start");
  }
  
  public ApplyElevationWidget( String name, String toolTip )
  {
//    super( name, toolTip );
    super( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, name, toolTip );    
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate(commandPoster, mapPanel);
    
    IMapModell mapModell = mapPanel.getMapModell();
    IFEDiscretisationModel1d2d model1d2d = 
      UtilMap.findFEModelTheme( 
          mapModell, 
          Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT);
    dataModel.setDiscretisationModel( model1d2d );
    dataModel.setMapModell( mapModell );
    dataModel.setMapPanel( mapPanel );
    
    //find and set Elevation model system
    IKalypsoFeatureTheme terrainElevationTheme = UtilMap.findEditableTheme( 
          mapModell, 
          KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL
          );
    Feature eleSystemFeature = 
      terrainElevationTheme.getFeatureList().getParentFeature();
    ITerrainElevationModelSystem system = 
      (ITerrainElevationModelSystem) eleSystemFeature.getAdapter( 
                                      ITerrainElevationModelSystem.class );
    
    IKalypsoFeatureTheme nodeTheme = UtilMap.findEditableTheme( 
        mapModell, 
        KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL
        );
    dataModel.setElevationModelSystem( system );
    dataModel.setData( 
          ApplyElevationWidgetDataModel.NODE_THEME, 
          nodeTheme );
    
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
    catch (Throwable th) 
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
    if(widgetFace!=null)
    {
      widgetFace.disposeControl();
    }
    dataModel.removeAllListeners();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ITerrainModelConsumer#setTerrainModel(org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel)
   */
  public void setTerrainModel( ITerrainElevationModel terrainModel )
  {
    dataModel.setElevationModel( terrainModel );
    
  }
   /**
   * @see org.kalypso.ogc.gml.map.widgets.IEvaluationContextConsumer#setEvaluationContext(org.eclipse.core.expressions.IEvaluationContext)
   */
  public void setEvaluationContext( IEvaluationContext evaluationContext )
  {
    Assert.throwIAEOnNullParam( evaluationContext, "evaluationContext" );
//    final ISzenarioDataProvider szenarioDataProvider = 
//      (ISzenarioDataProvider) context.getVariable( 
//            SzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
//    final ITerrainModel terrainModel = (ITerrainModel) szenarioDataProvider.getModel( ITerrainModel.class );    
    try
    {
      ITerrainModel terrainModel= refelectTerrainModel( evaluationContext );
      dataModel.setTerrainModel( terrainModel );
    }
    catch( Throwable e )
    {
      e.printStackTrace();
    }
   
  }
  
  private final ITerrainModel refelectTerrainModel(IEvaluationContext evaluationContext) throws SecurityException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException, InvocationTargetException
  {
    Object dataProvider=evaluationContext.getVariable( "activeSzenarioDataProvider" );
    
    if(dataProvider==null)
    {
      throw new IllegalArgumentException(
          "evaluation context does not contain an active szenarion data provider");
    }
    Method getModelMethod=
      dataProvider.getClass().getMethod( "getModel", new Class[]{Class.class});
    if(getModelMethod==null)
    {
      throw new IllegalArgumentException(
          "Data provider in "+evaluationContext+" does not have getModel method"); 
    }
    try
    {
      ITerrainModel terrainModel=
        (ITerrainModel) getModelMethod.invoke( 
            dataProvider, new Object[]{ITerrainModel.class});
      return terrainModel;
    }
    catch (Exception e) 
    {
      e.printStackTrace();
      return null;
    }
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
    dataModel.setIgnoreMapSelection( false );
    super.moved(p);
    this.point=p;
    //TODO: check if this repaint is necessary for the widget
    MapPanel mapPanel = dataModel.getMapPanel();
    if ( mapPanel != null)
      mapPanel.repaint();
  }
  
  private void drawToolTip( String tooltip, Point p, Graphics g )
  {
    Rectangle2D rectangle = g.getFontMetrics().getStringBounds( tooltip, g );

    g.setColor( new Color( 255, 255, 225 ) );
    g.fillRect( (int) p.getX(), (int) p.getY() + 20, (int) rectangle.getWidth() + 10, (int) rectangle.getHeight() + 5 );

    g.setColor( Color.BLUE);
    g.drawRect( (int) p.getX(), (int) p.getY() + 20, (int) rectangle.getWidth() + 10, (int) rectangle.getHeight() + 5 );

    /* Tooltip zeichnen. */
    g.drawString( tooltip, (int) p.getX() + 5, (int) p.getY() + 35 );

    
  }

  private String getElevationMessage( GM_Point nodePoint )
  {
    StringBuffer nodeElevationText= new StringBuffer(64);
    if(nodePoint.getCoordinateDimension()<=2)
    {
      nodeElevationText.append( "keine Netzhöhe" );
    }
    else
    {
      nodeElevationText.append( "Netzhöhe = " );
      nodeElevationText.append( String.format( "%.3f",nodePoint.getZ()) );
      nodeElevationText.append( " m");
    }
    
    final String elevationMessage = nodeElevationText.toString();
    return elevationMessage;
}
  
  
  private final void drawElevationData(Graphics g, Point p)
  {
    final Color color = g.getColor();
    g.setColor( Color.BLACK );
    try
    {
      if(p==null)
      {
        return;
      }
      MapPanel mapPanel = dataModel.getMapPanel();
      if(mapPanel==null)
      {
        System.out.println("map panel is null");
        return;
      }
      
      //finde node
      GM_Point point = 
            MapUtilities.transform( 
                  mapPanel, p );
      final double DELTA = 
        MapUtilities.calculateWorldDistance( 
                                      mapPanel, 
                                      point, 
                                      10 );
      IFE1D2DNode node = dataModel.getDiscretisationModel().findNode( 
                                point, 
                                DELTA );
      double height=0;
      GM_Point nodePoint=null;
      
      if(node!=null)
      {
        nodePoint = node.getPoint();
        
        
        StringBuffer nodeElevationText= new StringBuffer(64);
        if(nodePoint.getCoordinateDimension()<=2)
        {
          nodeElevationText.append( "Keine Höhendaten an FE-Knoten" );
        }
        else
        {
          nodeElevationText.append( "Knotenhöhe = " );
          nodeElevationText.append( String.format( "%.3f",nodePoint.getZ() ));
          nodeElevationText.append( " m" );
        }
        
        //show info
        //g.drawString( nodeElevationText.toString(), (int)p.getX(), (int)p.getY() );
        drawToolTip( nodeElevationText.toString() , p, g);
        FontMetrics fontMetrics = g.getFontMetrics();
        LineMetrics lineMetrics = fontMetrics.getLineMetrics( nodeElevationText.toString(), g );
        height= lineMetrics.getHeight()*1.35;
      }
      
      if(nodePoint==null)
      {
        nodePoint = MapUtilities.transform( mapPanel, p );  
      }
      StringBuffer modelEleText = new StringBuffer(64);
      IElevationProvider elevationProvider=getElevationProvider();
      if(elevationProvider!=null)
      {
        double elevation = elevationProvider.getElevation( nodePoint );
        modelEleText.append( "DGM-Höhe = " );
        modelEleText.append(String.format( "%.3f",elevation));
        modelEleText.append(" m");
      }
      else
      {
        modelEleText.append("Geländemodell nicht gefunden");
      }
        
      //g.drawString( modelEleText.toString(), (int)p.getX(), (int)(p.getY()+height) );
      Point p2 = new Point();
      p2.setLocation((int)p.getX(), (int)(p.getY()+height) );
      drawToolTip( modelEleText.toString() , p2, g);
      return;
    }
    catch( RuntimeException e )
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
  public void doubleClickedLeft( Point p )
  {
    dataModel.setIgnoreMapSelection( false );
    super.doubleClickedLeft(p);
    assignElevationToSelectedNodes(p);
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#doubleClickedRight(java.awt.Point)
   */
  @Override
  public void doubleClickedRight( Point p )
  {
    dataModel.setIgnoreMapSelection( false );
    super.doubleClickedRight(p);
    assignElevationToSelectedNodes( p );
  }
  
  @Override
  public void keyPressed( KeyEvent e )
  {
    int code = e.getKeyCode();
    MapPanel mapPanel = dataModel.getMapPanel();

    // TODO:
    // zoom in "-"

    if( e.isActionKey() )
    {
      System.out.println( "e:" + e );
    }
    /* zoom in */
    if( code == KeyEvent.VK_PLUS )
    {
//      if( e.isShiftDown() )
//      {
        final GM_Envelope currentBBox = mapPanel.getBoundingBox();

        GM_Envelope wishBBox = null;

        final GM_Position currentMax = currentBBox.getMax();
        final GM_Position currentMin = currentBBox.getMin();

        final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 10;
        final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 10;
        final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 10;
        final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 10;

        final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
        final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

        wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

        mapPanel.setBoundingBox( wishBBox );
//      }
//      else
//      {
//        mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );
//      }

    }
    /* zoom out */
    else if( code == KeyEvent.VK_MINUS )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 10;
      final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 10;
      final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 10;
      final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 10;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }

    // pan "arrows
    else if( code == KeyEvent.VK_RIGHT )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 20;
      final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 20;
      

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_LEFT )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 20;
      final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 20;
      

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_UP )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 20;
      final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 20;
      

      final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_DOWN )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 20;
      final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 20;
      

      final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }

  }

  
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( Point p )
  {
    dataModel.setIgnoreMapSelection( false );
    super.leftClicked(p);
  }
  
  private void assignElevationToSelectedNodes( Point p )
  {
    MapPanel mapPanel = dataModel.getMapPanel();
    if(mapPanel==null)
    {
      System.out.println("map panel is null");
      return;
    }
    
    IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    IKalypsoFeatureTheme theme = getTheme( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    IFEDiscretisationModel1d2d model1d2d = dataModel.getDiscretisationModel();
    if(theme==null)
    {
      System.out.println("Could not get node theme");
      return;
    }
    CommandableWorkspace workspace = theme.getWorkspace();
    ChangeDiscretiationModelCommand modelCommand = 
      new ChangeDiscretiationModelCommand(workspace,model1d2d);
    ChangeNodePositionCommand changeNodePosCmd = null;

    IElevationProvider elevationProvider=getElevationProvider();
    if(elevationProvider==null)
    {
      System.out.println("could not find elevation provider 1");
      return;
    }
    
    if(!selectionManager.isEmpty())
    {
      double elevation;
      for(EasyFeatureWrapper easyFeatureWrapper:selectionManager.getAllFeatures())
      {
        if(easyFeatureWrapper!=null)
        {
          Feature feature = easyFeatureWrapper.getFeature();
          if(feature!=null)
          {
            IFE1D2DNode node = (IFE1D2DNode)feature.getAdapter( IFE1D2DNode.class );
            if(node!=null)
            {
              elevation = elevationProvider.getElevation( node.getPoint() );
              changeNodePosCmd = new ChangeNodePositionCommand(model1d2d,node,elevation);
              modelCommand.addCommand( changeNodePosCmd );
            }
          }
        }
      }
      
      try
      {
        workspace.postCommand( modelCommand );
      }
      catch( Throwable th )
      {
        th.printStackTrace();
      }
      
    }
    else
    {
    
      if(p==null)
      {
        return;
      }
      
      
      //finde node
      GM_Point point = 
            MapUtilities.transform( 
                  mapPanel, p );
      final double DELTA = 
        MapUtilities.calculateWorldDistance( 
                                      mapPanel, 
                                      point, 
                                      10 );
      IFE1D2DNode node = dataModel.getDiscretisationModel().findNode( 
                                point, 
                                DELTA );
      if(node!=null)
      {
        //
        double elevation = elevationProvider.getElevation( node.getPoint() );
        changeNodePosCmd = new ChangeNodePositionCommand(model1d2d,node,elevation);
        modelCommand.addCommand( changeNodePosCmd );
        try
        {
          workspace.postCommand( modelCommand );
        }
        catch( Throwable th )
        {
          th.printStackTrace();
        }
        
      }
      return;
    }
  }
  
  private final IElevationProvider getElevationProvider()
  {
    IElevationProvider elevationProvider = dataModel.getElevationModel();
    if(elevationProvider==null)
    {
      System.out.println("Could not found selected elevation model; trying to use elevation model system");
      elevationProvider = dataModel.getElevationModelSystem();
      if(elevationProvider==null)
      {
        System.out.println("Could not find elevation model system");
      }
    }
    return elevationProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    super.paint(g);
    drawElevationData( g, point );
    drawHandle( g, point, Color.BLUE, 6 );
  }
  
  public static final void drawHandle(
                          Graphics g, 
                          Point currentPoint, 
                          Color handleColor, 
                          int squareWidth)
  {
    if(currentPoint==null)
    {
      return;
    }
    
    int pointRectSizeHalf=squareWidth/2;
    g.drawRect( 
        (int) currentPoint.getX() - pointRectSizeHalf, 
        (int) currentPoint.getY() - pointRectSizeHalf, 
        squareWidth, 
        squareWidth );
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#canBeActivated(org.eclipse.jface.viewers.ISelection, org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    return true;//super.canBeActivated(selection, mapPanel);
  }
}
