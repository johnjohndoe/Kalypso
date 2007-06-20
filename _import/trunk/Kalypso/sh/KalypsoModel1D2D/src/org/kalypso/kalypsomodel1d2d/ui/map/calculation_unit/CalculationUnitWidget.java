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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.apache.commons.httpclient.methods.GetMethod;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1d2dCalUnitTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.popup.PopupBlocker;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.views.map.MapView;

/**
 * 
 * @author Patrice Congo
 * @author Madanagopal
 *
 */
public class CalculationUnitWidget 
                    implements IWidgetWithOptions, 
                                IWidget, 
                                IWidgetWithStrategy,
                                IGrabDistanceProvider
{
  
  private IWidget strategy = null; 
    
  private CalculationUnitDataModel dataModel= new CalculationUnitDataModel();
  
  private CalculationUnitWidgetFace widgetFace = new CalculationUnitWidgetFace(dataModel);
  
  private String name;
  
  private String tooltip;
  
  private Model1d2dCalUnitTheme calUnitTheme;
  
  
  private KeyBasedDataModelChangeListener calThemeUpdater =
      new KeyBasedDataModelChangeListener()
  {

    public void dataChanged( String key, Object newValue )
    {
      if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
      {
        calUnitTheme.setCalulationUnit( (ICalculationUnit) newValue );
        KeyBasedDataModelUtil.repaintMapPanel( 
                    dataModel, ICommonKeys.KEY_MAP_PANEL );
      }
    }
    
  };
  
  /**
   * Used to prevent the swt popup to show
   */
  final PopupBlocker popupBlocker = new PopupBlocker();
  
  public CalculationUnitWidget()
  {
    this("Berechnungseinheiten Modellieren","Berechnungseinheiten Modellieren");
  }
  
  public CalculationUnitWidget( String name, String toolTip )
  {
    this.name = name;
    this.tooltip = toolTip;    
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
//  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    dataModel.setData( ICommonKeys.KEY_MAP_PANEL, mapPanel );
    IMapModell mapModell = mapPanel.getMapModell();
    IFEDiscretisationModel1d2d model1d2d =
        UtilMap.findFEModelTheme( mapModell );
    //TODO check model1d2d for null and do something
    dataModel.setData( 
        ICommonKeys.KEY_DISCRETISATION_MODEL, model1d2d );
    dataModel.setData(
        ICommonKeys.KEY_FEATURE_WRAPPER_LIST, 
        CalUnitOps.getModelCalculationUnits( model1d2d ) );
    dataModel.setData( ICommonKeys.WIDGET_WITH_STRATEGY, this );
    
    //command manager since it is use in the dirty pool object framework
    //the commandable workspace of the target theme is taken
    IKalypsoFeatureTheme targetTheme = UtilMap.findEditableTheme( 
        mapModell, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );
    dataModel.setData( 
        ICommonKeys.KEY_COMMAND_MANAGER, 
        targetTheme.getWorkspace());
    calUnitTheme = 
      new Model1d2dCalUnitTheme("Aktuelle CalUnit",mapModell);
    mapModell.addTheme( calUnitTheme );
    dataModel.addKeyBasedDataChangeListener( calThemeUpdater );
    
    IKalypsoFeatureTheme bcTheme = UtilMap.findEditableTheme( 
        mapModell, 
        IBoundaryCondition.QNAME );
    if( bcTheme == null )
    {
      throw new RuntimeException("Could not find boundary condition theme");
    }
    
//    dataModel.setData( 
//        ICommonKeys.KEY_BOUNDARY_CONDITION_THEME, 
//        bcTheme );
    dataModel.setData( 
        ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE, 
        bcTheme.getWorkspace() );
    dataModel.setData( 
        ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER, 
        this );
    
//    registerPopupBlocker( popupBlocker );
    popupBlocker.registerPopupBlockerToActiveMapView();
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
    {
    try
    {
      dataModel.setData( 
          ICommonKeys.KEY_SELECTED_DISPLAY,
          parent.getDisplay() );
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
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection, org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  
  public void clickPopup( Point p )
  {
    if( strategy != null )
    {
      strategy.clickPopup( p );
    }
        
//    MapPanel mapPanel = (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
//    JPopupMenu popupMenu = new JPopupMenu();
//    
//    JMenuItem addElement = new JMenuItem();
//    addElement.setText( "Add Element" );
//    addElement.setIcon( new ImageIcon(PluginUtilities.findResource(
//                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
//                                  "icons/elcl16/add.gif" )));
//   
//    JMenuItem removeElement = new JMenuItem();
//    removeElement.setText("Remove Element");
//    removeElement.setIcon( new ImageIcon(PluginUtilities.findResource(
//                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
//                                  "icons/elcl16/remove.gif" )));
//    
//    JMenuItem addBoundaryUP = new JMenuItem();
//    addBoundaryUP.setText("Add Boundary UP");
//    addBoundaryUP.setIcon( new ImageIcon(PluginUtilities.findResource(
//                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
//                                  "icons/elcl16/addBoundary.gif" )));
//    
//    JMenuItem removeBoundaryUP = new JMenuItem();
//    removeBoundaryUP.setText("Remove Boundary UP");
//    removeBoundaryUP.setIcon( new ImageIcon(PluginUtilities.findResource(
//                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
//                                  "icons/elcl16/remove.gif" )));
//        
//    
//    JMenuItem addBoundaryDOWN = new JMenuItem();
//    addBoundaryDOWN.setText("Add Boundary DOWN");
//    addBoundaryDOWN.setIcon( new ImageIcon(PluginUtilities.findResource(
//                                 KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
//                                 "icons/elcl16/addBoundary.gif" )));
//
//    
//    JMenuItem removeBoundaryDOWN = new JMenuItem();
//    removeBoundaryDOWN.setText("remove Boundary DOWN");
//    removeBoundaryDOWN.setIcon( new ImageIcon(PluginUtilities.findResource(
//                                KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
//                                "icons/elcl16/remove.gif" )));
//
//    popupMenu.add( addElement);
//    popupMenu.add( removeElement);
//    popupMenu.addSeparator();
//    popupMenu.add( addBoundaryUP);
//    popupMenu.add( removeBoundaryUP);
//    popupMenu.addSeparator();
//    popupMenu.add( addBoundaryDOWN);
//    popupMenu.add( removeBoundaryDOWN);
//    
//    popupMenu.show( mapPanel, p.x, p.y );    
  }

  
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    if( strategy != null )
    {
      strategy.doubleClickedLeft( p );
    }
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
    if( strategy != null )
    {
      strategy.doubleClickedRight( p );
    }    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    if( strategy != null )
    {
      strategy.dragged( p );
    }    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    try
    {
      MapPanel mapPanel =
        (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
      IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell != null )
      {
        mapModell.removeTheme( calUnitTheme );
      }
    }
    catch ( Exception e) 
    {
      e.printStackTrace();
    }
    
    try
    {
      if( strategy != null )
      {
        strategy.finish();
      }
    }
    catch (Exception e) 
    {
      e.printStackTrace();
    }
    
    popupBlocker.unRegisterPopupBlocker();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    return name;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    return tooltip;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyPressed( e );
    }
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyReleased( e );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyTyped( e );
    }
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    if( strategy != null )
    {
      strategy.leftClicked( p );
    }
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    if( strategy != null )
    {
      strategy.leftPressed( p );
    }
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    if( strategy != null )
    {
      strategy.leftReleased( p );
    }
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    if( strategy != null )
    {
      strategy.middleClicked( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    if( strategy != null )
    {
      strategy.middlePressed( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    if( strategy != null )
    {
      strategy.middleReleased( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    if( strategy != null )
    {
      strategy.moved( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    if( strategy != null )
    {
      strategy.paint( g );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    if( strategy != null )
    {
      strategy.rightClicked( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    if( strategy != null )
    {
      strategy.rightPressed( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    if( strategy != null )
    {
      strategy.rightReleased( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    if( strategy != null )
    {
      strategy.setSelection( selection );
    }
  }
 
  /**
   * Sets a new strategy for this widget
   * 
   * @param strategy the new strategy to set or null
   *        if the actual strategy is to be removed
   */
  public void setStrategy( IWidget strategy )
  {
    if( this.strategy != null )
    {
      this.strategy.finish();
    }
    
    this.strategy = strategy;
    if( this.strategy != null )
    {
     ICommandTarget commandPoster = 
       (ICommandTarget) dataModel.getData( ICommonKeys.KEY_COMMAND_TARGET );
     MapPanel mapPanel = 
         (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
     this.strategy.activate( commandPoster, mapPanel ); 
    }
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider#getGrabDistance()
   */
  public double getGrabDistance( )
  {
    if( strategy instanceof IGrabDistanceProvider )
    {
      return ((IGrabDistanceProvider)strategy).getGrabDistance();
    }
    
    System.out.println("getting fix grab distance");
    
    final MapPanel mapPanel = 
      dataModel.getData( MapPanel.class, ICommonKeys.KEY_MAP_PANEL );
    return MapUtilities.calculateWorldDistance( mapPanel , 6 );
  }
}
  