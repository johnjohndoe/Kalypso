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
package org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1d2dCalUnitTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;

/**
 * @author Madanagopal
 *
 */
public class CalculationUnitPerformWidget implements IWidgetWithOptions, 
                                                     IWidget, 
                                                     IWidgetWithStrategy,
                                                     IGrabDistanceProvider
{
  private CalculationUnitDataModel dataModel = new CalculationUnitDataModel();
  private CalculationUnitPerformWidgetFace calcWidgetFace = new CalculationUnitPerformWidgetFace(dataModel);
  private String toolTip;
  private IWidget strategy;
  private String name;
  private Model1d2dCalUnitTheme calUnitTheme;
  /**
   * The constructor.
   */
  
  public CalculationUnitPerformWidget() {
    this("Calculation Unit Perform","Berechnungseinheiten Modellieren");
  }

  public CalculationUnitPerformWidget(String name, String toolTip ){
    this.name = name;
    this.toolTip = toolTip;
  }
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
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite, org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    try
    {
      dataModel.setData( 
          ICommonKeys.KEY_SELECTED_DISPLAY,
          parent.getDisplay() );
      return calcWidgetFace.createControl( parent );
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
    if(calcWidgetFace!=null)
    {
      calcWidgetFace.disposeControl();
    }
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    
    dataModel.setData( ICommonKeys.KEY_MAP_PANEL, mapPanel );
    IMapModell mapModell = mapPanel.getMapModell();
    IFEDiscretisationModel1d2d m_model = UtilMap.findFEModelTheme( mapModell );
    dataModel.setData( 
        ICommonKeys.KEY_DISCRETISATION_MODEL, m_model );
    dataModel.setData(
        ICommonKeys.KEY_FEATURE_WRAPPER_LIST, 
        CalUnitOps.getModelCalculationUnits( m_model ) );
    dataModel.setData( ICommonKeys.WIDGET_WITH_STRATEGY, this );
    
    IKalypsoFeatureTheme targetTheme = UtilMap.findEditableTheme( 
        mapModell, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );
    dataModel.setData( 
        ICommonKeys.KEY_COMMAND_MANAGER, 
        targetTheme.getWorkspace());
    calUnitTheme = 
      new Model1d2dCalUnitTheme("Aktuelle CalUnit",mapModell);
//    mapModell.addTheme( calUnitTheme );
    mapModell.insertTheme( calUnitTheme, 0 );
    dataModel.addKeyBasedDataChangeListener( calThemeUpdater );
    
    //command manager since it is use in the dirty pool object framework
    //the commandable workspace of the target theme is taken
    dataModel.setData( ICommonKeys.KEY_COMMAND_MANAGER, m_model.getWrappedFeature().getWorkspace());
    
    IKalypsoFeatureTheme bcTheme = UtilMap.findEditableTheme( 
        mapModell, 
        IBoundaryCondition.QNAME );
    if( bcTheme == null )
    {
      throw new RuntimeException("Could not find boundary condition theme");
    }

    final CommandableWorkspace bcWorkspace = bcTheme.getWorkspace();
    dataModel.setData( 
        ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE, 
        bcWorkspace );
    
    dataModel.setData( 
        ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER, 
        this );
    dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, CalUnitOps.getModelCalculationUnits( m_model));
    
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
    if( strategy != null )
    {
      strategy.finish();
    }    
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
    return toolTip;
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
   * @see org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy#setStrategy(org.kalypso.ogc.gml.widgets.IWidget)
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
