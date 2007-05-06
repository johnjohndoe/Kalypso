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

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 */
public class ApplyColorOnFENETWidget implements IWidgetWithOptions, IWidget
{

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  ColorOnFEDataModel feDataModel = new ColorOnFEDataModel();

  ApplyColorOnFENETWidgetFace widgetFace = new ApplyColorOnFENETWidgetFace( feDataModel );

  private String name;

  private String tooltip;

  public ApplyColorOnFENETWidget( )
  {
    this.name = "Rauheitszuweisung anzeigen";
    this.tooltip = "Aktiviert die Anzeige der zugewiesenen Element-Rauheiten. ";
  }

  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    try
    {
      return widgetFace.createControl( parent );
    }
    catch( Throwable th )
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
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    IMapModell mapModell = mapPanel.getMapModell();
    feDataModel.setMapModell( mapModell );
    feDataModel.setMapPanel( mapPanel );

    IKalypsoFeatureTheme roughnessTheme = UtilMap.findEditableTheme( mapModell, 
                                  KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_TERRAIN_ELE_MODEL );
    Feature systemFeature = roughnessTheme.getFeatureList().getParentFeature();// ;
    System.out.println( systemFeature.getFeatureType().getQName());
    System.out.println( systemFeature.getParent().getFeatureType().getQName() );
    ITerrainModel system = (ITerrainModel) systemFeature.getParent().getAdapter( ITerrainModel.class );
    feDataModel.setRoughnessPolygonCollection( system.getRoughnessPolygonCollection() );
    feDataModel.setRoughnessTheme( roughnessTheme );
    feDataModel.setMapPanel( mapPanel );    
//    IKalypsoFeatureTheme feNodesTheme = UtilMap.findEditableTheme( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );
//    Feature feNodesFeature = feNodesTheme.getFeatureList().getParentFeature();
    
    IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme(mapModell );
    feDataModel.setDiscretisationModel1d2d(model1d2d);
    
    // To access the IFE1D2DElements
//    IFeatureWrapperCollection<IFE1D2DElement> elements = model1d2d.getElements();
//    
//    for (IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> ink:elements) {
//      //System.out.println("Poly Elements :"+ink.);  
//    }
    //IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge> sys =  
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    // TODO Auto-generated method stub
    return name;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    // TODO Auto-generated method stub
    return tooltip;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    // TODO Auto-generated method stub

  }

}
