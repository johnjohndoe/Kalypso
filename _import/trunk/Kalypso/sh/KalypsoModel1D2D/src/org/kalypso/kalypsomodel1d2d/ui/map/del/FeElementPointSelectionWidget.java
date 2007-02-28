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
package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.List;


import org.eclipse.jface.viewers.ISelection;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Implements a Strategy for selectio an fe element
 * 
 * @author Patrice Congo
 */
public class FeElementPointSelectionWidget implements IWidget
{

  private ICommandTarget commandPoster;
  
  private MapPanel mapPanel;
 
  private IFEDiscretisationModel1d2d model1d2d;

  private boolean addToSelection;

  private IKalypsoFeatureTheme featureTheme;

  private CommandableWorkspace cmdWorkspace;
  
  private JMSelector selector= new JMSelector();
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
   this.commandPoster=commandPoster;
   this.mapPanel=mapPanel;
   IMapModell mapModell = mapPanel.getMapModell();
  this.model1d2d = 
     UtilMap.findFEModelTheme( 
       mapModell, 
       Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT );
   Assert.throwIAEOnNull( this.model1d2d, "Could not found model" );
   this.featureTheme = 
     UtilMap.findEditableThem( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT );
   cmdWorkspace = this.featureTheme.getWorkspace();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection, org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
    
  }
  
  Point draggedPoint0;
  Point draggedPoint1;

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    if(draggedPoint0==null)
    {
      draggedPoint0=p;
    }
    else
    {
      draggedPoint1=p;
      mapPanel.getMapModell().fireModellEvent( null );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    if(KeyEvent.CTRL_MASK == e.getModifiers())
    {
      this.addToSelection=true;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    if(e.VK_CONTROL  == e.getKeyCode())
    {
      this.addToSelection=false;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    GM_Point point = MapUtilities.transform( mapPanel, p );
    //model1d2d.getElements().getWrappedList().query( env, result )
    
    
    List selected = 
      selector.select( point.getPosition(), model1d2d.getElements().getWrappedList() );
   addSelection( selected );
  }

  
  private final void addSelection(List selected)
  {
    IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    if(addToSelection)
    {
      //features 
    }
    else
    {
      selectionManager.clear();
    }
    
    final int SIZE = selected.size();
    EasyFeatureWrapper[] featuresToAdd = new EasyFeatureWrapper[SIZE];
    Feature parentFeature=model1d2d.getWrappedFeature();
    IFeatureType featureType = parentFeature.getFeatureType();
    IRelationType parentFeatureProperty=
      (IRelationType)featureType.getProperty( 
          Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );
    
    
    for(int i=0;i<SIZE;i++)
    {
      Feature curFeature=(Feature)selected.get( i );
      featuresToAdd[i]=
        new EasyFeatureWrapper(
          cmdWorkspace,
          curFeature,
          parentFeature,
          parentFeatureProperty);
    }
    
    Feature[] featuresToRemove= new Feature[]{};
    selectionManager.changeSelection( featuresToRemove, featuresToAdd ); 
  }
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    if(draggedPoint0!=null && draggedPoint1!=null)
    {
      System.out.println("DO Select");
      GM_Point point0=MapUtilities.transform( mapPanel, draggedPoint0 );
      GM_Point point1=MapUtilities.transform( mapPanel, draggedPoint1 );
      GM_Envelope env= GeometryFactory.createGM_Envelope( 
                                    point0.getPosition(), point1.getPosition() );
      List selected=selector.select( env, model1d2d.getElements().getWrappedList(), false );
      addSelection( selected );
    }
    draggedPoint0=null;
    draggedPoint1=null;
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    //TODO calculate real rect
    if(draggedPoint0!=null && draggedPoint1!=null)
    {
      double x = Math.min( draggedPoint0.getX(),draggedPoint1.getX());
      double y = Math.min( draggedPoint0.getY(),draggedPoint1.getY());;
      double width = Math.abs( draggedPoint0.getX()-draggedPoint1.getX());
      double height = Math.abs( draggedPoint0.getY()-draggedPoint1.getY());
      
      g.drawRect( (int)x, (int)y, (int)width, (int)height );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    
  }

  

}
