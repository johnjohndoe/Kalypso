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
package org.kalypso.kalypsomodel1d2d.ui.map.select;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Implements a Strategy for selectio an fe element
 * 
 * @author Patrice Congo
 */
public class FENetConceptSelectionWidget implements IWidget
{
  private class QNameBasedSelectionContext
  {
    final private QName m_themeElementsQName;
    private IKalypsoFeatureTheme m_featureTheme;
    private IFEDiscretisationModel1d2d m_model1d2d;
    private CommandableWorkspace m_cmdWorkspace;
    
    
    public QNameBasedSelectionContext( final QName themeElementsQName )
    {
      m_themeElementsQName = themeElementsQName;
    }
    
    public void init( final IMapModell mapModell ) throws IllegalArgumentException
    {
      m_model1d2d = 
        UtilMap.findFEModelTheme( 
          mapModell );
      Assert.throwIAEOnNull( this.m_model1d2d, "Could not found model" );
      m_featureTheme = 
        UtilMap.findEditableTheme( 
            mapModell, 
            m_themeElementsQName );
      m_cmdWorkspace = this.m_featureTheme.getWorkspace();
    }
    
    public List getSelectedByPolygon(GM_Object polygon,ISelectionFilter selectionFilter)
    {
//      GM_Object object = 
//        polygonGeometryBuilder.finish();
      GM_Envelope env = polygon.getEnvelope();
      List selected = 
              m_selector.select( 
                      env, 
                      m_featureTheme.getFeatureList(),//model1d2d.getElements().getWrappedList(), 
                      true );
      for(int i=selected.size()-1;i>=0;i--)
      {
        //TODO WHAT is this doing???
        ((Feature)selected.get( i )).getDefaultGeometryProperty();
      }
      return filterSelected( selected,selectionFilter);
    }
    
    public List<EasyFeatureWrapper> getSelectedByEnvelope(
                                          GM_Envelope env, 
                                          ISelectionFilter selectionFilter, 
                                          boolean selectWithinBox)
    {
      
      List selected = 
        m_selector.select( 
            env,
            m_featureTheme.getFeatureList(),
            selectWithinBox);
      return filterSelected( selected, selectionFilter );
      
    }
    
    private List<EasyFeatureWrapper> filterSelected(List selected, ISelectionFilter selectionFilter)
    {
        
        final int SIZE = selected.size();
        List<EasyFeatureWrapper> featuresToAdd = new ArrayList<EasyFeatureWrapper>(SIZE);
        Feature parentFeature=m_model1d2d.getWrappedFeature();
        IFeatureType featureType = parentFeature.getFeatureType();
        IRelationType parentFeatureProperty=
          (IRelationType)featureType.getProperty( 
                                    m_themeElementsQName);
        
        if(selectionFilter==null)
        {
          for(int i=0;i<SIZE;i++)
          {
            Feature curFeature=(Feature)selected.get( i );
            featuresToAdd.add( 
              new EasyFeatureWrapper(
                m_cmdWorkspace,
                curFeature,
                parentFeature,
                parentFeatureProperty));
          }
        }
        else
        {
          for(int i=0;i<SIZE;i++)
          {
            Feature curFeature=(Feature)selected.get( i );
            if(selectionFilter.accept( curFeature ))
            {
              
                EasyFeatureWrapper easyFeatureWrapper = new EasyFeatureWrapper(
                  m_cmdWorkspace,
                  curFeature,
                  parentFeature,
                  parentFeatureProperty);
              featuresToAdd.add( easyFeatureWrapper );
              
            }
          }
        }
        return featuresToAdd;
        
    }
    
  }
  
  private ICommandTarget commandPoster;
  
  private MapPanel mapPanel;
 
//  private IFEDiscretisationModel1d2d model1d2d;

  private boolean addToSelection;

  
//  private IKalypsoFeatureTheme featureTheme;

  
  private PolygonGeometryBuilder m_polygonGeometryBuilder; 
  
  private JMSelector m_selector= new JMSelector();

  private IMapModell m_mapModell;
  
  private QNameBasedSelectionContext m_selectionContexts[];
  
  private ISelectionFilter m_selectionFilter;
  
  public FENetConceptSelectionWidget(
                      QName themeElementsQName,
                      String name, 
                      String toolTip )
  {
    this(new QName[]{themeElementsQName},name,toolTip);    
  }
  
  public FENetConceptSelectionWidget(
                  QName themeElementsQNames[],
                  String name, 
                  String toolTip )
  {
    this.name=name;
    this.toolTip=toolTip;
    this.m_selectionContexts= new QNameBasedSelectionContext[themeElementsQNames.length];
    for(int i=0;i<themeElementsQNames.length;i++)
    {
      m_selectionContexts[i] = 
        new QNameBasedSelectionContext(themeElementsQNames[i]);
    }
  }
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( 
                ICommandTarget commandPoster, 
                MapPanel mapPanel )
  {
   this.commandPoster=commandPoster;
   this.mapPanel=mapPanel;
   m_mapModell = mapPanel.getMapModell();
   //QName name = Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT;
   for(QNameBasedSelectionContext selectionContext:m_selectionContexts)
   {
    selectionContext.init( m_mapModell );
   }
   crs = m_mapModell.getCoordinatesSystem();
   
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
    if(polygonSelectModus)
    {
      if(m_polygonGeometryBuilder!=null)
      {
        try
        {
          GM_Object object = 
              m_polygonGeometryBuilder.finish();
//          GM_Envelope env = object.getEnvelope();
//          List selected = 
//                  selector.select( 
//                          env, 
//                          featureTheme.getFeatureList(),//model1d2d.getElements().getWrappedList(), 
//                          false );
//          for(int i=selected.size()-1;i>=0;i--)
//          {
//            ((Feature)selected.get( i )).getDefaultGeometryProperty();
//          }
//          addSelection( selected );
          for(QNameBasedSelectionContext selectionContext:m_selectionContexts)
          {
            List selectedByPolygon = selectionContext.getSelectedByPolygon( object,m_selectionFilter );
            addSelection(selectedByPolygon); 
          }
          
//          selector.selectNearestHandel( geom, pos, snapRadius )
          
        }
        catch( Exception e )
        {
          e.printStackTrace();
          throw new RuntimeException(e);
        }
        finally
        {
          m_polygonGeometryBuilder= new PolygonGeometryBuilder(0,crs);
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
    
  }
  
  Point draggedPoint0;
  Point draggedPoint1;

  private boolean polygonSelectModus;

  private CS_CoordinateSystem crs;

  private Point currentPoint;

  private String toolTip;

  private String name;

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
    //TODO: check if this repaint is really necessary
    if (mapPanel != null)
      mapPanel.repaint();

    
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
    return name;
  }

  public void setName( String name )
  {
    this.name = name;
  }
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    return toolTip;
  }
 
  public void setToolTip(String newToolTip)
  {
    Assert.throwIAEOnNullParam( newToolTip, "newToolTip" );
  }
  
  
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    int modifiers = e.getModifiers();
    if(KeyEvent.CTRL_MASK == modifiers)
    {
      this.addToSelection=true;
    }
    else if(e.isShiftDown())
    {
      this.polygonSelectModus=true;
      if(m_polygonGeometryBuilder==null)
      {
        m_polygonGeometryBuilder= 
          new PolygonGeometryBuilder(0,crs);
//        System.out.println("DDCDCD="+crs);
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    int keyCode = e.getKeyCode();
    if(e.VK_CONTROL  == keyCode)
    {
      this.addToSelection=false;
    }
    else if(e.VK_SHIFT == keyCode )
    {
      this.polygonSelectModus=false;
      m_polygonGeometryBuilder=null;
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
    if(polygonSelectModus)
    {
      try
      {
        m_polygonGeometryBuilder.addPoint( point );
        mapPanel.getMapModell().fireModellEvent( null );
      }
      catch (Exception e) 
      {
        //TODO better exception handling
        e.printStackTrace();
        throw new RuntimeException(e);
      }
    }
    else
    {
      //klick point select modus
      //model1d2d.getElements().getWrappedList().query( env, result )
       
      //TODO get the delta from preferences
      double delta=MapUtilities.calculateWorldDistance( mapPanel, point, 6 );
      GM_Position min = 
          GeometryFactory.createGM_Position( 
                point.getX()-delta, point.getY()-delta );
      
      GM_Position max = 
        GeometryFactory.createGM_Position( 
              point.getX()+delta, point.getY()+delta );
      GM_Envelope env= GeometryFactory.createGM_Envelope( min, max );

     for(QNameBasedSelectionContext selectionContext:m_selectionContexts)
     {
       List selectedByEnvelope = 
         selectionContext.getSelectedByEnvelope( env,m_selectionFilter, false );
       addSelection( selectedByEnvelope );
     }
    }
  }

  
  private final void addSelection(List<EasyFeatureWrapper> selected)
  {
    IFeatureSelectionManager selectionManager = 
                      mapPanel.getSelectionManager();
    final Feature[] featuresToRemove;
    
    if(addToSelection)
    {
      //features 
      List<EasyFeatureWrapper> toRemoveSelection = 
                  new ArrayList<EasyFeatureWrapper>(
                      Arrays.asList( 
                            selectionManager.getAllFeatures()));
      toRemoveSelection.retainAll( selected ); 
      selected.removeAll( toRemoveSelection );
//      System.out.println("Selected size="+selected.size());
      final int size = toRemoveSelection.size();
      featuresToRemove= new Feature[size];
      for(int i= size-1; i>=0;i--)
      {
        featuresToRemove[i] = 
            toRemoveSelection.get( i ).getFeature(); 
      }
    }
    else
    {
      selectionManager.clear();
      featuresToRemove= new Feature[]{};
    }
    
    final int SIZE = selected.size();
    EasyFeatureWrapper[] featuresToAdd = 
                     selected.toArray( new EasyFeatureWrapper[SIZE]);
    
    
    selectionManager.changeSelection( 
                        featuresToRemove, 
                        featuresToAdd ); 
  }
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
//    System.out.println("Left pressed");
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    if(draggedPoint0!=null && draggedPoint1!=null)
    {
      
     GM_Point point0=MapUtilities.transform( mapPanel, draggedPoint0 );
      GM_Point point1=MapUtilities.transform( mapPanel, draggedPoint1 );
      GM_Envelope env= 
        GeometryFactory.createGM_Envelope( 
                                    point0.getPosition(), 
                                    point1.getPosition() );
      for(QNameBasedSelectionContext selectionContext:m_selectionContexts)
      {
        List selectedByEnvelope = 
            selectionContext.getSelectedByEnvelope( env,m_selectionFilter, true );
        addSelection( selectedByEnvelope );
      }
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
//    currentPoint = MapUtilities.transform( mapPanel, p );
    currentPoint=p;
    
//  TODO: check if this repaint is necessary for the widget
    if ( mapPanel != null)
      mapPanel.repaint();
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
    
    if(m_polygonGeometryBuilder!=null)
    {
      GeoTransform projection = mapPanel.getProjection();
      m_polygonGeometryBuilder.paint( g, projection, currentPoint );
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

  public void setSelectionFilter( ISelectionFilter selectionFilter )
  {
    this.m_selectionFilter = selectionFilter;
  }
  
  public ISelectionFilter getSelectionFilter( )
  {
    return m_selectionFilter;
  }
  
  public Feature[] getSelectedFeature()
  {
    EasyFeatureWrapper[] easyFeatureWrappers = 
            mapPanel.getSelectionManager().getAllFeatures();
    Feature features[]=new Feature[easyFeatureWrappers.length];
    for(int i=features.length-1;i>=0;i--)
    {
      features[i]=easyFeatureWrappers[i].getFeature();
    }
    return features;
  }
  
  public IFEDiscretisationModel1d2d getModel1d2d(QName themeQName )
  {
    if(themeQName==null)
    {
      return null;
    }
    
    for(QNameBasedSelectionContext selectionContext:m_selectionContexts)
    {
      if(themeQName.equals( selectionContext.m_themeElementsQName))
      {
        return selectionContext.m_model1d2d;
      }
    }
    return null;
  }
  
  public void postCommand(ICommand command)
  {
    commandPoster.postCommand( command, null );
  }
  
  public IKalypsoFeatureTheme getTheme(QName themeQName)
  {
    if(themeQName==null)
    {
      return null;
    }
    for(QNameBasedSelectionContext selectionContext:m_selectionContexts)
    {
      if(themeQName.equals( selectionContext.m_themeElementsQName))
      {
        return selectionContext.m_featureTheme;
      }
    }
    
    return null;
  }

  public boolean isPolygonSelectModus( )
  {
    return polygonSelectModus;
  }
}
