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
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
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
import org.kalypsodeegree.model.feature.FeatureList;
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
@SuppressWarnings({"unchecked", "hiding"})
public class FENetConceptSelectionWidget implements IWidget
{
  private class QNameBasedSelectionContext
  {
    final private QName m_themeElementsQName;

    private IKalypsoFeatureTheme m_featureTheme;

//    private IFEDiscretisationModel1d2d m_model1d2d;
    

    private CommandableWorkspace m_cmdWorkspace;

    public QNameBasedSelectionContext( final QName themeElementsQName )
    {
      m_themeElementsQName = themeElementsQName;
    }

    public void init( final IMapModell mapModell ) throws IllegalArgumentException
    {
//      m_model1d2d = UtilMap.findFEModelTheme( mapModell );
//      Assert.throwIAEOnNull( this.m_model1d2d, "Could not found model" );
      m_featureTheme = UtilMap.findEditableTheme( mapModell, m_themeElementsQName );
      m_cmdWorkspace = this.m_featureTheme.getWorkspace();
    }

    public List getSelectedByPolygon( final GM_Object polygon, final ISelectionFilter selectionFilter )
    {
// GM_Object object =
// polygonGeometryBuilder.finish();
      final GM_Envelope env = polygon.getEnvelope();
      final List selected = m_selector.select( env, m_featureTheme.getFeatureList(),// model1d2d.getElements().getWrappedList(),
      true );
      for( int i = selected.size() - 1; i >= 0; i-- )
      {
        // TODO WHAT is this doing???
        ((Feature) selected.get( i )).getDefaultGeometryProperty();
      }
      return filterSelected( selected, selectionFilter );
    }

    public List<EasyFeatureWrapper> getSelectedByEnvelope( final GM_Envelope env, final ISelectionFilter selectionFilter, final boolean selectWithinBox )
    {

      final List selected = m_selector.select( env, m_featureTheme.getFeatureList(), selectWithinBox );
      return filterSelected( selected, selectionFilter );

    }

    private List<EasyFeatureWrapper> filterSelected( final List selected, final ISelectionFilter selectionFilter )
    {

      final int SIZE = selected.size();
      final List<EasyFeatureWrapper> featuresToAdd = new ArrayList<EasyFeatureWrapper>( SIZE );
      final Feature parentFeature = m_featureTheme.getFeatureList().getParentFeature();//m_model1d2d.getWrappedFeature();
      final IFeatureType featureType = parentFeature.getFeatureType();
      final IRelationType parentFeatureProperty = (IRelationType) featureType.getProperty( m_themeElementsQName );

      if( selectionFilter == null )
      {
        for( int i = 0; i < SIZE; i++ )
        {
          final Feature curFeature = (Feature) selected.get( i );
          featuresToAdd.add( new EasyFeatureWrapper( m_cmdWorkspace, curFeature, parentFeature, parentFeatureProperty ) );
        }
      }
      else
      {
        for( int i = 0; i < SIZE; i++ )
        {
          final Feature curFeature = (Feature) selected.get( i );
          if( selectionFilter.accept( curFeature ) )
          {

            final EasyFeatureWrapper easyFeatureWrapper = new EasyFeatureWrapper( m_cmdWorkspace, curFeature, parentFeature, parentFeatureProperty );
            featuresToAdd.add( easyFeatureWrapper );

          }
        }
      }
      return featuresToAdd;

    }

  }

  private ICommandTarget commandPoster;

  private MapPanel mapPanel;

  private boolean addToSelection;

  private PolygonGeometryBuilder m_polygonGeometryBuilder;

  private final JMSelector m_selector = new JMSelector();

  private IMapModell m_mapModell;

  private final QNameBasedSelectionContext m_selectionContexts[];

  private ISelectionFilter m_selectionFilter;

  private Point draggedPoint0;

  private Point draggedPoint1;

  private boolean polygonSelectModus;

  private CS_CoordinateSystem crs;

  private Point currentPoint;

  private final String toolTip;

  private String name;

  /**
   * Creates a new fe net concept selection to select in a theme showing feature of the given q-name. The provided
   * q-name is used to look for a suitable themes.
   * 
   * Note that {@link QNameBasedSelectionFilter}s, which support substitution, are created for those q-names.
   * 
   * use {@link #setSelectionFilter(ISelectionFilter)} to override this beahaviour
   * 
   * @param themeElementsQNames
   *            an array of q-name to select
   * @param name
   *            the name of the widget
   * @param toolTip
   *            the tooltip of the widget
   * @see #FENetConceptSelectionWidget(QName[], String, String)
   */
  public FENetConceptSelectionWidget( final QName themeElementsQName, final String name, final String toolTip )
  {
    this( new QName[] { themeElementsQName }, name, toolTip );
  }

  /**
   * Creates a new fe net concept selection to select in themes showing feature of the given q-names. The provided
   * qnames are used to look for suitable themes.
   * 
   * Note that {@link QNameBasedSelectionFilter}s, which support substitution, are created for those q-names.
   * 
   * use {@link #setSelectionFilter(ISelectionFilter)} to override this beahaviour
   * 
   * @param themeElementsQNames
   *            an array of q-name to select
   * @param name
   *            the name of the widget
   * @param toolTip
   *            the tooltip of the widget
   * 
   */
  public FENetConceptSelectionWidget( final QName themeElementsQNames[], final String name, final String toolTip )
  {
    this.name = name;
    this.toolTip = toolTip;
    this.m_selectionContexts = new QNameBasedSelectionContext[themeElementsQNames.length];

    QNameBasedSelectionFilter qnameBasedFilter = null;
    for( int i = 0; i < themeElementsQNames.length; i++ )
    {
      m_selectionContexts[i] = new QNameBasedSelectionContext( themeElementsQNames[i] );
      // TODO patrice check if this is working properly
      // make qname base filter
      if( qnameBasedFilter == null )
      {
        qnameBasedFilter = QNameBasedSelectionFilter.getFilterForQName( themeElementsQNames[i] );
        qnameBasedFilter.setAcceptSubstituables( true );
      }
      else
      {
        qnameBasedFilter.add( themeElementsQNames[i] );
      }
    }

    m_selectionFilter = qnameBasedFilter;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    this.commandPoster = commandPoster;
    this.mapPanel = mapPanel;
    m_mapModell = mapPanel.getMapModell();
    // QName name = Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT;
    for( final QNameBasedSelectionContext selectionContext : m_selectionContexts )
    {
      selectionContext.init( m_mapModell );
    }
    crs = m_mapModell.getCoordinatesSystem();

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( final ISelection selection, final MapPanel mapPanel )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( final Point p )
  {
    if( polygonSelectModus )
    {
      if( m_polygonGeometryBuilder != null )
      {
        try
        {
          final GM_Object object = m_polygonGeometryBuilder.finish();
// GM_Envelope env = object.getEnvelope();
// List selected =
// selector.select(
// env,
// featureTheme.getFeatureList(),//model1d2d.getElements().getWrappedList(),
// false );
// for(int i=selected.size()-1;i>=0;i--)
// {
// ((Feature)selected.get( i )).getDefaultGeometryProperty();
// }
// addSelection( selected );
          for( final QNameBasedSelectionContext selectionContext : m_selectionContexts )
          {
            final List selectedByPolygon = selectionContext.getSelectedByPolygon( object, m_selectionFilter );
            addSelection( selectedByPolygon );
          }

// selector.selectNearestHandel( geom, pos, snapRadius )

        }
        catch( final Exception e )
        {
          e.printStackTrace();
          throw new RuntimeException( e );
        }
        finally
        {
          m_polygonGeometryBuilder = new PolygonGeometryBuilder( 0, crs );
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( final Point p )
  {
    if( draggedPoint0 == null )
    {
      draggedPoint0 = p;
    }
    else
    {
      draggedPoint1 = p;
      mapPanel.repaint();
    }
    // TODO: check if this repaint is really necessary
    if( mapPanel != null )
      mapPanel.repaint();

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    
    IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    selectionManager.clear();//changeSelection( featuresToRemove, featuresToAdd );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    return name;
  }

  public void setName( final String name )
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

  public void setToolTip( final String newToolTip )
  {
    Assert.throwIAEOnNullParam( newToolTip, "newToolTip" );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( final KeyEvent e )
  {
    final int modifiers = e.getModifiers();
    if( KeyEvent.CTRL_MASK == modifiers )
    {
      this.addToSelection = true;
    }
    else if( e.isShiftDown() )
    {
      this.polygonSelectModus = true;
      if( m_polygonGeometryBuilder == null )
      {
        m_polygonGeometryBuilder = new PolygonGeometryBuilder( 0, crs );
// System.out.println("DDCDCD="+crs);
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    if( e.VK_CONTROL == keyCode )
    {
      this.addToSelection = false;
    }
    else if( e.VK_SHIFT == keyCode )
    {
      this.polygonSelectModus = false;
      m_polygonGeometryBuilder = null;
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( final KeyEvent e )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( final Point p )
  {

    final GM_Point point = MapUtilities.transform( mapPanel, p );
    if( polygonSelectModus )
    {
      try
      {
        m_polygonGeometryBuilder.addPoint( point );
        mapPanel.repaint();
      }
      catch( final Exception e )
      {
        // TODO better exception handling
        e.printStackTrace();
        throw new RuntimeException( e );
      }
    }
    else
    {
      // klick point select modus
      // model1d2d.getElements().getWrappedList().query( env, result )

      // TODO get the delta from preferences
      final double delta = MapUtilities.calculateWorldDistance( mapPanel, point, 6 );
      final GM_Position min = GeometryFactory.createGM_Position( point.getX() - delta, point.getY() - delta );

      final GM_Position max = GeometryFactory.createGM_Position( point.getX() + delta, point.getY() + delta );
      final GM_Envelope env = GeometryFactory.createGM_Envelope( min, max );

      for( final QNameBasedSelectionContext selectionContext : m_selectionContexts )
      {
        final List selectedByEnvelope = selectionContext.getSelectedByEnvelope( env, m_selectionFilter, false );
        addSelection( selectedByEnvelope );
      }
    }
  }

  private final void addSelection( final List<EasyFeatureWrapper> selected )
  {
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final Feature[] featuresToRemove;

    if( addToSelection )
    {
      // features
      final List<EasyFeatureWrapper> toRemoveSelection = new ArrayList<EasyFeatureWrapper>( Arrays.asList( selectionManager.getAllFeatures() ) );
      toRemoveSelection.retainAll( selected );
      selected.removeAll( toRemoveSelection );
// System.out.println("Selected size="+selected.size());
      final int size = toRemoveSelection.size();
      featuresToRemove = new Feature[size];
      for( int i = size - 1; i >= 0; i-- )
      {
        featuresToRemove[i] = toRemoveSelection.get( i ).getFeature();
      }
    }
    else
    {
      selectionManager.clear();
      featuresToRemove = new Feature[] {};
    }

    final int SIZE = selected.size();
    final EasyFeatureWrapper[] featuresToAdd = selected.toArray( new EasyFeatureWrapper[SIZE] );

    selectionManager.changeSelection( featuresToRemove, featuresToAdd );
    selectionMade();
  }

  /**
   * Called after a selection habe been made
   */
  protected void selectionMade( )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( final Point p )
  {
// System.out.println("Left pressed");
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( final Point p )
  {
    if( draggedPoint0 != null && draggedPoint1 != null )
    {

      final GM_Point point0 = MapUtilities.transform( mapPanel, draggedPoint0 );
      final GM_Point point1 = MapUtilities.transform( mapPanel, draggedPoint1 );
      final GM_Envelope env = GeometryFactory.createGM_Envelope( point0.getPosition(), point1.getPosition() );
      for( final QNameBasedSelectionContext selectionContext : m_selectionContexts )
      {
        final List selectedByEnvelope = selectionContext.getSelectedByEnvelope( env, m_selectionFilter, true );
        addSelection( selectedByEnvelope );
      }
    }
    draggedPoint0 = null;
    draggedPoint1 = null;

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( final Point p )
  {
// currentPoint = MapUtilities.transform( mapPanel, p );
    currentPoint = p;

// TODO: check if this repaint is necessary for the widget
    if( mapPanel != null )
      mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( final Graphics g )
  {
    // TODO calculate real rect
    if( draggedPoint0 != null && draggedPoint1 != null )
    {
      final double x = Math.min( draggedPoint0.getX(), draggedPoint1.getX() );
      final double y = Math.min( draggedPoint0.getY(), draggedPoint1.getY() );
      
      final double width = Math.abs( draggedPoint0.getX() - draggedPoint1.getX() );
      final double height = Math.abs( draggedPoint0.getY() - draggedPoint1.getY() );

      g.drawRect( (int) x, (int) y, (int) width, (int) height );
    }

    if( m_polygonGeometryBuilder != null )
    {
      final GeoTransform projection = mapPanel.getProjection();
      m_polygonGeometryBuilder.paint( g, projection, currentPoint );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {

  }

  public void setSelectionFilter( final ISelectionFilter selectionFilter )
  {
    this.m_selectionFilter = selectionFilter;
  }

  public ISelectionFilter getSelectionFilter( )
  {
    return m_selectionFilter;
  }

  /**
   * To get the wrapped selected feature
   * 
   * @param targetWrapClass the class the selection feature
   *            should be wrapped
   * @return return an array containing the wrappers of the selected
   *           features  
   */
  public <T>T[] getWrappedSelectedFeature( Class<T> targetWrapClass )
  {
    Feature[] selectedFeature = getSelectedFeature();
    T[] wrappers = (T[])Array.newInstance( targetWrapClass, selectedFeature.length);
    for(int i= selectedFeature.length-1;i>=0;i--)
    {
      wrappers[i] = 
        (T) selectedFeature[i].getAdapter( targetWrapClass );
    }
    return wrappers;
  }
  
  
  /**
   * To get all selected feature
   * @return a {@link Feature} array containing the selected feature
   */
  public Feature[] getSelectedFeature( )
  {
    final EasyFeatureWrapper[] easyFeatureWrappers = mapPanel.getSelectionManager().getAllFeatures();
    final Feature features[] = new Feature[easyFeatureWrappers.length];
    for( int i = features.length - 1; i >= 0; i-- )
    {
      features[i] = easyFeatureWrappers[i].getFeature();
    }
    return features;
  }

  public IFEDiscretisationModel1d2d getModel1d2d( final QName themeQName )
  {
    if( themeQName == null )
    {
      return null;
    }

    for( final QNameBasedSelectionContext selectionContext : m_selectionContexts )
    {
      if( themeQName.equals( selectionContext.m_themeElementsQName ) )
      {
//        return selectionContext.m_model1d2d;
        final FeatureList featureList = selectionContext.m_featureTheme.getFeatureList();
        final Feature parentFeature = featureList.getParentFeature();
        return (IFEDiscretisationModel1d2d) parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
      }
    }
    return null;
  }

  public void postCommand( final ICommand command )
  {
    commandPoster.postCommand( command, null );
  }

  public IKalypsoFeatureTheme getTheme( final QName themeQName )
  {
    if( themeQName == null )
    {
      return null;
    }
    for( final QNameBasedSelectionContext selectionContext : m_selectionContexts )
    {
      if( themeQName.equals( selectionContext.m_themeElementsQName ) )
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

  public MapPanel getMapPanel( )
  {
    return mapPanel;
  }
  
  /**
   * To get the last clicked point.
   * 
   * @return a {@link GM_Point} representing the last clicked point
   * 
   */
  public GM_Point getCurrentPoint()
  {
    if( currentPoint == null )
    {
      return null;
    }
    else
    {
      final GM_Point point = 
        MapUtilities.transform( mapPanel, currentPoint );
      return point;
    }
  }
}
