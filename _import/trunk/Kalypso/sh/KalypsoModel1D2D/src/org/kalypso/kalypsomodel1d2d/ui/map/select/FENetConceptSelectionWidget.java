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
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Implements a strategy for selection of elements on the map
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class FENetConceptSelectionWidget implements IWidget, IGrabDistanceProvider
{
  private final QName[] m_themeElementQNames;

  private IKalypsoFeatureTheme[] m_featureThemes;

  private String m_toolTip;

  private ICommandTarget m_commandPoster;

  private MapPanel m_mapPanel;

  private boolean m_appendToCurrentSelection;

  private PolygonGeometryBuilder m_polygonGeometryBuilder;

  private IMapModell m_mapModell;

  private Point m_draggedPoint0;

  private Point m_draggedPoint1;

  private boolean m_polygonSelectModus;

  private Point m_currentPoint;

  private String m_name;

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
    m_name = name;
    m_toolTip = toolTip;
    m_themeElementQNames = themeElementsQNames;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    m_commandPoster = commandPoster;
    m_mapPanel = mapPanel;
    m_mapModell = mapPanel.getMapModell();
    m_featureThemes = new IKalypsoFeatureTheme[m_themeElementQNames.length];
    for( int i = 0; i < m_themeElementQNames.length; i++ )
      m_featureThemes[i] = UtilMap.findEditableTheme( m_mapModell, m_themeElementQNames[i] );
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
    if( m_polygonSelectModus )
    {
      if( m_polygonGeometryBuilder != null )
      {
        try
        {
          final GM_Object object = m_polygonGeometryBuilder.finish();
          changeSelection( getSelectedByPolygon( object ) );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          throw new RuntimeException( e );
        }
        finally
        {
          m_polygonGeometryBuilder = new PolygonGeometryBuilder( 0, m_mapModell.getCoordinatesSystem() );
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
    if( m_draggedPoint0 == null )
    {
      m_draggedPoint0 = p;
    }
    else
    {
      m_draggedPoint1 = p;
    }

    if( m_mapPanel != null )
      m_mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    m_mapPanel.getSelectionManager().clear();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    return m_toolTip;
  }

  public void setToolTip( final String newToolTip )
  {
    m_toolTip = newToolTip;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    if( KeyEvent.VK_CONTROL == keyCode )
      m_appendToCurrentSelection = true;
    if( KeyEvent.VK_SHIFT == keyCode )
    {
      m_polygonSelectModus = true;
      if( m_polygonGeometryBuilder == null )
        m_polygonGeometryBuilder = new PolygonGeometryBuilder( 0, m_mapModell.getCoordinatesSystem() );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    if( KeyEvent.VK_CONTROL == keyCode )
      m_appendToCurrentSelection = false;
    if( KeyEvent.VK_SHIFT == keyCode )
    {
      m_polygonSelectModus = false;
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
    final GM_Point point = MapUtilities.transform( m_mapPanel, p );
    if( m_polygonSelectModus )
    {
      try
      {
        m_polygonGeometryBuilder.addPoint( point );
        // m_mapPanel.repaint();
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
      try
      {
        final List<EasyFeatureWrapper> selected = getSelectedByPoint( point );
        changeSelection( selected );
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private final void changeSelection( final List<EasyFeatureWrapper> newUserSelection )
  {
    final IFeatureSelectionManager selectionManager = m_mapPanel.getSelectionManager();
    if( m_appendToCurrentSelection )
    {
      final List<EasyFeatureWrapper> currentSelection = new ArrayList<EasyFeatureWrapper>( Arrays.asList( selectionManager.getAllFeatures() ) );
      for( final EasyFeatureWrapper newlySelectedFeature : newUserSelection )
        if( currentSelection.contains( newlySelectedFeature ) )
          currentSelection.remove( newlySelectedFeature );
        else
          currentSelection.add( newlySelectedFeature );
      selectionManager.setSelection( currentSelection.toArray( new EasyFeatureWrapper[0] ) );
    }
    else
      selectionManager.setSelection( newUserSelection.toArray( new EasyFeatureWrapper[0] ) );
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
    if( m_draggedPoint0 != null && m_draggedPoint1 != null )
    {
      final GM_Point point0 = MapUtilities.transform( m_mapPanel, m_draggedPoint0 );
      final GM_Point point1 = MapUtilities.transform( m_mapPanel, m_draggedPoint1 );
      final GM_Envelope envelope = GeometryFactory.createGM_Envelope( point0.getPosition(), point1.getPosition() );
      changeSelection( getSelectedByEnvelope( envelope ) );
    }
    m_draggedPoint0 = null;
    m_draggedPoint1 = null;
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
    m_currentPoint = p;
    m_mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( final Graphics g )
  {
    // TODO calculate real rect
    if( m_draggedPoint0 != null && m_draggedPoint1 != null )
    {
      final double x = Math.min( m_draggedPoint0.getX(), m_draggedPoint1.getX() );
      final double y = Math.min( m_draggedPoint0.getY(), m_draggedPoint1.getY() );

      final double width = Math.abs( m_draggedPoint0.getX() - m_draggedPoint1.getX() );
      final double height = Math.abs( m_draggedPoint0.getY() - m_draggedPoint1.getY() );

      g.drawRect( (int) x, (int) y, (int) width, (int) height );
    }

    if( m_polygonGeometryBuilder != null )
    {
      final GeoTransform projection = m_mapPanel.getProjection();
      m_polygonGeometryBuilder.paint( g, projection, m_currentPoint );
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

  public List getSelectedByPolygon( final GM_Object polygon ) throws GM_Exception
  {
    final Geometry polygonGeometry = JTSAdapter.export( polygon );
    final List roughSelection = m_featureThemes[0].getFeatureList().query( polygon.getEnvelope(), null );
    for( int i = 1; i < m_featureThemes.length; i++ )
      roughSelection.addAll( m_featureThemes[0].getFeatureList().query( polygon.getEnvelope(), null ) );
    final List fineSelection = new ArrayList();
    for( final Object object : roughSelection )
      if( object instanceof Feature )
      {
        final GM_Object defaultGeometryProperty = ((Feature) object).getDefaultGeometryProperty();
        if( defaultGeometryProperty != null )
        {
          final Geometry featureGeometry = JTSAdapter.export( defaultGeometryProperty );
          if( polygonGeometry.contains( featureGeometry ) )
            fineSelection.add( object );
        }
      }
    return packForSelectionManager( fineSelection );
  }

  public List<EasyFeatureWrapper> getSelectedByEnvelope( final GM_Envelope envelope )
  {
    final List roughSelection = JMSelector.select( envelope, m_featureThemes[0].getFeatureList(), true );
    for( int i = 1; i < m_featureThemes.length; i++ )
      roughSelection.addAll( JMSelector.select( envelope, m_featureThemes[i].getFeatureList(), true ) );
    return packForSelectionManager( roughSelection );
  }

  public List<EasyFeatureWrapper> getSelectedByPoint( final GM_Point point ) throws GM_Exception
  {
    final Geometry pointGeometry = JTSAdapter.export( point );
    final List roughSelection = m_featureThemes[0].getFeatureList().query( point.getPosition(), null );
    for( int i = 1; i < m_featureThemes.length; i++ )
      roughSelection.addAll( m_featureThemes[0].getFeatureList().query( point.getPosition(), null ) );
    final List fineSelection = new ArrayList();
    for( final Object object : roughSelection )
      if( object instanceof Feature )
      {
        final Geometry featureGeometry = JTSAdapter.export( ((Feature) object).getDefaultGeometryProperty() );
        if( pointGeometry.coveredBy( featureGeometry ) )
        {
          fineSelection.add( object );
          return packForSelectionManager( fineSelection );
        }
      }
    return packForSelectionManager( fineSelection );
  }

  private List<EasyFeatureWrapper> packForSelectionManager( final List selected )
  {
    final List<EasyFeatureWrapper> featuresToAdd = new ArrayList<EasyFeatureWrapper>();
    for( final Object object : selected )
    {
      if( object instanceof Feature )
      {
        final Feature selectedFeature = (Feature) object;
        for( int i = 0; i < m_themeElementQNames.length; i++ )
        {
          final IFeatureType featureType = selectedFeature.getFeatureType();
          if( featureType.equals( m_themeElementQNames[i] ) || GMLSchemaUtilities.substitutes( featureType, m_themeElementQNames[i] ) )
          {
            final Feature parentFeature = m_featureThemes[i].getFeatureList().getParentFeature();
            final IRelationType parentFeatureProperty = (IRelationType) parentFeature.getFeatureType().getProperty( m_themeElementQNames[i] );
            featuresToAdd.add( new EasyFeatureWrapper( m_featureThemes[i].getWorkspace(), selectedFeature, parentFeature, parentFeatureProperty ) );
          }
        }
      }
    }
    return featuresToAdd;
  }

  /**
   * To get the wrapped selected feature
   * 
   * @param targetWrapClass
   *            the class the selection feature should be wrapped
   * @return return an array containing the wrappers of the selected features
   */
  public <T> T[] getWrappedSelectedFeature( final Class<T> targetWrapClass )
  {
    final Feature[] selectedFeature = getSelectedFeature();
    final T[] wrappers = (T[]) Array.newInstance( targetWrapClass, selectedFeature.length );
    for( int i = selectedFeature.length - 1; i >= 0; i-- )
    {
      wrappers[i] = (T) selectedFeature[i].getAdapter( targetWrapClass );
    }
    return wrappers;
  }

  /**
   * To get all selected feature
   * 
   * @return a {@link Feature} array containing the selected feature
   */
  public Feature[] getSelectedFeature( )
  {
    final EasyFeatureWrapper[] easyFeatureWrappers = m_mapPanel.getSelectionManager().getAllFeatures();
    final Feature features[] = new Feature[easyFeatureWrappers.length];
    for( int i = features.length - 1; i >= 0; i-- )
    {
      features[i] = easyFeatureWrappers[i].getFeature();
    }
    return features;
  }

  public void postCommand( final ICommand command )
  {
    m_commandPoster.postCommand( command, null );
  }

  public boolean isPolygonSelectModus( )
  {
    return m_polygonSelectModus;
  }

  public MapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  /**
   * To get the last clicked point.
   * 
   * @return a {@link GM_Point} representing the last clicked point
   * 
   */
  public GM_Point getCurrentPoint( )
  {
    if( m_currentPoint == null )
      return null;
    else
      return MapUtilities.transform( m_mapPanel, m_currentPoint );
  }

  public double getGrabDistance( )
  {
    return MapUtilities.calculateWorldDistance( m_mapPanel, getCurrentPoint(), 6 );
  }
}
