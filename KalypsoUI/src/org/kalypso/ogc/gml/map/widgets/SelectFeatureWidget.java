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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PointGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.RectangleGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.util.MapUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * General selection widget that allows multiple ways of selection (by point, by polygon, by rectangle). The user can
 * change between them by pressing the 'SPACE' key. <br>
 * <br>
 * Advanced selection modi are included as follows:<br>
 * TOGGLE (on via pressed 'STRG' key):<br>
 * already selected features gets toggled by the new selection (already selected get de-selected, not selected get
 * selected).<br>
 * ADD (on via pressed 'SHIFT' key):<br>
 * the new selection is added to the scurrent selection. INTERSECT / CONTAINS (via 'ALT' key):<br>
 * If pressed selection is done by using INTERSECT-method.
 * 
 * @author Thomas Jung
 */
public class SelectFeatureWidget extends AbstractWidget
{
  private IGeometryBuilder m_selectionTypeDelegate;

  private IKalypsoFeatureTheme m_theme;

  private final QName[] m_qnamesToSelect;

  private final QName m_geomQName;

  private FeatureList m_featureList;

  private Feature m_foundFeature;

  private boolean m_toggleMode;

  private boolean m_addMode;

  private final int m_grabRadius = 20;

  private Point m_currentPoint;

  private boolean m_intersectMode;

  public SelectFeatureWidget( String name, String toolTip, final QName qnamesToSelect[], final QName geomQName )
  {
    super( name, toolTip );
    m_qnamesToSelect = qnamesToSelect;
    m_geomQName = geomQName;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  private void reinit( )
  {
    // default: selection by Rectangle
    m_selectionTypeDelegate = new RectangleGeometryBuilder( getMapPanel().getMapModell().getCoordinatesSystem() );

    m_theme = null;
    m_featureList = null;
    m_foundFeature = null;

    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    m_theme = activeTheme instanceof IKalypsoFeatureTheme ? (IKalypsoFeatureTheme) activeTheme : null;
    m_featureList = m_theme == null ? null : m_theme.getFeatureList();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
    m_currentPoint = p;
    final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );

    m_foundFeature = null;

    if( m_featureList == null )
      return;

    if( m_selectionTypeDelegate instanceof PointGeometryBuilder )
    {
      /* Grab next feature */
      final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), currentPos, m_grabRadius * 2 );
      m_foundFeature = GeometryUtilities.findNearestFeature( currentPos, grabDistance, m_featureList, m_geomQName, m_qnamesToSelect );
    }

    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {

    m_currentPoint = p;

    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();

    super.dragged( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_selectionTypeDelegate == null )
      return;

    try
    {
      if( m_selectionTypeDelegate instanceof RectangleGeometryBuilder )
      {
        final GM_Point point = MapUtilities.transform( mapPanel, p );
        GM_Object object = m_selectionTypeDelegate.addPoint( point );
        if( object != null )
        {
          doSelect( object, m_featureList );
          m_selectionTypeDelegate.reset();
        }
      }
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    super.leftPressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_selectionTypeDelegate == null )
      return;

    try
    {
      GM_Point point = null;
      if( m_selectionTypeDelegate instanceof PointGeometryBuilder )
      {
        /* snap to grabbed feature */
        if( m_foundFeature != null )
        {
          final GM_Object geom = (GM_Object) m_foundFeature.getProperty( m_geomQName );
          if( geom instanceof GM_Point )
            point = (GM_Point) geom;
        }

        if( point == null )
          point = MapUtilities.transform( mapPanel, p );

        final GM_Object selectGeometry = m_selectionTypeDelegate.addPoint( point );
        doSelect( selectGeometry, m_featureList );
        m_selectionTypeDelegate.reset();
      }
      else if( m_selectionTypeDelegate instanceof PolygonGeometryBuilder )
      {
        point = MapUtilities.transform( mapPanel, p );
        m_selectionTypeDelegate.addPoint( point );
      }
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    super.leftClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_selectionTypeDelegate == null )
      return;

    final GM_Point point = MapUtilities.transform( mapPanel, p );
    try
    {
      m_selectionTypeDelegate.addPoint( point );
      final GM_Object selectGeometry = m_selectionTypeDelegate.finish();
      doSelect( selectGeometry, m_featureList );
      m_selectionTypeDelegate.reset();

    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    super.doubleClickedLeft( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( KeyEvent e )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    m_toggleMode = false;
    m_addMode = false;
    m_intersectMode = false;

    int keyCode = e.getKeyCode();

    switch( keyCode )
    {
      // "SHFT": Add mode
      case KeyEvent.VK_SHIFT:
        m_addMode = true;
        break;

      // "STRG": Toggle mode
      case KeyEvent.VK_CONTROL:
        m_toggleMode = true;
        break;

      // "ALT": switch between intersect / contains mode
      case KeyEvent.VK_ALT:
        m_intersectMode = true;
        break;

      // "SPACE": switch between polygon / rect mode
      case KeyEvent.VK_SPACE:
        changeGeometryBuilder( mapPanel );
        break;

      case KeyEvent.VK_ESCAPE:
        // TODO: clear selection
        changeGeometryBuilder( mapPanel );
        break;
    }
    mapPanel.repaint();

    super.keyPressed( e );

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( KeyEvent e )
  {
    m_toggleMode = false;
    m_addMode = false;
    m_intersectMode = false;

    MapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
      mapPanel.repaint();

    super.keyReleased( e );
  }

  private void changeGeometryBuilder( MapPanel mapPanel )
  {
    if( m_selectionTypeDelegate instanceof RectangleGeometryBuilder )
      m_selectionTypeDelegate = new PolygonGeometryBuilder( 0, mapPanel.getMapModell().getCoordinatesSystem() );
    else if( m_selectionTypeDelegate instanceof PolygonGeometryBuilder )
      m_selectionTypeDelegate = new PointGeometryBuilder( mapPanel.getMapModell().getCoordinatesSystem() );
    else
      m_selectionTypeDelegate = new RectangleGeometryBuilder( mapPanel.getMapModell().getCoordinatesSystem() );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_selectionTypeDelegate != null )
      m_selectionTypeDelegate.paint( g, mapPanel.getProjection(), m_currentPoint );

    if( m_foundFeature != null )
      MapUtils.paintGrabbedFeature( g, mapPanel, m_foundFeature, m_geomQName );

    super.paint( g );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_selectionTypeDelegate == null )
      return;

    if( m_selectionTypeDelegate instanceof RectangleGeometryBuilder )
    {
      final GM_Point point = MapUtilities.transform( mapPanel, p );

      try
      {
        final GM_Object selectGeometry = m_selectionTypeDelegate.addPoint( point );
        if( selectGeometry != null )
        {
          doSelect( selectGeometry, m_featureList );
          m_selectionTypeDelegate.reset();
        }
      }
      catch( Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private void doSelect( GM_Object selectGeometry, FeatureList featureList )
  {
    if( selectGeometry == null )
      return;
    // select feature from featureList by using the selectGeometry
    final List<Feature> selectedFeatures = selectFeatures( featureList, selectGeometry, m_qnamesToSelect, m_geomQName, m_intersectMode );

    /* consider the selection modes */
    final CommandableWorkspace workspace = m_theme.getWorkspace();
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    if( selectedFeatures.size() == 0 )
      selectionManager.clear();

    final List<Feature> toRemove = new ArrayList<Feature>();
    final List<EasyFeatureWrapper> toAdd = new ArrayList<EasyFeatureWrapper>();

    if( m_addMode == true )
    {
      // Add selection
      for( final Feature feature : selectedFeatures )
      {
        if( !selectionManager.isSelected( feature ) )
          toAdd.add( new EasyFeatureWrapper( workspace, feature, feature.getParent(), feature.getParentRelation() ) );
      }
    }
    else if( m_toggleMode == true )
    {
      // Toggle selection
      for( final Feature feature : selectedFeatures )
      {
        if( selectionManager.isSelected( feature ) )
          toRemove.add( feature );
        else
          toAdd.add( new EasyFeatureWrapper( workspace, feature, feature.getParent(), feature.getParentRelation() ) );
      }
    }
    else
    {
      selectionManager.clear();
      for( final Feature feature : selectedFeatures )
      {
        toAdd.add( new EasyFeatureWrapper( workspace, feature, feature.getParent(), feature.getParentRelation() ) );
      }
    }

    selectionManager.changeSelection( toRemove.toArray( new Feature[toRemove.size()] ), toAdd.toArray( new EasyFeatureWrapper[toAdd.size()] ) );
  }

  @SuppressWarnings("unchecked")
  private static List<Feature> selectFeatures( final FeatureList featureList, final GM_Object selectGeometry, final QName[] qnamesToSelect, final QName geomQName, final boolean intersectMode )
  {
    final List<Feature> selectedFeatures = new ArrayList<Feature>();

    if( selectGeometry instanceof GM_Surface )
    {
      final GM_Surface surface = (GM_Surface) selectGeometry;

      final GM_Envelope envelope = surface.getEnvelope();
      final GMLWorkspace workspace = featureList.getParentFeature().getWorkspace();
      final List result = featureList.query( envelope, null );

      for( final Object object : result )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );

        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), qnamesToSelect ) )
        {
          final GM_Object geom = (GM_Object) feature.getProperty( geomQName );

          if( intersectMode == true )
          {
            if( geom != null && surface.intersects( geom ) )
              selectedFeatures.add( feature );
          }
          else
          {
            if( geom != null && surface.contains( geom ) )
              selectedFeatures.add( feature );
          }
        }
      }
    }
    else if( selectGeometry instanceof GM_Point )
    {
      final GM_Point point = (GM_Point) selectGeometry;

      final GM_Envelope envelope = GeometryUtilities.getEnvelope( selectGeometry );

      final GMLWorkspace workspace = featureList.getParentFeature().getWorkspace();
      final List result = featureList.query( envelope, null );

      for( final Object object : result )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );

        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), qnamesToSelect ) )
        {
          final GM_Object geom = (GM_Object) feature.getProperty( geomQName );
          if( geom != null && point.intersects( geom ) )
            selectedFeatures.add( feature );
        }
      }
    }

    return selectedFeatures;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#getToolTip()
   */
  @Override
  public String getToolTip( )
  {
    StringBuffer sb = new StringBuffer().append( "Selektionsmodus: " );

    if( m_selectionTypeDelegate instanceof PolygonGeometryBuilder )
      sb.append( "Polygon" );
    else if( m_selectionTypeDelegate instanceof RectangleGeometryBuilder )
      sb.append( "Rechteck" );
    else
      sb.append( "Punkt" );

    if( m_addMode == true )
      sb.append( " <Hinzuf¸gen>" );
    if( m_toggleMode == true )
      sb.append( " <Umschalten>" );
    if( m_intersectMode == true )
      sb.append( " <Verschneidung>" );

    return sb.toString();
  }

  public void setTheme( IKalypsoFeatureTheme theme )
  {
    m_theme = theme;
    m_featureList = m_theme == null ? null : m_theme.getFeatureList();
  }

}
