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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
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
import org.kalypso.ogc.gml.widgets.AbstractWidget;
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
 * the new selection is added to the current selection. INTERSECT / CONTAINS (via 'ALT' key):<br>
 * If pressed selection is done by using INTERSECT-method.
 *
 * @author Thomas Jung
 */
public class SelectFeatureWidget extends AbstractWidget
{
  public static final int GRAB_RADIUS = 20;

  private IGeometryBuilder m_selectionTypeDelegate;

  private IKalypsoFeatureTheme[] m_themes;

  private final QName[] m_qnamesToSelect;

  private final QName m_geomQName;

  private FeatureList[] m_featureLists;

  private Feature m_foundFeature;

  private boolean m_toggleMode;

  private boolean m_addMode;

  private Point m_currentPoint;

  private boolean m_intersectMode;

  /**
   * @param qnamesToSelect
   *          Only feature, that substitutes at least one of the given feature types (as qnames), will be selected from
   *          the map. If all feature should be selected, use new QName[]{ Feature.QNAME }
   * @param geomQName
   */
  public SelectFeatureWidget( final String name, final String toolTip, final QName qnamesToSelect[], final QName geomQName )
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
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  private void reinit( )
  {
    // default: selection by Rectangle
    // TODO: get from outside
    if( m_selectionTypeDelegate == null )
      m_selectionTypeDelegate = new RectangleGeometryBuilder( getMapPanel().getMapModell().getCoordinatesSystem() );

    m_themes = null;
    m_featureLists = null;
    m_foundFeature = null;

    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();
    mapPanel.repaintMap();
    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      m_themes = new IKalypsoFeatureTheme[1];
      m_featureLists = new FeatureList[1];

      m_themes[0] = (IKalypsoFeatureTheme) activeTheme;
      m_featureLists[0] = m_themes == null ? null : m_themes[0].getFeatureList();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;
    final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );

    m_foundFeature = null;

    if( m_featureLists == null )
      return;

    for( final FeatureList featureList : m_featureLists )
    {
      if( featureList == null )
        continue;

      if( m_selectionTypeDelegate instanceof PointGeometryBuilder )
      {
        /* Grab next feature */
        final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), currentPos, GRAB_RADIUS * 2 );
        final QName geomQName = findGeomQName( featureList, m_geomQName );
        m_foundFeature = GeometryUtilities.findNearestFeature( currentPos, grabDistance, featureList, geomQName, m_qnamesToSelect );

        /* grap to the first feature that you can get */
        if( m_foundFeature != null )
          continue;
      }
    }

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    m_currentPoint = p;

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();

    super.dragged( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_selectionTypeDelegate == null )
      return;

    try
    {
      if( m_selectionTypeDelegate instanceof RectangleGeometryBuilder )
      {
        final GM_Point point = MapUtilities.transform( mapPanel, p );
        final GM_Object object = m_selectionTypeDelegate.addPoint( point );
        if( object != null )
        {
          doSelect( object );
          m_selectionTypeDelegate.reset();
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    super.leftPressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_selectionTypeDelegate == null )
      return;

    try
    {
      GM_Point point = null;
      if( m_selectionTypeDelegate instanceof PointGeometryBuilder )
      {
        /* just snap to grabbed feature */
        if( m_foundFeature != null )
        {
          final List<Feature> selectedFeatures = new ArrayList<Feature>();
          selectedFeatures.add( m_foundFeature );
          final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
          changeSelection( selectionManager, selectedFeatures, m_themes, m_addMode, m_toggleMode );
        }
        m_selectionTypeDelegate.reset();
      }
      else if( m_selectionTypeDelegate instanceof PolygonGeometryBuilder )
      {
        point = MapUtilities.transform( mapPanel, p );
        m_selectionTypeDelegate.addPoint( point );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    super.leftClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_selectionTypeDelegate == null )
      return;

    final GM_Point point = MapUtilities.transform( mapPanel, p );
    try
    {
      m_selectionTypeDelegate.addPoint( point );
      final GM_Object selectGeometry = m_selectionTypeDelegate.finish();
      doSelect( selectGeometry );
      m_selectionTypeDelegate.reset();

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    super.doubleClickedLeft( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    m_toggleMode = false;
    m_addMode = false;
    m_intersectMode = false;

    final int keyCode = e.getKeyCode();

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
      // "ESC": deselection
      case KeyEvent.VK_ESCAPE:
        m_selectionTypeDelegate.reset();
        final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
        selectionManager.clear();
        break;
    }

    mapPanel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    m_toggleMode = false;
    m_addMode = false;
    m_intersectMode = false;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
      mapPanel.repaintMap();

    super.keyReleased( e );
  }

  private void changeGeometryBuilder( final IMapPanel mapPanel )
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
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_selectionTypeDelegate != null )
      m_selectionTypeDelegate.paint( g, mapPanel.getProjection(), m_currentPoint );

    if( m_foundFeature != null )
    {
      final QName geomQName = m_geomQName != null ? m_geomQName : m_foundFeature.getFeatureType().getDefaultGeometryProperty().getQName();
      MapUtils.paintGrabbedFeature( g, mapPanel, m_foundFeature, geomQName );
    }

    super.paint( g );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
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
          doSelect( selectGeometry );
          m_selectionTypeDelegate.reset();
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  private void doSelect( final GM_Object selectGeometry )
  {
    if( selectGeometry == null )
      return;
    // select feature from featureList by using the selectGeometry
    if( m_featureLists == null )
      return;

    final List<Feature> selectedFeatures = new ArrayList<Feature>();

    for( final FeatureList featureList : m_featureLists )
    {
      if( featureList == null )
        continue;

      final QName geomQName = findGeomQName( featureList, m_geomQName );

      final Collection<Feature> selectedSubList = selectFeatures( featureList, selectGeometry, m_qnamesToSelect, geomQName, m_intersectMode );
      if( selectedSubList != null )
        selectedFeatures.addAll( selectedSubList );
    }

    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    changeSelection( selectionManager, selectedFeatures, m_themes, m_addMode, m_toggleMode );
  }

  /**
   * Finds the geometry property to select from.<br>
   * If a default type is specified, this will always be used.<br>
   * Else, the default geometry property of the target type of the list will be taken.
   */
  public static QName findGeomQName( final FeatureList featureList, final QName defaultGeometry )
  {
    if( defaultGeometry != null )
      return defaultGeometry;

    final IFeatureType targetFeatureType = featureList.getParentFeatureTypeProperty().getTargetFeatureType();
    return targetFeatureType.getDefaultGeometryProperty().getQName();
  }

  public static void changeSelection( final IFeatureSelectionManager selectionManager, final List<Feature> selectedFeatures, final IKalypsoFeatureTheme[] themes, final boolean add, final boolean toggle )
  {
    if( selectedFeatures.size() == 0 )
      selectionManager.clear();

    final List<Feature> toRemove = new ArrayList<Feature>();
    final List<EasyFeatureWrapper> toAdd = new ArrayList<EasyFeatureWrapper>();

    for( final IKalypsoFeatureTheme theme : themes )
    {
      /* consider the selection modes */
      final CommandableWorkspace workspace = theme.getWorkspace();

      for( final Feature feature : selectedFeatures )
      {
        if( add )
        {
          if( !selectionManager.isSelected( feature ) )
            toAdd.add( new EasyFeatureWrapper( workspace, feature, feature.getOwner(), feature.getParentRelation() ) );
        }
        else if( toggle )
        {
          if( selectionManager.isSelected( feature ) )
            toRemove.add( feature );
          else
            toAdd.add( new EasyFeatureWrapper( workspace, feature, feature.getOwner(), feature.getParentRelation() ) );
        }
        else
          toAdd.add( new EasyFeatureWrapper( workspace, feature, feature.getOwner(), feature.getParentRelation() ) );
      }
    }

    if( !add && !toggle )
    {
      // REMARK: instead of invoking
      // selectionManager.clear();
      // We add all features to the remove-list; else we get two selection-change events here
      final EasyFeatureWrapper[] allFeatures = selectionManager.getAllFeatures();
      for( final EasyFeatureWrapper feature : allFeatures )
        toRemove.add( feature.getFeature() );
    }

    selectionManager.changeSelection( toRemove.toArray( new Feature[toRemove.size()] ), toAdd.toArray( new EasyFeatureWrapper[toAdd.size()] ) );
  }

  private Collection<Feature> selectFeatures( final FeatureList featureList, final GM_Object selectGeometry, final QName[] qnamesToSelect, final QName geomQName, final boolean intersectMode )
  {
    final Collection<Feature> selectedFeatures = new HashSet<Feature>();

    // why the check here?
    if( selectGeometry instanceof GM_Surface )
    {
      final GM_Envelope envelope = selectGeometry.getEnvelope();
      final GMLWorkspace workspace = featureList.getParentFeature().getWorkspace();
      final List< ? > result = featureList.query( envelope, null );

      for( final Object object : result )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );
        final IFeatureType featureType = feature.getFeatureType();

        if( GMLSchemaUtilities.substitutes( featureType, qnamesToSelect ) )
        {
          final IPropertyType pt = featureType.getProperty( geomQName );
          final Object property = feature.getProperty( pt );

          if( pt.isList() )
          {
            final List<?> list = (List< ? >) property;
            for( final Object elmt : list )
            {
              if( intersects( selectGeometry, (GM_Object) elmt, intersectMode ) )
                selectedFeatures.add( feature );
            }
          }
          else
          {
            if( intersects( selectGeometry, (GM_Object) property, intersectMode ) )
              selectedFeatures.add( feature );
          }
        }
      }
    }

    return selectedFeatures;
  }

  private boolean intersects( final GM_Object selectGeom, final GM_Object geom, final boolean intersectMode )
  {
    if( geom == null )
      return false;

    if( intersectMode == true )
      return selectGeom.intersects( geom );

    return selectGeom.contains( geom );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#getToolTip()
   */
  @Override
  public String getToolTip( )
  {
    final StringBuffer sb = new StringBuffer().append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.1" ) ); //$NON-NLS-1$

    if( m_selectionTypeDelegate instanceof PolygonGeometryBuilder )
      sb.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.2" ) ); //$NON-NLS-1$
    else if( m_selectionTypeDelegate instanceof RectangleGeometryBuilder )
      sb.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.3" ) ); //$NON-NLS-1$
    else
      sb.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.4" ) ); //$NON-NLS-1$

    if( m_addMode == true )
      sb.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.5" ) ); //$NON-NLS-1$
    if( m_toggleMode == true )
      sb.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.6" ) ); //$NON-NLS-1$
    if( m_intersectMode == true )
      sb.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.7" ) ); //$NON-NLS-1$

    return sb.toString();
  }

  public void setThemes( final IKalypsoFeatureTheme[] themes )
  {
    m_themes = new IKalypsoFeatureTheme[themes.length];
    m_themes = themes;

    m_featureLists = new FeatureList[themes.length];

    if( m_themes == null )
    {
      m_featureLists = null;
      return;
    }

    for( int i = 0; i < m_themes.length; i++ )
    {
      if( m_themes[i] != null )
      {
        final FeatureList featureList = m_themes[i].getFeatureList();
        m_featureLists[i] = featureList;
      }
    }
  }

  public static Feature grabNextFeature( final IMapPanel mapPanel, final GM_Point currentPos, final IKalypsoFeatureTheme[] themes, final QName[] qnamesToSelect, final QName geomQName )
  {
    for( final IKalypsoFeatureTheme theme : themes )
    {
      if( theme == null )
        continue;

      final FeatureList featureList = theme.getFeatureList();
      if( featureList == null )
        continue;

      /* Grab next feature */
      final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPos, SelectFeatureWidget.GRAB_RADIUS * 2 );
      final QName geomQNameToSelect = SelectFeatureWidget.findGeomQName( featureList, geomQName );
      final Feature foundFeature = GeometryUtilities.findNearestFeature( currentPos, grabDistance, featureList, geomQNameToSelect, qnamesToSelect );
      if( foundFeature != null )
      {
        final FeatureList visibles = theme.getFeatureListVisible( foundFeature.getEnvelope() );
        if( visibles != null && visibles.contains( foundFeature ) )
          return foundFeature;
      }
    }

    return null;
  }
}
