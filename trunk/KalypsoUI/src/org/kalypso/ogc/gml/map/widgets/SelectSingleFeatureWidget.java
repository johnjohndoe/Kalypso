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
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PointGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.util.MapUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * A widget to select a single feature.<br>
 * De-selection is not possible.<br>
 * <br>
 * <code>Ctrl</code> toggles toggle-modus.
 * 
 * @author Gernot Belger
 */
public class SelectSingleFeatureWidget extends AbstractWidget
{
  private final IMapModellListener m_mapModellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeStatusChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeStatusChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      reinit();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeActivated(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeActivated( final IMapModell source, final IKalypsoTheme previouslyActive, final IKalypsoTheme nowActive )
    {
      reinit();
    }
  };

  private IGeometryBuilder m_geometryBuilder;

  private IKalypsoFeatureTheme[] m_themes;

  private final QName[] m_qnamesToSelect;

  private final QName m_geomQName;

  private FeatureList[] m_featureLists;

  private Feature m_foundFeature;

  private Point m_currentPoint;

  private boolean m_toggle = false;


  public SelectSingleFeatureWidget( )
  {
    this( "single select widget", "", new QName[] { Feature.QNAME_FEATURE }, null );
  }

  /**
   * @param qnamesToSelect
   *          Only feature, that substitutes at least one of the given feature types (as qnames), will be selected from
   *          the map. If all feature should be selected, use new QName[]{ Feature.QNAME }
   * @param geomQName
   */
  public SelectSingleFeatureWidget( final String name, final String toolTip, final QName qnamesToSelect[], final QName geomQName )
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

    mapPanel.getMapModell().addMapModelListener( m_mapModellListener );

    reinit();
  }

  protected void reinit( )
  {
    m_geometryBuilder = new PointGeometryBuilder( getMapPanel().getMapModell().getCoordinatesSystem() );

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
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    final IMapModell mapModell = getMapPanel().getMapModell();
    if( mapModell != null )
      mapModell.removeMapModelListener( m_mapModellListener );
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

    final IMapPanel mapPanel = getMapPanel();

    if( m_featureLists == null || mapPanel == null )
      return;

    m_foundFeature = SelectFeatureWidget.grabNextFeature( mapPanel, currentPos, m_themes, m_qnamesToSelect, m_geomQName );

    mapPanel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    try
    {
      /* just snap to grabbed feature */
      if( m_foundFeature != null )
      {
        final List<Feature> selectedFeatures = new ArrayList<Feature>();
        selectedFeatures.add( m_foundFeature );
        final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
        if( selectionManager.size() == 1 && m_toggle )
        {
          // Do not allow deselection of last item
          if( selectionManager.getAllFeatures()[0].getFeature() == m_foundFeature )
            return;
        }

        SelectFeatureWidget.changeSelection( selectionManager, selectedFeatures, m_themes, false, m_toggle );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
// // Always reinit, this makes the widget more robust against change of the active theme
// // Else, we must be a panel listener etc.
// reinit();
      m_geometryBuilder.reset();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    m_toggle = false;

    final int keyCode = e.getKeyCode();
    switch( keyCode )
    {
      // "STRG": Toggle mode
      case KeyEvent.VK_CONTROL:
        m_toggle = true;
        break;
    }

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
      mapPanel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    m_toggle = false;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
      mapPanel.repaintMap();
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

    if( m_currentPoint != null )
      m_geometryBuilder.paint( g, mapPanel.getProjection(), m_currentPoint );

    if( m_foundFeature != null )
    {
      final QName geomQName = m_geomQName != null ? m_geomQName : m_foundFeature.getFeatureType().getDefaultGeometryProperty().getQName();
      MapUtils.paintGrabbedFeature( g, mapPanel, m_foundFeature, geomQName );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#getToolTip()
   */
  @Override
  public String getToolTip( )
  {
    final StringBuffer sb = new StringBuffer().append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.1" ) ); //$NON-NLS-1$

    sb.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget.4" ) );

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
        if( featureList != null )
          m_featureLists[i] = featureList;
      }
    }
  }
}
