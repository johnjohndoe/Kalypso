/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml;

import java.awt.Color;
import java.awt.Graphics;
import java.net.URL;
import java.util.ArrayList;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;

/**
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class ScrabLayerFeatureTheme implements IKalypsoFeatureTheme
{

  public static final String STYLE_NAME = "ScrabLayerStyle";

  public static final String POLYGON_GEOM_PROP_NAME = "geomPolygon";

  public static final String LINESTRING_GEOM_PROP_NAME = "geomLineString";

  public static final String POINT_GEOM_PROP_NAME = "geomPoint";

  public static final String FEATURE_MEMBER = "scrabLayerMember";

  private final IKalypsoFeatureTheme m_scrabLayerTheme;

  private final IFeatureSelectionManager m_selectionManager;

  public ScrabLayerFeatureTheme( IFeatureSelectionManager selectionManager )
  {
    m_selectionManager = selectionManager;
    final URL scrabLayerURL = getClass().getResource( "/org/kalypso/core/resources/basicScrabLayer.gml" );
    CommandableWorkspace workspace = null;
    try
    {
      final GMLWorkspace createGMLWorkspace = GmlSerializer.createGMLWorkspace( scrabLayerURL );
      workspace = new CommandableWorkspace( createGMLWorkspace );
    }
    catch( Exception e )
    {
      // TODO what is to be done??
      e.printStackTrace();
      
      // at least do not continue to initialize
      m_scrabLayerTheme = null;
      return;
    }
    
    // IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();
    m_scrabLayerTheme = new KalypsoFeatureTheme( workspace, FEATURE_MEMBER, "Skizzier-Thema", selectionManager );
    // add styles for visualisation
    ArrayList<Symbolizer> symbolizers = new ArrayList<Symbolizer>();
    symbolizers.add( StyleFactory.createPointSymbolizer( StyleFactory.createGraphic( null, null, 1, 5, 0 ), POINT_GEOM_PROP_NAME ) );
    symbolizers.add( StyleFactory.createLineSymbolizer( StyleFactory.createStroke(), LINESTRING_GEOM_PROP_NAME ) );
    symbolizers.add( StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill( Color.GRAY, 0.5d ), POLYGON_GEOM_PROP_NAME ) );
    FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( STYLE_NAME, symbolizers.toArray( new Symbolizer[symbolizers.size()] ) );
    UserStyle style = (UserStyle_Impl) StyleFactory.createStyle( STYLE_NAME, STYLE_NAME, "empty Abstract", featureTypeStyle );
    m_scrabLayerTheme.addStyle( new KalypsoUserStyle( style, style.getName() ) );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getWorkspace()
   */
  public CommandableWorkspace getWorkspace( )
  {
    return m_scrabLayerTheme.getWorkspace();

  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSchedulingRule()
   */
  public ISchedulingRule getSchedulingRule( )
  {
    return m_scrabLayerTheme.getSchedulingRule();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    return m_scrabLayerTheme.getFeatureType();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#addStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void addStyle( KalypsoUserStyle style )
  {
    m_scrabLayerTheme.addStyle( style );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#removeStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void removeStyle( KalypsoUserStyle style )
  {
    m_scrabLayerTheme.removeStyle( style );

  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getStyles()
   */
  public UserStyle[] getStyles( )
  {
    return m_scrabLayerTheme.getStyles();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureList()
   */
  public FeatureList getFeatureList( )
  {
    return m_scrabLayerTheme.getFeatureList();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureListVisible(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public FeatureList getFeatureListVisible( GM_Envelope env )
  {
    return m_scrabLayerTheme.getFeatureListVisible( env );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose( )
  {
    m_scrabLayerTheme.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( Graphics g, GeoTransform p, double scale, GM_Envelope bbox, boolean selected )
  {
    m_scrabLayerTheme.paint( g, p, scale, bbox, selected );

  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getName()
   */
  public String getName( )
  {
    return m_scrabLayerTheme.getName();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getType()
   */
  public String getType( )
  {
    return "";
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setName(java.lang.String)
   */
  public void setName( String name )
  {
    m_scrabLayerTheme.setName( name );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return m_scrabLayerTheme.getBoundingBox();
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( ModellEventListener listener )
  {
    m_scrabLayerTheme.addModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( ModellEventListener listener )
  {
    m_scrabLayerTheme.removeModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( ModellEvent event )
  {
    m_scrabLayerTheme.fireModellEvent( event );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    m_scrabLayerTheme.onModellChange( modellEvent );
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( ICommand command, Runnable runnable )
  {
    m_scrabLayerTheme.postCommand( command, runnable );
  }

  public IFeatureSelectionManager getSelectionManager( )
  {
    return m_selectionManager;
  }
}
