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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;

/**
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class ScrabLayerFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme
{
  public static final String STYLE_NAME = "ScrabLayerStyle";

  public static final String POLYGON_GEOM_PROP_NAME = "geomPolygon";

  public static final String LINESTRING_GEOM_PROP_NAME = "geomLineString";

  public static final String POINT_GEOM_PROP_NAME = "geomPoint";

  public static final String FEATURE_MEMBER = "scrabLayerMember";

  private static final String CONTEXT = ScrabLayerFeatureTheme.class.getName();

  private final IKalypsoFeatureTheme m_scrabLayerTheme;

  private final IFeatureSelectionManager m_selectionManager;

  public ScrabLayerFeatureTheme( final IFeatureSelectionManager selectionManager, final IMapModell mapModel )
  {
    super( "Scrab-Layer", "Scrab", mapModel );
    m_selectionManager = selectionManager;
    final URL scrabLayerURL = getClass().getResource( "/org/kalypso/core/resources/basicScrabLayer.gml" );
    CommandableWorkspace workspace = null;
    try
    {
      final GMLWorkspace createGMLWorkspace = GmlSerializer.createGMLWorkspace( scrabLayerURL, null );
      workspace = new CommandableWorkspace( createGMLWorkspace );
    }
    catch( final Exception e )
    {
      // TODO what is to be done??
      e.printStackTrace();

      // at least do not continue to initialize
      m_scrabLayerTheme = null;
      return;
    }

    // IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();
    m_scrabLayerTheme = new KalypsoFeatureTheme( workspace, ScrabLayerFeatureTheme.FEATURE_MEMBER, "Skizzier-Thema", selectionManager, mapModel );
    // add styles for visualisation
    final ArrayList<Symbolizer> symbolizers = new ArrayList<Symbolizer>();
    symbolizers.add( StyleFactory.createPointSymbolizer( StyleFactory.createGraphic( null, null, 1, 5, 0 ), new PropertyName( ScrabLayerFeatureTheme.POINT_GEOM_PROP_NAME ) ) );
    symbolizers.add( StyleFactory.createLineSymbolizer( StyleFactory.createStroke(), new PropertyName( ScrabLayerFeatureTheme.LINESTRING_GEOM_PROP_NAME ) ) );
    symbolizers.add( StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill( Color.GRAY, 0.5d ), new PropertyName( ScrabLayerFeatureTheme.POLYGON_GEOM_PROP_NAME ) ) );
    // Added/commented by Dejan //symbolizers.add( StyleFactory.createRasterSymbolizer() );
    final FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( ScrabLayerFeatureTheme.STYLE_NAME, symbolizers.toArray( new Symbolizer[symbolizers.size()] ) );
    final UserStyle style = (UserStyle_Impl) StyleFactory.createStyle( ScrabLayerFeatureTheme.STYLE_NAME, ScrabLayerFeatureTheme.STYLE_NAME, "empty Abstract", featureTypeStyle );
    m_scrabLayerTheme.addStyle( new KalypsoUserStyle( style, style.getName() ) );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getWorkspace()
   */
  public CommandableWorkspace getWorkspace( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getWorkspace();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSchedulingRule()
   */
  public ISchedulingRule getSchedulingRule( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getSchedulingRule();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getFeatureType();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#addStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void addStyle( final KalypsoUserStyle style )
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.addStyle( style );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#removeStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void removeStyle( final KalypsoUserStyle style )
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.removeStyle( style );

  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getStyles()
   */
  public UserStyle[] getStyles( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getStyles();
    return new UserStyle[0];
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureList()
   */
  public FeatureList getFeatureList( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getFeatureList();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureListVisible(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public FeatureList getFeatureListVisible( final GM_Envelope env )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getFeatureListVisible( env );
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.dispose();
    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.paint( g, p, scale, bbox, selected, monitor );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getName()
   */
  @Override
  public String getName( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getName();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setName(java.lang.String)
   */
  @Override
  public void setName( final String name )
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.setName( name );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getContext()
   */
  @Override
  public String getContext( )
  {
    return ScrabLayerFeatureTheme.CONTEXT;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getFullExtent( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getFullExtent();
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.postCommand( command, runnable );
  }

  public IFeatureSelectionManager getSelectionManager( )
  {
    if( m_scrabLayerTheme != null )
      return m_selectionManager;
    return null;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return getName();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#paintInternal(org.kalypso.ogc.gml.IPaintInternalDelegate)
   */
  public void paintInternal( final IPaintInternalDelegate delegate )
  {
    // nothing to do
  }
}
