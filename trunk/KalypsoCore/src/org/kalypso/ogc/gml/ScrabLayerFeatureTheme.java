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
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.core.i18n.Messages;
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
  public static final String STYLE_NAME = "ScrabLayerStyle"; //$NON-NLS-1$

  public static final String POLYGON_GEOM_PROP_NAME = "geomPolygon"; //$NON-NLS-1$

  public static final String LINESTRING_GEOM_PROP_NAME = "geomLineString"; //$NON-NLS-1$

  public static final String POINT_GEOM_PROP_NAME = "geomPoint"; //$NON-NLS-1$

  public static final String FEATURE_MEMBER = "scrabLayerMember"; //$NON-NLS-1$

  private static final String CONTEXT = ScrabLayerFeatureTheme.class.getName();

  private final IKalypsoFeatureTheme m_scrabLayerTheme;

  private final IFeatureSelectionManager m_selectionManager;

  /**
   * Holds the descriptor for the default icon of this theme. Is used in legends, such as the outline.
   */
  private org.eclipse.swt.graphics.Image m_scrapThemeIcon;

  public ScrabLayerFeatureTheme( final I10nString layerName, final IFeatureSelectionManager selectionManager, final IMapModell mapModel, final String legendIcon, final URL context, final boolean shouldShowChildren )
  {
    super( layerName, "scrab", mapModel, legendIcon, context, shouldShowChildren ); //$NON-NLS-1$

    m_selectionManager = selectionManager;
    m_scrapThemeIcon = null;

    final URL scrabLayerURL = getClass().getResource( "/org/kalypso/core/resources/basicScrabLayer.gml" ); //$NON-NLS-1$
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
    m_scrabLayerTheme = new KalypsoFeatureTheme( workspace, ScrabLayerFeatureTheme.FEATURE_MEMBER, new I10nString( Messages.getString("org.kalypso.ogc.gml.ScrabLayerFeatureTheme.7") ), selectionManager, mapModel, legendIcon, context, shouldShowChildren ); //$NON-NLS-1$
    // add styles for visualisation
    final ArrayList<Symbolizer> symbolizers = new ArrayList<Symbolizer>();
    symbolizers.add( StyleFactory.createPointSymbolizer( StyleFactory.createGraphic( null, null, 1, 5, 0 ), new PropertyName( ScrabLayerFeatureTheme.POINT_GEOM_PROP_NAME ) ) );
    symbolizers.add( StyleFactory.createLineSymbolizer( StyleFactory.createStroke(), new PropertyName( ScrabLayerFeatureTheme.LINESTRING_GEOM_PROP_NAME ) ) );
    symbolizers.add( StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill( Color.GRAY, 0.5d ), new PropertyName( ScrabLayerFeatureTheme.POLYGON_GEOM_PROP_NAME ) ) );
    // Added/commented by Dejan //symbolizers.add( StyleFactory.createRasterSymbolizer() );
    final FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( ScrabLayerFeatureTheme.STYLE_NAME, symbolizers.toArray( new Symbolizer[symbolizers.size()] ) );
    final UserStyle style = (UserStyle_Impl) StyleFactory.createStyle( ScrabLayerFeatureTheme.STYLE_NAME, ScrabLayerFeatureTheme.STYLE_NAME, "empty Abstract", featureTypeStyle ); //$NON-NLS-1$
    m_scrabLayerTheme.addStyle( new KalypsoUserStyle( style, style.getName(), false ) );
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
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeaturePath()
   */
  public String getFeaturePath( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getFeaturePath();
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

    if( m_scrapThemeIcon != null )
      m_scrapThemeIcon.dispose();

    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.kalypsodeegree.model.geometry.GM_Envelope,
   *      double, java.lang.Boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final Boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.paint( g, p, bbox, scale, selected, monitor );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getName()
   */
  @Override
  public I10nString getName( )
  {
    if( m_scrabLayerTheme != null )
      return m_scrabLayerTheme.getName();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setName(org.kalypso.contribs.java.lang.I10nString)
   */
  @Override
  public void setName( final I10nString name )
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.setName( name );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getContext()
   */
  @Override
  public String getTypeContext( )
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
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#paint(double, org.kalypsodeegree.model.geometry.GM_Envelope,
   *      java.lang.Boolean, org.eclipse.core.runtime.IProgressMonitor, org.kalypso.ogc.gml.IPaintDelegate)
   */
  @Override
  public void paint( final double scale, final GM_Envelope bbox, final Boolean selected, final IProgressMonitor monitor, final IPaintDelegate delegate ) throws CoreException
  {
    if( m_scrabLayerTheme != null )
      m_scrabLayerTheme.paint( scale, bbox, selected, monitor, delegate );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getDefaultIcon()
   */
  @Override
  protected ImageDescriptor getDefaultIcon( )
  {
    if( m_scrapThemeIcon == null )
      m_scrapThemeIcon = new org.eclipse.swt.graphics.Image( Display.getCurrent(), getClass().getResourceAsStream( "resources/scrapTheme.gif" ) ); //$NON-NLS-1$

    return ImageDescriptor.createFromImage( m_scrapThemeIcon );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getLabel()
   */
  @Override
  public String getLabel( )
  {
    if( m_scrabLayerTheme == null )
      return null;

    return m_scrabLayerTheme.getLabel();
  }
}