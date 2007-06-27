/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class GisTemplateMapModell implements IMapModell
{
  private final IMapModell m_modell;

  private final URL m_context;

  private final IFeatureSelectionManager m_selectionManager;

  /**
   * Special constructor for use only by the {@link CascadingKalypsoTheme}. Sets a special model as parent to use for
   * creation of themes.
   * <p>
   * This is needed in order to return the correct parent for the cascaded themes, so the outline view behaves
   * correctly.
   */
  public GisTemplateMapModell( final URL context, final CS_CoordinateSystem crs, final IProject project, final IFeatureSelectionManager selectionManager )
  {
    m_context = context;
    m_selectionManager = selectionManager;
    m_modell = new MapModell( crs, project );

    // layer 1 is legend
    final IKalypsoTheme legendTheme = new KalypsoLegendTheme( this );
    addTheme( legendTheme );
    legendTheme.setVisible( false );
    // layer 2 is scrablayer
    final ScrabLayerFeatureTheme scrabLayer = new ScrabLayerFeatureTheme( selectionManager, this );
    addTheme( scrabLayer );
  }

  /**
   * Replaces layers based on Gismapview template. Resolves cascading themes if necessary.
   * 
   * @throws CoreException
   *             if a theme in the {@link Gismapview} cannot be loaded.
   */
  public void createFromTemplate( final Gismapview gisview ) throws Exception
  {
    setName( gisview.getName() );

    for( final IKalypsoTheme theme : getAllThemes() )
      if( !((theme instanceof KalypsoLegendTheme) || (theme instanceof ScrabLayerFeatureTheme)) )
        removeTheme( theme );
    final Layers layerListType = gisview.getLayers();
    final Object activeLayer = layerListType.getActive();

    final List<StyledLayerType> layerList = layerListType.getLayer();
    for( final StyledLayerType layerType : layerList )
    {
      final IKalypsoTheme theme = addTheme( layerType );
      if( layerType == activeLayer )
        activateTheme( theme );
    }
  }

  public void setName( final String name )
  {
    m_modell.setName( name );
  }

  public IKalypsoTheme addTheme( final StyledLayerType layer ) throws Exception
  {
    final IKalypsoTheme theme = loadTheme( layer, m_context );
    if( theme != null )
    {
      addTheme( theme );
      theme.setVisible( layer.isVisible() );
    }
    return theme;
  }

  public IKalypsoTheme insertTheme( final StyledLayerType layer, final int position ) throws Exception
  {
    final IKalypsoTheme theme = loadTheme( layer, m_context );
    if( theme != null )
    {
      insertTheme( theme, position );
      theme.setVisible( layer.isVisible() );
    }
    return theme;
  }

  public void dispose( )
  {
    m_modell.dispose();
  }

  private IKalypsoTheme loadTheme( final StyledLayerType layerType, final URL context ) throws Exception
  {
    final String[] arrImgTypes = new String[] { "tif", "jpg", "png", "gif", "gmlpic" };

    final CS_CoordinateSystem defaultCS = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    if( "wms".equals( layerType.getLinktype() ) ) //$NON-NLS-1$
    {
      final String layerName = layerType.getName();
      final String source = layerType.getHref();
      return new KalypsoWMSTheme( layerType.getLinktype(), layerName, source, defaultCS, this );
    }
    else if( ArrayUtils.contains( arrImgTypes, layerType.getLinktype().toLowerCase() ) )
      return KalypsoPictureTheme.getPictureTheme( layerType, context, this, defaultCS );
    else if( "gmt".equals( layerType.getLinktype() ) )
      return new CascadingKalypsoTheme( layerType, context, m_selectionManager, this );
    else
      // TODO: returns handling of gml files - part of else?!? dont assume it, proof it!
      return new GisTemplateFeatureTheme( layerType, context, m_selectionManager, this );
  }

  // Helper
  public void createGismapTemplate( final GM_Envelope bbox, final String srsName, final String customName, IProgressMonitor monitor, final IFile file ) throws CoreException
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();
    ByteArrayInputStream bis = null;
    try
    {
      final IKalypsoTheme[] themes = m_modell.getAllThemes();
      monitor.beginTask( "Kartenvorlage speichern", themes.length * 1000 + 1000 );

      final org.kalypso.template.gismapview.ObjectFactory maptemplateFactory = new org.kalypso.template.gismapview.ObjectFactory();
      final org.kalypso.template.types.ObjectFactory templateFactory = new org.kalypso.template.types.ObjectFactory();

      final org.kalypso.template.types.ObjectFactory extentFac = new org.kalypso.template.types.ObjectFactory();
      final Gismapview gismapview = maptemplateFactory.createGismapview();
      final Layers layersType = maptemplateFactory.createGismapviewLayers();
      if( customName != null )
        gismapview.setName( customName );
      if( bbox != null )
      {
        final ExtentType extentType = extentFac.createExtentType();

        extentType.setTop( bbox.getMax().getY() );
        extentType.setBottom( bbox.getMin().getY() );
        extentType.setLeft( bbox.getMin().getX() );
        extentType.setRight( bbox.getMax().getX() );
        extentType.setSrs( srsName );
        gismapview.setExtent( extentType );
      }

      final List<StyledLayerType> layerList = layersType.getLayer();

      gismapview.setLayers( layersType );

      monitor.worked( 100 );
      // Gismapview gismapview = GisTemplateHelper.emptyGisView(bbox); //CK
      // final List layerList = (List)gismapview.getLayers(); //CK
      for( int i = 0; i < themes.length; i++ )
      {
        final StyledLayerType layer = templateFactory.createStyledLayerType();
        if( layer == null )
          continue;

        final IKalypsoTheme kalypsoTheme = themes[i];
        if( kalypsoTheme instanceof GisTemplateFeatureTheme )
        {
          ((GisTemplateFeatureTheme) kalypsoTheme).fillLayerType( layer, "ID_" + i, kalypsoTheme.isVisible() );//$NON-NLS-1$
          layerList.add( layer );
          monitor.worked( 1000 );
        }
        else if( kalypsoTheme instanceof KalypsoWMSTheme )
        {
          final String name = kalypsoTheme.getName();
          GisTemplateHelper.fillLayerType( layer, "ID_" + i, name, kalypsoTheme.isVisible(), //$NON-NLS-1$
          (KalypsoWMSTheme) kalypsoTheme );
          layerList.add( layer );
          monitor.worked( 1000 );
        }
        else if( kalypsoTheme instanceof KalypsoPictureTheme )
        {
          ((KalypsoPictureTheme) kalypsoTheme).fillLayerType( layer, "ID_" + i, kalypsoTheme.isVisible() ); //$NON-NLS-1$
          layerList.add( layer );
          monitor.worked( 1000 );
        }
        else if( kalypsoTheme instanceof CascadingKalypsoTheme )
        {
          final CascadingKalypsoTheme cascadingKalypsoTheme = ((CascadingKalypsoTheme) kalypsoTheme);
          cascadingKalypsoTheme.fillLayerType( layer, "ID_" + i, kalypsoTheme.isVisible() ); //$NON-NLS-1$

          layerList.add( layer );

          cascadingKalypsoTheme.createGismapTemplate( bbox, srsName, new SubProgressMonitor( monitor, 1000 ) );
        }

        if( m_modell.isThemeActivated( kalypsoTheme ) && !(kalypsoTheme instanceof KalypsoLegendTheme) && !(kalypsoTheme instanceof ScrabLayerFeatureTheme) )
          layersType.setActive( layer );
      }

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      GisTemplateHelper.saveGisMapView( gismapview, bos, file.getCharset() );
      bos.close();
      bis = new ByteArrayInputStream( bos.toByteArray() );
      if( file.exists() )
        file.setContents( bis, false, true, new SubProgressMonitor( monitor, 900 ) );
      else
        file.create( bis, false, new SubProgressMonitor( monitor, 900 ) );
    }
    catch( final Throwable e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "XML-Vorlagendatei konnte nicht erstellt werden." ) );
    }
    finally
    {
      monitor.done();

      if( bis != null )
        try
        {
          bis.close();
        }
        catch( final IOException e1 )
        {
          // never occurs with a byteinputstream
          e1.printStackTrace();
        }
    }
  }

  public void activateTheme( final IKalypsoTheme theme )
  {
    m_modell.activateTheme( theme );
  }

  public void addTheme( final IKalypsoTheme theme )
  {
    m_modell.addTheme( theme );
  }

  public IKalypsoTheme getActiveTheme( )
  {
    return m_modell.getActiveTheme();
  }

  public IKalypsoTheme[] getAllThemes( )
  {
    return m_modell.getAllThemes();
  }

  public CS_CoordinateSystem getCoordinatesSystem( )
  {
    return m_modell.getCoordinatesSystem();
  }

  public GM_Envelope getFullExtentBoundingBox( )
  {
    return m_modell.getFullExtentBoundingBox();
  }

  public IKalypsoTheme getTheme( final int pos )
  {
    return m_modell.getTheme( pos );
  }

  public int getThemeSize( )
  {
    return m_modell.getThemeSize();
  }

  public boolean isThemeActivated( final IKalypsoTheme theme )
  {
    return m_modell.isThemeActivated( theme );
  }

  public void moveDown( final IKalypsoTheme theme )
  {
    m_modell.moveDown( theme );
  }

  public void moveUp( final IKalypsoTheme theme )
  {
    m_modell.moveUp( theme );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.kalypsodeegree.model.geometry.GM_Envelope,
   *      double, boolean)
   */
  public void paint( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final boolean selected )
  {
    m_modell.paint( g, p, bbox, scale, selected );
  }

  public void removeTheme( final IKalypsoTheme theme )
  {
    m_modell.removeTheme( theme );
    theme.dispose();
  }

  public void swapThemes( final IKalypsoTheme theme1, final IKalypsoTheme theme2 )
  {
    m_modell.swapThemes( theme1, theme2 );
  }

  // TODO: remove this from the interface, better: support theme specific actions
  public void saveTheme( final ITemplateTheme theme, final IProgressMonitor monitor ) throws CoreException
  {
    theme.saveFeatures( monitor );
    // if( theme instanceof GisTemplateFeatureTheme )
    // ((GisTemplateFeatureTheme) theme).saveFeatures( monitor );
    // TODO save WMS and Picture Theme
    // else if( theme instanceof KalypsoWMSTheme )
    // ( (KalypsoWMSTheme)theme ).saveTheme( monitor );
    // else if (theme instanceof KalypsoPictureTheme )
    // ((KalypsoPictureTheme)theme).saveTheme(monitor);
    // else
    // throw new UnsupportedOperationException( "theme must be of type " + GisTemplateFeatureTheme.class.getName() );
    // //$NON-NLS-1$
  }

  public URL getContext( )
  {
    return m_context;
  }

  public IMapModell getModell( )
  {
    return m_modell;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getProject()
   */
  public IProject getProject( )
  {
    return m_modell.getProject();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getScrabLayer()
   */
  public IKalypsoFeatureTheme getScrabLayer( )
  {
    return m_modell.getScrabLayer();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#accept(org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor, int)
   */
  public void accept( final KalypsoThemeVisitor visitor, final int depth )
  {
    m_modell.accept( visitor, depth );

  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#insertTheme(org.kalypso.ogc.gml.IKalypsoTheme, int)
   */
  public void insertTheme( final IKalypsoTheme theme, final int position )
  {
    m_modell.insertTheme( theme, position );

  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getName()
   */
  public String getName( )
  {
    return m_modell.getName();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#accept(org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor, int,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void accept( final KalypsoThemeVisitor visitor, final int depth_infinite, final IKalypsoTheme theme )
  {
    m_modell.accept( visitor, depth_infinite, theme );

  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object o )
  {
    return m_modell.getChildren( o );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    return m_modell.getImageDescriptor( object );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( final Object o )
  {
    return m_modell.getLabel( o );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  public Object getParent( final Object o )
  {
    return m_modell.getParent( o );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#addMapModelListener(org.kalypso.ogc.gml.mapmodel.IMapModellListener)
   */
  public void addMapModelListener( final IMapModellListener l )
  {
    m_modell.addMapModelListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#removeMapModelListener(org.kalypso.ogc.gml.mapmodel.IMapModellListener)
   */
  public void removeMapModelListener( final IMapModellListener l )
  {
    m_modell.removeMapModelListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#invalidate(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void invalidate( final GM_Envelope bbox )
  {
    m_modell.invalidate( bbox );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getThemeParent(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public Object getThemeParent( final IKalypsoTheme theme )
  {
    return this;
  }
}