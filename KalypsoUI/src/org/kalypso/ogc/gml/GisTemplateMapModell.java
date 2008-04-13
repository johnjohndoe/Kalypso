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
import java.net.URL;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBElement;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.commons.i18n.ITranslator;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.map.themes.KalypsoScaleTheme;
import org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypso.ogc.gml.wms.utils.KalypsoWMSUtilities;
import org.kalypso.template.gismapview.CascadingLayer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.gismapview.Gismapview.Translator;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Gernot Belger
 */
public class GisTemplateMapModell implements IMapModell, IKalypsoLayerModell
{
  private final IFeatureSelectionManager m_selectionManager;

  private final IMapModell m_modell;

  private final URL m_context;

  private boolean m_isLoaded = true;

  public GisTemplateMapModell( final URL context, final String crs, final IProject project, final IFeatureSelectionManager selectionManager )
  {
    m_context = context;
    m_selectionManager = selectionManager;
    m_modell = new MapModell( crs, project );

    setName( new I10nString( "name not defined", null ) );
  }

  /**
   * Replaces layers based on Gismapview template. Resolves cascading themes if necessary.
   * 
   * @throws CoreException
   *             if a theme in the {@link Gismapview} cannot be loaded.
   */
  public void createFromTemplate( final Gismapview gisview ) throws Exception
  {
    m_isLoaded = false;

    try
    {
      final ITranslator translator = createTranslator( gisview );
      final I10nString name = new I10nString( gisview.getName(), translator );
      setName( name );

      for( final IKalypsoTheme theme : getAllThemes() )
        if( !(theme instanceof KalypsoLegendTheme || theme instanceof ScrabLayerFeatureTheme) )
          removeTheme( theme );
      final Layers layerListType = gisview.getLayers();

      if( layerListType != null )
      {
        final Object activeLayer = layerListType.getActive();
        final List<JAXBElement< ? extends StyledLayerType>> layerList = layerListType.getLayer();

        createFromTemplate( layerList, activeLayer );
      }
    }
    finally
    {
      m_isLoaded = true;
    }
  }

  private ITranslator createTranslator( final Gismapview gisview )
  {
    final Translator translatorElement = gisview.getTranslator();
    if( translatorElement == null )
      return null;

    final ITranslator translator = KalypsoCommonsExtensions.createTranslator( translatorElement.getId() );
    if( translator != null )
      translator.configure( m_context, translatorElement.getAny() );
    return translator;
  }

  public void createFromTemplate( final List<JAXBElement< ? extends StyledLayerType>> layerList, final Object activeLayer ) throws Exception
  {
    for( final JAXBElement< ? extends StyledLayerType> layerType : layerList )
    {
      final IKalypsoTheme theme = addLayer( layerType.getValue() );
      if( layerType.getValue() == activeLayer )
        activateTheme( theme );
    }
  }

  public void setName( final I10nString name )
  {
    m_modell.setName( name );
  }

  public IKalypsoTheme addLayer( final StyledLayerType layer ) throws Exception
  {
    final IKalypsoTheme theme = loadTheme( layer, m_context );
    if( theme != null )
    {
      theme.setVisible( layer.isVisible() );
      addTheme( theme );
    }
    return theme;
  }

  public IKalypsoTheme insertLayer( final StyledLayerType layer, final int position ) throws Exception
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

    final JAXBElement<String> lg = layerType.getLegendicon();
    String legendIcon = null;
    if( lg != null )
      legendIcon = lg.getValue();

    final JAXBElement<Boolean> sC = layerType.getShowChildren();
    boolean showChildren = true;
    if( sC != null )
      showChildren = sC.getValue().booleanValue();

    final String linktype = layerType.getLinktype();
    final ITranslator translator = getName().getTranslator();
    final I10nString layerName = new I10nString( layerType.getName(), translator );

    if( layerType instanceof CascadingLayer )
      return new CascadingLayerKalypsoTheme( layerName, (CascadingLayer) layerType, context, m_selectionManager, this, legendIcon, showChildren );

    final IKalypsoThemeFactory themeFactory = ThemeFactoryExtension.getThemeFactory( linktype );
    if( themeFactory != null )
      return themeFactory.createTheme( linktype, layerName, layerType, context, this, m_selectionManager );

    if( "wms".equals( linktype ) ) //$NON-NLS-1$
    {
      final String source = layerType.getHref();

      /* Parse the source into properties. */
      final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );

      /* Get the provider attribute. */
      final String layers = sourceProps.getProperty( IKalypsoImageProvider.KEY_LAYERS, null );
      final String styles = sourceProps.getProperty( IKalypsoImageProvider.KEY_STYLES, null );
      final String service = sourceProps.getProperty( IKalypsoImageProvider.KEY_URL, null );
      final String providerID = sourceProps.getProperty( IKalypsoImageProvider.KEY_PROVIDER, null );

      /* Create the image provider. */
      final IKalypsoImageProvider imageProvider = KalypsoWMSUtilities.getImageProvider( layerName.getValue(), layers, styles, service, providerID );

      return new KalypsoWMSTheme( source, linktype, layerName, imageProvider, this, legendIcon, context, showChildren );
    }

    if( ArrayUtils.contains( arrImgTypes, linktype.toLowerCase() ) )
      return KalypsoPictureTheme.getPictureTheme( layerName, layerType, context, this, legendIcon, showChildren );

    if( "gmt".equals( linktype ) )
      return new CascadingKalypsoTheme( layerName, layerType, context, m_selectionManager, this, legendIcon, showChildren );

    if( "legend".equals( linktype ) )
      return new KalypsoLegendTheme( layerName, this, legendIcon, context, showChildren );

    if( "scrab".equals( linktype ) )
      return new ScrabLayerFeatureTheme( layerName, m_selectionManager, this, legendIcon, context, showChildren );

    if( "scale".equals( linktype ) )
      return new KalypsoScaleTheme( layerName, linktype, this, legendIcon, context, showChildren );

    // TODO: returns handling of gml files - part of else?!? do not assume it, proof it!
    return new GisTemplateFeatureTheme( layerName, layerType, context, m_selectionManager, this, legendIcon, showChildren );
  }

  // Helper
  public void createGismapTemplate( final GM_Envelope bbox, final String srsName, IProgressMonitor monitor, final IFile file ) throws CoreException
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();
    ByteArrayInputStream bis = null;
    try
    {
      final IKalypsoTheme[] themes = m_modell.getAllThemes();
      monitor.beginTask( "Kartenvorlage speichern", themes.length * 1000 + 1000 );

      final org.kalypso.template.gismapview.ObjectFactory maptemplateFactory = new org.kalypso.template.gismapview.ObjectFactory();

      final org.kalypso.template.types.ObjectFactory extentFac = new org.kalypso.template.types.ObjectFactory();
      final Gismapview gismapview = maptemplateFactory.createGismapview();
      final Layers layersType = maptemplateFactory.createGismapviewLayers();
      final I10nString name = getName();
      gismapview.setName( name.getKey() );
      final ITranslator i10nTranslator = name.getTranslator();
      if( i10nTranslator != null )
      {
        final Translator translator = maptemplateFactory.createGismapviewTranslator();
        translator.setId( i10nTranslator.getId() );
        translator.getAny().addAll( i10nTranslator.getConfiguration() );
        gismapview.setTranslator( translator );
      }

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

      final List<JAXBElement< ? extends StyledLayerType>> layerList = layersType.getLayer();

      gismapview.setLayers( layersType );

      monitor.worked( 100 );

      int count = 0;

      for( final IKalypsoTheme theme : themes )
      {
        final StyledLayerType layer = GisTemplateHelper.addLayer( layerList, theme, count++, bbox, srsName, monitor );

        if( layer != null )
        {
          if( m_modell.isThemeActivated( theme ) && !(theme instanceof KalypsoLegendTheme) && !(theme instanceof ScrabLayerFeatureTheme) )
            layersType.setActive( layer );

          if( theme instanceof AbstractKalypsoTheme )
          {
            final AbstractKalypsoTheme kalypsoTheme = (AbstractKalypsoTheme) theme;
            final String legendIcon = kalypsoTheme.getLegendIcon();
            if( legendIcon != null )
              layer.setLegendicon( extentFac.createStyledLayerTypeLegendicon( legendIcon ) );
          }
        }
      }

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      GisTemplateHelper.saveGisMapView( gismapview, bos, file.getCharset() );
      bos.close();
      bis = new ByteArrayInputStream( bos.toByteArray() );
      if( file.exists() )
        file.setContents( bis, false, true, new SubProgressMonitor( monitor, 900 ) );
      else
        file.create( bis, false, new SubProgressMonitor( monitor, 900 ) );

      bis.close();
    }
    catch( final Throwable e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "XML-Vorlagendatei konnte nicht erstellt werden." ) );
    }
    finally
    {
      monitor.done();

      IOUtils.closeQuietly( bis );
    }
  }

  public void activateTheme( final IKalypsoTheme theme )
  {
    m_modell.activateTheme( theme );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#internalActivate(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void internalActivate( final IKalypsoTheme theme )
  {
    m_modell.internalActivate( theme );
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

  public String getCoordinatesSystem( )
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
  public void accept( final IKalypsoThemeVisitor visitor, final int depth )
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
  public I10nString getName( )
  {
    return m_modell.getName();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#accept(org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor, int,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void accept( final IKalypsoThemeVisitor visitor, final int depth_infinite, final IKalypsoTheme theme )
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

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#isLoaded()
   */
  public boolean isLoaded( )
  {
    return m_isLoaded;
  }
}