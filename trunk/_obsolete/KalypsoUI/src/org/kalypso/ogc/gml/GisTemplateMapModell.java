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

import javax.xml.bind.JAXBElement;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.NotImplementedException;
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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
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

    setName( new I10nString( Messages.getString( "org.kalypso.ogc.gml.GisTemplateMapModell.0" ), null ) ); //$NON-NLS-1$
  }

  /**
   * Replaces layers based on Gismapview template. Resolves cascading themes if necessary.
   *
   * @throws CoreException
   *           if a theme in the {@link Gismapview} cannot be loaded.
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
        if( !(theme instanceof KalypsoLegendTheme) )
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

  public void createFromTemplate( final List<JAXBElement< ? extends StyledLayerType>> layerList, final Object activeLayer ) throws CoreException
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

  public IKalypsoTheme addLayer( final StyledLayerType layer ) throws CoreException
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

  private IKalypsoTheme loadTheme( final StyledLayerType layerType, final URL context ) throws CoreException
  {
    final JAXBElement<String> lg = layerType.getLegendicon();
    final String legendIcon = lg == null ? null : lg.getValue();

    final JAXBElement<Boolean> sC = layerType.getShowChildren();
    final boolean showChildren = sC == null ? true : sC.getValue().booleanValue();

    final String linktype = layerType.getLinktype();
    final ITranslator translator = getName().getTranslator();
    final I10nString layerName = new I10nString( layerType.getName(), translator );

    final IKalypsoThemeFactory themeFactory = ThemeFactoryExtension.getThemeFactory( linktype );
    if( themeFactory == null )
      throw new NotImplementedException( String.format( "Could not load theme '%', linktype unknown: %s", layerName.getValue(), linktype ) );

    final IKalypsoTheme theme = themeFactory.createTheme( layerName, layerType, context, this, m_selectionManager );
    if( theme instanceof AbstractKalypsoTheme )
    {
      ((AbstractKalypsoTheme) theme).setLegendIcon( legendIcon, context );
      ((AbstractKalypsoTheme) theme).setShowLegendChildren( showChildren );
    }

    return theme;
  }

  /**
   * Create the gismapview object from the current state of the model.
   */
  public Gismapview createGismapTemplate( final GM_Envelope bbox, final String srsName, IProgressMonitor monitor ) throws CoreException
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      final IKalypsoTheme[] themes = m_modell.getAllThemes();
      monitor.beginTask( Messages.getString( "org.kalypso.ogc.gml.GisTemplateMapModell.10" ), themes.length * 1000 + 1000 ); //$NON-NLS-1$

      final Gismapview gismapview = GisTemplateHelper.OF_GISMAPVIEW.createGismapview();
      final Layers layersType = GisTemplateHelper.OF_GISMAPVIEW.createGismapviewLayers();
      final I10nString name = getName();
      gismapview.setName( name.getKey() );
      final ITranslator i10nTranslator = name.getTranslator();
      if( i10nTranslator != null )
      {
        final Translator translator = GisTemplateHelper.OF_GISMAPVIEW.createGismapviewTranslator();
        translator.setId( i10nTranslator.getId() );
        translator.getAny().addAll( i10nTranslator.getConfiguration() );
        gismapview.setTranslator( translator );
      }

      if( bbox != null )
      {
        final ExtentType extentType = GisTemplateHelper.OF_TEMPLATE_TYPES.createExtentType();

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
        final JAXBElement< ? extends StyledLayerType> layerElement = GisTemplateHelper.configureLayer( theme, count++, bbox, srsName, new SubProgressMonitor( monitor, 1000 ) );
        if( layerElement != null )
        {
          layerList.add( layerElement );

          final StyledLayerType layer = layerElement.getValue();

          if( m_modell.isThemeActivated( theme ) && !(theme instanceof KalypsoLegendTheme) )
            layersType.setActive( layer );

          if( theme instanceof AbstractKalypsoTheme )
          {
            final AbstractKalypsoTheme kalypsoTheme = (AbstractKalypsoTheme) theme;
            final String legendIcon = kalypsoTheme.getLegendIcon();
            if( legendIcon != null )
              layer.setLegendicon( GisTemplateHelper.OF_TEMPLATE_TYPES.createStyledLayerTypeLegendicon( legendIcon ) );
          }
        }
      }

      return gismapview;
    }
    finally
    {
      monitor.done();
    }
  }

  public void saveGismapTemplate( final GM_Envelope bbox, final String srsName, IProgressMonitor monitor, final IFile file ) throws CoreException
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();

    ByteArrayInputStream bis = null;
    try
    {
      final IKalypsoTheme[] themes = m_modell.getAllThemes();
      monitor.beginTask( Messages.getString( "org.kalypso.ogc.gml.GisTemplateMapModell.10" ), themes.length * 1000 + 1000 ); //$NON-NLS-1$

      final Gismapview gismapview = createGismapTemplate( bbox, srsName, new SubProgressMonitor( monitor, 100 ) );

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
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.ogc.gml.GisTemplateMapModell.11" ) ) ); //$NON-NLS-1$
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

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getTheme(int)
   */
  @Override
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
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform p, final IProgressMonitor monitor ) throws CoreException
  {
    m_modell.paint( g, p, monitor );
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