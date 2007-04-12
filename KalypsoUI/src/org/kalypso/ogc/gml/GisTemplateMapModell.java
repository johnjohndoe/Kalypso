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
import java.net.URL;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public class GisTemplateMapModell implements IMapModell, IKalypsoThemeListener
{
  private final IMapModell m_modell;

  private final URL m_context;

  private final IFeatureSelectionManager m_selectionManager;

  public GisTemplateMapModell( final URL context, final CS_CoordinateSystem crs, final IProject project, final IFeatureSelectionManager selectionManager )
  {
    m_context = context;
    m_selectionManager = selectionManager;
    m_modell = new MapModell( crs, project );
    // layer 1 is legend
    final IKalypsoTheme legendTheme = new KalypsoLegendTheme( this );
    addTheme( legendTheme );
    enableTheme( legendTheme, false );
    // layer 2 is scrablayer
    final ScrabLayerFeatureTheme scrabLayer = new ScrabLayerFeatureTheme( selectionManager, this );
    // m_modell.addModellListener( m_scrabLayer );
    addTheme( scrabLayer );
  }

  /**
   * Replaces layers based on Gismapview template. Resolves cascading themes if necessary.
   * 
   * @throws CoreException
   *           if a theme in the {@link Gismapview} cannot be loaded.
   */
  public void createFromTemplate( final Gismapview gisview ) throws Exception
  {
    for( final IKalypsoTheme theme : getAllThemes() )
    {
      if( !((theme instanceof KalypsoLegendTheme) || (theme instanceof ScrabLayerFeatureTheme)) )
      {
        removeTheme( theme );
      }
    }
    final Layers layerListType = gisview.getLayers();
    final Object activeLayer = layerListType.getActive();

    final List<StyledLayerType> layerList = layerListType.getLayer();
    for( final StyledLayerType layerType : layerList )
    {
      final IKalypsoTheme theme = addTheme( layerType );
      if( layerType == activeLayer )
      {
        activateTheme( theme );
      }
    }
  }

  public IKalypsoTheme addTheme( final StyledLayerType layer ) throws Exception
  {
    final IKalypsoTheme theme = loadTheme( layer, m_context );
    if( theme != null )
    {
      addTheme( theme );
      enableTheme( theme, layer.isVisible() );
    }
    return theme;
  }

  public void dispose( )
  {
    if( m_modell != null )
    {
      m_modell.dispose();
    }
  }

  private IKalypsoTheme loadTheme( final StyledLayerType layerType, final URL context ) throws Exception
  {
    final String[] arrImgTypes = new String[] { "tif", "jpg", "png", "gif", "gmlpic" };

    if( "wms".equals( layerType.getLinktype() ) ) //$NON-NLS-1$
    {
      final String layerName = layerType.getName();
      final String source = layerType.getHref();
      final CS_CoordinateSystem cs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      return new KalypsoWMSTheme( layerType.getLinktype(), layerName, source, cs, this );
    }
    else if( ArrayUtils.contains( arrImgTypes, layerType.getLinktype().toLowerCase() ) ) 
    {
      return KalypsoPictureTheme.getPictureTheme( layerType, context, this, KalypsoGisPlugin.getDefault().getCoordinatesSystem() );
    }
    else if( "gmt".equals( layerType.getLinktype() ) )
    {
      return new CascadingKalypsoTheme( layerType, context, m_selectionManager, this );
    }
    else
    {
      // TODO: returns handling of gml files - part of else?!? dont assume it, proofe it!
      return new GisTemplateFeatureTheme( layerType, context, m_selectionManager, this );
    }
  }

  // Helper
  public Gismapview createGismapTemplate( final GM_Envelope bbox, final String srsName )
  {
    final ObjectFactory maptemplateFactory = new ObjectFactory();
    final org.kalypso.template.types.ObjectFactory templateFactory = new org.kalypso.template.types.ObjectFactory();
    //
    final org.kalypso.template.types.ObjectFactory extentFac = new org.kalypso.template.types.ObjectFactory();
    final Gismapview gismapview = maptemplateFactory.createGismapview();
    final Layers layersType = maptemplateFactory.createGismapviewLayers();
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

    // Gismapview gismapview = GisTemplateHelper.emptyGisView(bbox); //CK
    // final List layerList = (List)gismapview.getLayers(); //CK
    final IKalypsoTheme[] themes = m_modell.getAllThemes();
    for( int i = 0; i < themes.length; i++ )
    {
      final StyledLayerType layer = templateFactory.createStyledLayerType();
      if( layer == null )
      {
        continue;
      }

      final IKalypsoTheme kalypsoTheme = themes[i];
      if( kalypsoTheme instanceof GisTemplateFeatureTheme )
      {
        ((GisTemplateFeatureTheme) kalypsoTheme).fillLayerType( layer, "ID_" + i, m_modell //$NON-NLS-1$
        .isThemeEnabled( kalypsoTheme ) );
        layerList.add( layer );
      }
      else if( kalypsoTheme instanceof KalypsoWMSTheme )
      {
        final String name = kalypsoTheme.getName();
        GisTemplateHelper.fillLayerType( layer, "ID_" + i, name, m_modell.isThemeEnabled( kalypsoTheme ), //$NON-NLS-1$
        (KalypsoWMSTheme) kalypsoTheme );
        layerList.add( layer );
      }
      else if( kalypsoTheme instanceof KalypsoPictureTheme )
      {
        ((KalypsoPictureTheme) kalypsoTheme).fillLayerType( layer, "ID_" + i, m_modell.isThemeEnabled( kalypsoTheme ) ); //$NON-NLS-1$
        layerList.add( layer );
      }
      else if( kalypsoTheme instanceof CascadingKalypsoTheme )
      {
        ((CascadingKalypsoTheme) kalypsoTheme).fillLayerType( layer, "ID_" + i, m_modell //$NON-NLS-1$
        .isThemeEnabled( kalypsoTheme ) );
        layerList.add( layer );
      }
      if( m_modell.isThemeActivated( kalypsoTheme ) && !(kalypsoTheme instanceof KalypsoLegendTheme) && !(kalypsoTheme instanceof ScrabLayerFeatureTheme) )
      {
        layersType.setActive( layer );
      }
    }
    // try
    // {
    // // is this code still used?
    // // GeoTransformer gt = new GeoTransformer( ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:4326" ) );
    // /* GM_Envelope env = */
    // // gt.transformEnvelope( bbox, KalypsoGisPlugin.getDefault().getCoordinatesSystem() );
    // // System.out.println( env );
    // }
    // catch( Exception e )
    // {
    // e.printStackTrace();
    // }

    return gismapview;
  }

  public void activateTheme( final IKalypsoTheme theme )
  {
    m_modell.activateTheme( theme );
  }

  public void addModellListener( final ModellEventListener listener )
  {
    m_modell.addModellListener( listener );
  }

  public void addTheme( final IKalypsoTheme theme )
  {
    theme.addModellListener( this );
    m_modell.addTheme( theme );
  }

  public void enableTheme( final IKalypsoTheme theme, final boolean status )
  {
    m_modell.enableTheme( theme, status );
  }

  public void fireModellEvent( final ModellEvent event )
  {
    m_modell.fireModellEvent( event );
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

  public boolean isThemeEnabled( final IKalypsoTheme theme )
  {
    return m_modell.isThemeEnabled( theme );
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

  public void removeModellListener( final ModellEventListener listener )
  {
    m_modell.removeModellListener( listener );
  }

  public void removeTheme( final IKalypsoTheme theme )
  {
    m_modell.removeTheme( theme );
    theme.dispose();
  }

  public void setCoordinateSystem( final CS_CoordinateSystem crs ) throws Exception
  {
    m_modell.setCoordinateSystem( crs );
  }

  public void swapThemes( final IKalypsoTheme theme1, final IKalypsoTheme theme2 )
  {
    m_modell.swapThemes( theme1, theme2 );
  }

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

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
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
   * @see org.kalypso.ogc.gml.IKalypsoThemeListener#kalypsoThemeChanged(org.kalypso.ogc.gml.KalypsoThemeEvent)
   */
  public void kalypsoThemeChanged( final KalypsoThemeEvent event )
  {
    // TODO this hack is for getting a repaint when a theme changes
    if( event.isType( KalypsoThemeEvent.CONTENT_CHANGED ) )
    {
      fireModellEvent( null );
    }
  }
}