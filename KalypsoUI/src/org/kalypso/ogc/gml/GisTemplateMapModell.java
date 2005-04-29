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

import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.ExtentType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * @author Belger
 */
public class GisTemplateMapModell implements IMapModell
{
  private final IMapModell m_modell;
  private final URL m_context;

  public GisTemplateMapModell( final Gismapview gisview, final URL context,
      final CS_CoordinateSystem crs )
  {
    m_context = context;
    m_modell = new MapModell( crs );
    // layer 1 is legend
    final IKalypsoTheme legendTheme = new KalypsoLegendTheme( this );
    addTheme( legendTheme );
    enableTheme( legendTheme, false );

    
    final LayersType layerListType = gisview.getLayers();
    final List layerList = layerListType.getLayer();

    final Layer activeLayer = (Layer)layerListType.getActive();

    for( int i = 0; i < layerList.size(); i++ )
    {
      final GismapviewType.LayersType.Layer layerType = (GismapviewType.LayersType.Layer)layerList
          .get( i );

      final IKalypsoTheme theme = loadTheme( layerType, context );
      if( theme != null )
      {
        addTheme( theme );
        enableTheme( theme, layerType.isVisible() );

        if( layerType == activeLayer )
          activateTheme( theme );
      }
    }
  }

  public IKalypsoTheme addTheme(org.kalypso.template.gismapview.GismapviewType.LayersType.Layer layer)
  {  
    final IKalypsoTheme theme = loadTheme( layer, m_context );
    if( theme != null )
    {
      addTheme( theme );
      enableTheme( theme, layer.isVisible() );
    }
    return theme;
  }
      
  public void dispose()
  {
    if( m_modell != null )
      m_modell.dispose();
  }

  private IKalypsoTheme loadTheme( final Layer layerType, final URL context )
  {
    if( "wms".equals( layerType.getLinktype() ) )
    {
      String layerName = "[" + layerType.getLinktype()+ "] " + layerType.getName();
      String source = layerType.getHref();
      CS_CoordinateSystem cs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      return new KalypsoWMSTheme( layerName, source, cs);
    }
    return new GisTemplateFeatureTheme( layerType, context );
  }

  // Helper
  public Gismapview createGismapTemplate( final GM_Envelope bbox ) throws JAXBException
  {
    final ObjectFactory maptemplateFactory = new ObjectFactory();
//
    final org.kalypso.template.types.ObjectFactory extentFac = new org.kalypso.template.types.ObjectFactory();
    final Gismapview gismapview = maptemplateFactory.createGismapview();
    final LayersType layersType = maptemplateFactory.createGismapviewTypeLayersType();
    if( bbox != null )
    {
      final ExtentType extentType = extentFac.createExtentType();

      extentType.setTop( bbox.getMax().getY() );
      extentType.setBottom( bbox.getMin().getY() );
      extentType.setLeft( bbox.getMin().getX() );
      extentType.setRight( bbox.getMax().getX() );

      gismapview.setExtent( extentType );
    }

    final List layerList = layersType.getLayer();
  
    gismapview.setLayers( layersType );
    
//    Gismapview gismapview = GisTemplateHelper.emptyGisView(bbox); //CK
//    final List layerList = (List)gismapview.getLayers(); //CK
    IKalypsoTheme[] themes = m_modell.getAllThemes();
    for( int i = 0; i < themes.length; i++ )
    {
      final Layer layer = maptemplateFactory.createGismapviewTypeLayersTypeLayer();
      
      final IKalypsoTheme kalypsoTheme = themes[i];
      if( kalypsoTheme instanceof GisTemplateFeatureTheme )
      {
        ( (GisTemplateFeatureTheme)kalypsoTheme ).fillLayerType( layer, "ID_" + i, m_modell
            .isThemeEnabled( kalypsoTheme ) );
        layerList.add( layer );
      }
      else if( kalypsoTheme instanceof KalypsoWMSTheme )
      {
        String name=kalypsoTheme.getName();
       GisTemplateHelper.fillLayerType(layer,"ID_"+i,name,m_modell.isThemeEnabled(kalypsoTheme),(KalypsoWMSTheme)kalypsoTheme);
       layerList.add( layer );    
      }

      if( m_modell.isThemeActivated( kalypsoTheme ) )
        layersType.setActive( layer );
    }

    try
    {
      GeoTransformer gt = new GeoTransformer(ConvenienceCSFactory.getInstance().getOGCCSByName("EPSG:4326"));
      GM_Envelope env = gt.transformEnvelope(bbox, KalypsoGisPlugin.getDefault().getCoordinatesSystem());
      System.out.println( env );
      
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return gismapview;
  }

  public void activateTheme( IKalypsoTheme theme )
  {
    m_modell.activateTheme( theme );
  }

  public void addModellListener( ModellEventListener listener )
  {
    m_modell.addModellListener( listener );
  }

  public void addTheme( final IKalypsoTheme theme )
  {
    m_modell.addTheme( theme );
  }

  public void enableTheme( IKalypsoTheme theme, boolean status )
  {
    m_modell.enableTheme( theme, status );
  }

  public void fireModellEvent( ModellEvent event )
  {
    m_modell.fireModellEvent( event );
  }

  public IKalypsoTheme getActiveTheme()
  {
    return m_modell.getActiveTheme();
  }

  public IKalypsoTheme[] getAllThemes()
  {
    return m_modell.getAllThemes();
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return m_modell.getCoordinatesSystem();
  }

  public GM_Envelope getFullExtentBoundingBox()
  {
    return m_modell.getFullExtentBoundingBox();
  }

  public IKalypsoTheme getTheme( int pos )
  {
    return m_modell.getTheme( pos );
  }

  public int getThemeSize()
  {
    return m_modell.getThemeSize();
  }

  public boolean isThemeActivated( IKalypsoTheme theme )
  {
    return m_modell.isThemeActivated( theme );
  }

  public boolean isThemeEnabled( IKalypsoTheme theme )
  {
    return m_modell.isThemeEnabled( theme );
  }

  public void moveDown( IKalypsoTheme theme )
  {
    m_modell.moveDown( theme );
  }

  public void moveUp( IKalypsoTheme theme )
  {
    m_modell.moveUp( theme );
  }

  public void paintSelected( Graphics g, GeoTransform p, GM_Envelope bbox, double scale,
      int selectionId )
  {
    m_modell.paintSelected( g, p, bbox, scale, selectionId );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    m_modell.removeModellListener( listener );
  }

  public void removeTheme( IKalypsoTheme theme )
  {
    m_modell.removeTheme( theme );
  }

  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception
  {
    m_modell.setCoordinateSystem( crs );
  }

  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 )
  {
    m_modell.swapThemes( theme1, theme2 );
  }

  public void saveTheme( final IKalypsoFeatureTheme theme, final IProgressMonitor monitor )
      throws CoreException
  {
    if( theme instanceof GisTemplateFeatureTheme )
      ( (GisTemplateFeatureTheme)theme ).saveFeatures( monitor );
    else
      throw new UnsupportedOperationException( "theme must be of type "
          + GisTemplateFeatureTheme.class.getName() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }

  public URL getContext()
  {
    return m_context;
  }
}