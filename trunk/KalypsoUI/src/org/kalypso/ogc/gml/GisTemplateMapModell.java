package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.ExtentType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * @author Belger
 */
public class GisTemplateMapModell implements IMapModell
{
  private final IMapModell m_modell;

  public GisTemplateMapModell( final Gismapview gisview, final URL context,
      final CS_CoordinateSystem crs )
  {
    m_modell = new MapModell( crs );

    IKalypsoTheme legenTheme = new KalypsoLegendTheme( this );
    addTheme( legenTheme );
    enableTheme( legenTheme, false );

    final LayersType layerListType = gisview.getLayers();
    final List layerList = layerListType.getLayer();

    // layer -> theme
    final Map layerMap = new HashMap();

    for( int i = 0; i < layerList.size(); i++ )
    {
      final GismapviewType.LayersType.Layer layerType = (GismapviewType.LayersType.Layer)layerList
          .get( i );

      final IKalypsoTheme theme = loadTheme( layerType, context );
      if( theme != null )
      {
        addTheme( theme );
        enableTheme( theme, layerType.isVisible() );
        layerMap.put( layerType, theme );
      }
    }

    final Layer activeLayer = (Layer)layerListType.getActive();
    if( activeLayer != null )
    {
      final IKalypsoTheme theme = (IKalypsoTheme)layerMap.get( activeLayer );
      activateTheme( theme );
    }
    else
      activateTheme( null );

  }

  private IKalypsoTheme loadTheme( final Layer layerType, final URL context )
  {
    if( "wms".equals( layerType.getLinktype() ) )
      return new KalypsoWMSTheme( layerType.getName(), layerType.getHref(), KalypsoGisPlugin.getDefault().getCoordinatesSystem() );

    return new PoolableKalypsoFeatureTheme( layerType, context );
  }

  // Helper
  public Gismapview createGismapTemplate( GM_Envelope bbox ) throws JAXBException
  {
    final org.kalypso.template.gismapview.ObjectFactory maptemplateFactory = new org.kalypso.template.gismapview.ObjectFactory();

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
    IKalypsoTheme[] themes = m_modell.getAllThemes();
    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme kalypsoTheme = themes[i];
      if( kalypsoTheme instanceof PoolableKalypsoFeatureTheme )
      {
       final Layer layer = maptemplateFactory.createGismapviewTypeLayersTypeLayer();

        ( (PoolableKalypsoFeatureTheme)kalypsoTheme ).fillLayerType( layer, Integer
            .toString( i ), m_modell.isThemeEnabled( kalypsoTheme ) );
        layerList.add( layer );

        if( m_modell.isThemeActivated( kalypsoTheme ) )
          layersType.setActive( layer );
      }
      else if( kalypsoTheme instanceof KalypsoWMSTheme )
      {
        // TODO: serialize it!
      }
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

  public void clear()
  {
    m_modell.clear();
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
}