package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.util.List;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.core.resources.IProject;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.PoolableObjectType;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * @author Belger
 */
public class GisTemplateMapModell implements IMapModell
{
  private final IMapModell m_modell;

  public GisTemplateMapModell( final Gismapview gisview, final IProject project,
      final CS_CoordinateSystem crs )
  {
    m_modell = new MapModell( crs );

    final LayersType layerListType = gisview.getLayers();
    final List layerList = layerListType.getLayer();

    for( int i = 0; i < layerList.size(); i++ )
    {
      final GismapviewType.LayersType.Layer layerType = (GismapviewType.LayersType.Layer)layerList
          .get( i );
      
      final IKalypsoTheme theme = loadTheme( layerType, project );
      
      if( theme != null )
      {
        addTheme( theme );
        enableTheme( theme, layerType.isVisible() );
        // TODO soll hier wirklich "wms" - coodiert werden, das sollte doch generischer gehen
        ILoaderFactory loaderFactory = KalypsoGisPlugin.getDefault().getLoaderFactory(KalypsoWMSLayer.class);
        try
        {
          ILoader loaderInstance = loaderFactory.getLoaderInstance(layerType.getLinktype());
          final String source = layerType.getHref();
          PoolableObjectType poolType = new PoolableObjectType( "sld", source, project );
          KalypsoWMSLayer layer = (KalypsoWMSLayer)loaderInstance.load(poolType.getSource(), project, null);
          addTheme(new KalypsoWMSTheme(layerType.getName(),layer));
        }
        catch( FactoryException e )
        {
          e.printStackTrace();
        }
        catch( LoaderException e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  private IKalypsoTheme loadTheme( final Layer layerType, final IProject project )
  {
    if( "wms".equals( layerType.getLinktype() ) )
    {
      // TODO soll hier wirklich "wms" - coodiert werden, das sollte doch generischer gehen
      final ILoaderFactory loaderFactory = KalypsoGisPlugin.getDefault().getLoaderFactory(KalypsoWMSLayer.class);
      try
      {
        final ILoader loaderInstance = loaderFactory.getLoaderInstance(layerType.getLinktype());
        final String source = layerType.getHref();
        final PoolableObjectType poolType = new PoolableObjectType( "sld", source, project );
        
        final KalypsoWMSLayer layer = (KalypsoWMSLayer)loaderInstance.load(poolType.getSource(), project, null);
        return new KalypsoWMSTheme( layerType.getName(), layer );
      }
      catch( FactoryException e )
      {
        e.printStackTrace();
      }
      catch( LoaderException e )
      {
        e.printStackTrace();
      }
    }

    return new PoolableKalypsoFeatureTheme( layerType, project );
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