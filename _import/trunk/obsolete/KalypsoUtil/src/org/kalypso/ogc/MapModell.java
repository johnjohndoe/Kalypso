package org.kalypso.ogc;

import java.awt.Component;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.pool.KeyedObjectPool;
import org.deegree.graphics.Highlighter;
import org.deegree.graphics.MapEventController;
import org.deegree.graphics.MapView;
import org.deegree.graphics.RenderException;
import org.deegree.graphics.Selector;
import org.deegree.graphics.Theme;
import org.deegree.graphics.optimizers.Optimizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.graphics.MapFactory;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;
import org.kalypso.ogc.event.ModellEventProviderAdapter;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.xml.gisview.Gisview;
import org.kalypso.xml.gisview.GisviewType.LayersType;
import org.kalypso.xml.types.GisviewLayerType;
import org.kalypso.xml.types.GisviewLayerType.StyleType;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author bce
 */
public class MapModell implements MapView, ModellEventProvider, ModellEventListener
{
  private ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private MapView myModell;

  private final Component myComponent;

  public MapModell( final Component component, final CS_CoordinateSystem crs )
  {
    myComponent = component;
    myModell = MapFactory.createMapView( "MapView", null, crs );
  }

  public MapModell( final Gisview gisview, final CS_CoordinateSystem crs, final KeyedObjectPool layerPool, final KeyedObjectPool stylePool, final Object helper, final Component component )
  {
    myComponent = component;

    final LayersType layerListType = gisview.getLayers();
    final List layerList = layerListType.getLayer();
    myModell = MapFactory.createMapView( "MapView", null, crs );

    for( int i = 0; i < layerList.size(); i++ )
    {
      final GisviewLayerType layerType = (GisviewLayerType)layerList.get( i );

      final KalypsoFeatureLayer layer;
      try
      {
        layer = (KalypsoFeatureLayer)layerPool.borrowObject(
            new PoolableObjectType( layerType.getType(), layerType.getSource(), helper ) );
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
        continue;
      }

      final Theme theme = new KalypsoTheme( layer );
      final List stylesList = layerType.getStyle();

      final List result = new ArrayList();
      for( int is = 0; is < stylesList.size(); is++ )
      {
        final StyleType styleType = ( (StyleType)stylesList.get( is ) );
        final IPoolableObjectType styleID = new PoolableObjectType( styleType.getType(), styleType
            .getSource(), helper );
        try
        {
          final KalypsoUserStyle style = (KalypsoUserStyle)stylePool.borrowObject(
              styleID );
          result.add( style );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }

      if( result.size() == 0 )
        theme.setStyles( new KalypsoUserStyle[]
        { null } );
      else
        theme.setStyles( (KalypsoUserStyle[])result.toArray( new KalypsoUserStyle[result.size()] ) );

      try
      {
        addTheme( theme );
      }
      catch( Exception ex )
      {
        System.out.println( "could not add Theme" );
        ex.printStackTrace();
      }
    }
  }

  public void activateTheme( Theme arg0 )
  {
    myModell.activateTheme( arg0 );
    fireModellEvent( null );

  }

  public void addEventController( MapEventController arg0 )
  {
    myModell.addEventController( arg0 );
  }

  public void addHighlighter( Highlighter arg0 )
  {
    myModell.addHighlighter( arg0 );
    fireModellEvent( null );
  }

  public void addOptimizer( Optimizer arg0 )
  {
    myModell.addOptimizer( arg0 );
    fireModellEvent( null );
  }

  public void addSelector( Selector arg0 )
  {
    myModell.addSelector( arg0 );
    fireModellEvent( null );
  }

  public void addTheme( Theme arg0 ) throws Exception
  {
    myModell.addTheme( arg0 );
    fireModellEvent( null );
  }

  public void clear()
  {
    myModell.clear();
    fireModellEvent( null );
  }

  public void enableTheme( Theme arg0, boolean arg1 )
  {
    myModell.enableTheme( arg0, arg1 );
    fireModellEvent( null );
  }

  public Theme[] getAllThemes()
  {
    return myModell.getAllThemes();
  }

  public GM_Envelope getBoundingBox()
  {
    return myModell.getBoundingBox();
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return myModell.getCoordinatesSystem();
  }

  public String getName()
  {
    return myModell.getName();
  }

  public Optimizer[] getOptimizers()
  {
    return myModell.getOptimizers();
  }

  public GeoTransform getProjection()
  {
    return myModell.getProjection();
  }

  public double getScale()
  {
    return myModell.getScale();
  }

  public double getScale( Graphics arg0 )
  {
    return myModell.getScale( arg0 );
  }

  public Theme getTheme( int arg0 )
  {
    return myModell.getTheme( arg0 );
  }

  public Theme getTheme( String arg0 )
  {
    return myModell.getTheme( arg0 );
  }

  public int getThemeSize()
  {
    return myModell.getThemeSize();
  }

  public boolean isThemeActivated( Theme arg0 )
  {
    return myModell.isThemeActivated( arg0 );
  }

  public boolean isThemeEnabled( Theme arg0 )
  {
    return myModell.isThemeEnabled( arg0 );
  }

  public void moveDown( Theme arg0 )
  {
    myModell.moveDown( arg0 );
    fireModellEvent( null );
  }

  public void moveUp( Theme arg0 )
  {
    myModell.moveUp( arg0 );
    fireModellEvent( null );
  }

  public void paint( Graphics arg0 ) throws RenderException
  {
    myModell.paint( arg0 );
  }

  public void paintHighlighted( Graphics arg0 ) throws RenderException
  {
    myModell.paintHighlighted( arg0 );
  }

  public void paintSelected( Graphics arg0 ) throws RenderException
  {
    myModell.paintSelected( arg0 );
  }

  public void removeEventController( MapEventController arg0 )
  {
    myModell.removeEventController( arg0 );
  }

  public void removeHighlighter( Highlighter arg0 )
  {
    myModell.removeHighlighter( arg0 );
    fireModellEvent( null );
  }

  public void removeSelector( Selector arg0 )
  {
    myModell.removeSelector( arg0 );
    fireModellEvent( null );
  }

  public void removeTheme( int arg0 )
  {
    myModell.removeTheme( arg0 );
    fireModellEvent( null );
  }

  public void removeTheme( String arg0 )
  {
    myModell.removeTheme( arg0 );
    fireModellEvent( null );
  }

  public void removeTheme( Theme arg0 )
  {
    myModell.removeTheme( arg0 );
    fireModellEvent( null );
  }

  public void setBoundingBox( GM_Envelope env )
  {
    myModell.setBoundingBox( adjustBoundingBox( env ) );
    fireModellEvent( null );
  }

  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  public void setOptimizers( Optimizer[] arg0 )
  {
    myModell.setOptimizers( arg0 );
  }

  public void swapThemes( Theme arg0, Theme arg1 )
  {
    myModell.swapThemes( arg0, arg1 );
  }

  private double getRatio()
  {
   return ( (double)myComponent.getHeight() ) / ( (double)myComponent.getWidth() );
  }
  
  private GM_Envelope adjustBoundingBox( GM_Envelope env )
  {
    if( env == null )
      env = getFullExtentBoundingBox();
    double ratio = getRatio();
    double minX = env.getMin().getX();
    double minY = env.getMin().getY();

    double maxX = env.getMax().getX();
    double maxY = env.getMax().getY();

    double dx = ( maxX - minX ) / 2d;
    double dy = ( maxY - minY ) / 2d;

    if( dx * ratio > dy )
      dy = dx * ratio;
    else
      dx = dy / ratio;

    double mx = ( maxX + minX ) / 2d;
    double my = ( maxY + minY ) / 2d;

    return GeometryFactory.createGM_Envelope( mx - dx, my - dy, mx + dx, my + dy );
  }

  public GM_Envelope getZoomOutBoundingBox(  )
  {
      GeoTransform transform = getProjection(  );
      double ratio = getRatio();
      double gisMX = transform.getSourceX( myComponent.getWidth() / 2d );
      double gisMY = transform.getSourceY( myComponent.getHeight() / 2d);

      double gisDX = 2 * ( gisMX - transform.getSourceX( 0 ) );
      double gisDY = gisDX * ratio;
      double gisX1 = gisMX - gisDX;
      double gisX2 = gisMX + gisDX;
      double gisY1 = gisMY - gisDY;
      double gisY2 = gisMY + gisDY;

      return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  
  public GM_Envelope getFullExtentBoundingBox()
  {
    Theme[] themes = myModell.getAllThemes();
    boolean found = false;
    double resultMinX = 0;
    double resultMaxX = 0;

    double resultMinY = 0;
    double resultMaxY = 0;

    for( int i = 0; i < themes.length; i++ )
    {
      //if( myMapView.isThemeEnabled( themes[i] ) )
      {
        try
        {
          GeoTransformer gt = new GeoTransformer( myModell.getCoordinatesSystem() );
          GM_Envelope env = gt.transformEnvelope( themes[i].getLayer().getBoundingBox(), themes[i]
              .getLayer().getCoordinatesSystem() );
          double minX = env.getMin().getX();
          double minY = env.getMin().getY();

          double maxX = env.getMax().getX();
          double maxY = env.getMax().getY();

          if( !found )
          {
            resultMinX = minX;
            resultMinY = minY;
            resultMaxX = maxX;
            resultMaxY = maxY;
            found = true;
          }
          else
          {
            if( minY < resultMinX )
              resultMinX = minX;

            if( minY < resultMinY )
              resultMinY = minY;

            if( maxX > resultMaxX )
              resultMaxX = maxX;

            if( maxY > resultMaxY )
              resultMaxY = maxY;
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    }

    if( found )
    {
      return GeometryFactory.createGM_Envelope( resultMinX, resultMinY, resultMaxX, resultMaxY );
    }
    return null;
  }

  public void addModellListener( ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

  /**
   * 
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }
}