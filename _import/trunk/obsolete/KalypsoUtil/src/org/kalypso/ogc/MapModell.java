package org.kalypso.ogc;

import java.awt.Component;
import java.awt.Graphics;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import org.deegree.graphics.RenderException;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;
import org.kalypso.ogc.event.ModellEventProviderAdapter;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class MapModell implements ModellEventProvider, ModellEventListener
{
  private final ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private final Component myComponent;

  private final static Boolean THEME_ENABLED = new Boolean( true );

  private final static Boolean THEME_DISABLED = new Boolean( false );

  private final Vector myThemes = new Vector();

  private final Map myEnabledThemeStatus = new HashMap();

  private final CS_CoordinateSystem myCoordinatesSystem;

  private final GeoTransform myProjection = new WorldToScreenTransform();

  private IKalypsoTheme myActiveTheme = null;

  private GM_Envelope myBoundingBox = null;

  private double myScale = 1;

  public MapModell( final Component component, final CS_CoordinateSystem crs )
  {
    myComponent = component;
    myCoordinatesSystem = crs;
  }

  //  public MapModell( final Gismapview gisview, final CS_CoordinateSystem crs,
  //      final KeyedObjectPool layerPool, final KeyedObjectPool stylePool, final
  // Object helper,
  //      final Component component )
  //  {
  //    myComponent = component;
  //    myEnabledThemeStatus = new HashMap();
  //    myActiveTheme = null;
  //    myCoordinatesSystem = crs;
  //    myThemes = new Vector();
  //    myProjection = new WorldToScreenTransform();
  //
  //    final LayersType layerListType = gisview.getLayers();
  //    final List layerList = layerListType.getLayer();
  //
  //    for( int i = 0; i < layerList.size(); i++ )
  //    {
  //      final GismapviewType.LayersType.Layer layerType =
  // (GismapviewType.LayersType.Layer)layerList.get( i );
  //
  //      final KalypsoFeatureLayer layer;
  //      try
  //      {
  //        layer = (KalypsoFeatureLayer)layerPool.borrowObject( new
  // PoolableObjectType( layerType
  //            .getLinktype(), layerType.getHref(), helper ) );
  //      }
  //      catch( Exception e1 )
  //      {
  //        e1.printStackTrace();
  //        continue;
  //      }
  //
  //      final KalypsoFeatureTheme theme = new KalypsoFeatureTheme( layer,
  // layerType.getName() );
  //      final List stylesList = layerType.getStyle();
  //
  //      final List result = new ArrayList();
  //      for( int is = 0; is < stylesList.size(); is++ )
  //      {
  //        final StyleType styleType = ( (StyleType)stylesList.get( is ) );
  //        final IPoolableObjectType styleID = new PoolableObjectType(
  // styleType.getLinktype(), styleType
  //            .getHref(), helper );
  //        try
  //        {
  //          final KalypsoUserStyle style = (KalypsoUserStyle)stylePool.borrowObject(
  // styleID );
  //          result.add( style );
  //        }
  //        catch( Exception e )
  //        {
  //          e.printStackTrace();
  //        }
  //      }
  //
  //      if( result.size() == 0 )
  //        theme.setStyles( new KalypsoUserStyle[]
  //        { null } );
  //      else
  //        theme.setStyles( (KalypsoUserStyle[])result.toArray( new
  // KalypsoUserStyle[result.size()] ) );
  //
  //      try
  //      {
  //        addTheme( theme );
  //      }
  //      catch( final Exception ex )
  //      {
  //        System.out.println( "could not add Theme" );
  //        ex.printStackTrace();
  //      }
  //    }
  //  }

  public void activateTheme( IKalypsoTheme theme )
  {
    myActiveTheme = theme;
    fireModellEvent( null );
  }

  public IKalypsoTheme getActiveTheme()
  {
    return myActiveTheme;
  }

  public void addTheme( IKalypsoTheme theme ) throws Exception
  {
    if( myActiveTheme == null )
      myActiveTheme = theme;
    myThemes.add( theme );
    theme.setParent( this );
    theme.getLayer().setCoordinatesSystem( myCoordinatesSystem );
    myEnabledThemeStatus.put( theme, THEME_ENABLED );
    theme.addModellListener( this );
    fireModellEvent( null );
  }

  public void clear()
  {
    myActiveTheme = null;
    IKalypsoTheme[] themes = getAllThemes();
    for( int i = 0; i < themes.length; i++ )
      removeTheme( themes[i] );
    fireModellEvent( null );
  }

  public void enableTheme( IKalypsoTheme theme, boolean status )
  {
    if( status )
      myEnabledThemeStatus.put( theme, THEME_ENABLED );
    else
      myEnabledThemeStatus.put( theme, THEME_DISABLED );
    fireModellEvent( null );
  }

  public IKalypsoTheme[] getAllThemes()
  {
    return (IKalypsoTheme[])myThemes.toArray( new IKalypsoTheme[myThemes.size()] );
  }

  public GM_Envelope getBoundingBox()
  {
    return myBoundingBox;
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return myCoordinatesSystem;
  }

  public GeoTransform getProjection()
  {
    return myProjection;
  }

  /**
   * renders the map to the passed graphic context
   * 
   * @param g
   * @throws RenderException
   *           thrown if the passed <tt>Graphic<tt> haven't
   *                         clipbounds. use g.setClip( .. );
   */
  public void paint( Graphics g ) throws RenderException
  {
    if( getThemeSize() == 0 )
      return;
    if( g.getClipBounds() == null )
    {
      throw new RenderException( "no clip bounds defined for graphic context" );
    }

    int x = g.getClipBounds().x;
    int y = g.getClipBounds().y;
    int w = g.getClipBounds().width;
    int h = g.getClipBounds().height;
    myProjection.setDestRect( x - 2, y - 2, w + x, h + y );
    myScale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
    for( int i = 0; i < getThemeSize(); i++ )
    {
      if( isThemeEnabled( getTheme( i ) ) )
      {
        getTheme( i ).paint( g );
      }
    }
  }

  public void paintSelected( Graphics g, int selectionId ) throws RenderException
  {
    if( getThemeSize() == 0 )
      return;
    if( g.getClipBounds() == null )
    {
      throw new RenderException( "no clip bounds defined for graphic context" );
    }

    int x = g.getClipBounds().x;
    int y = g.getClipBounds().y;
    int w = g.getClipBounds().width;
    int h = g.getClipBounds().height;
    myProjection.setDestRect( x - 2, y - 2, w + x, h + y );
    myScale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
    for( int i = 0; i < getThemeSize(); i++ )
    {
      if( isThemeEnabled( getTheme( i ) ) )
      {
        getTheme( i ).paintSelected( g, selectionId );
      }
    }
  }

  public double getScale()
  {
    return myScale;
  }

  public double getScale( Graphics g )
  {
    myScale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
    return myScale;
  }

  /**
   * calculates the map scale (denominator) as defined in the OGC SLD 1.0.0
   * specification
   * 
   * @return scale of the map
   */
  private double calcScale( int mapWidth, int mapHeight )
  {
    try
    {
      CS_CoordinateSystem epsg4326crs = getCoordinatesSystem();
      GM_Envelope bbox = getBoundingBox();

      if( !getCoordinatesSystem().getName().equalsIgnoreCase( "EPSG:4326" ) )
      {
        // transform the bounding box of the request to EPSG:4326
        GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
        bbox = transformer.transformEnvelope( bbox, epsg4326crs );
      }

      double dx = bbox.getWidth() / mapWidth;
      double dy = bbox.getHeight() / mapHeight;

      // create a box on the central map pixel to determine its size in meters
      GM_Position min = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
          * ( mapWidth / 2d - 1 ), bbox.getMin().getY() + dy * ( mapHeight / 2d - 1 ) );
      GM_Position max = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
          * ( mapWidth / 2d ), bbox.getMin().getY() + dy * ( mapHeight / 2d ) );
      double distance = calcDistance( min.getY(), min.getX(), max.getY(), max.getX() );

      // default pixel size defined in SLD specs is 28mm
      double scale = distance / 0.00028;

      return scale;
    }
    catch( Exception e )
    {
      Debug.debugException( e, "Exception occured when calculating scale!" );
    }
    return 0.0;
  }

  /**
   * calculates the distance in meters between two points in EPSG:4326
   * coodinates .
   */
  private double calcDistance( double lon1, double lat1, double lon2, double lat2 )
  {
    double r = 6378.137;
    double rad = Math.PI / 180d;
    double cose = 0;

    cose = Math.sin( rad * lon1 ) * Math.sin( rad * lon2 ) + Math.cos( rad * lon1 )
        * Math.cos( rad * lon2 ) * Math.cos( rad * ( lat1 - lat2 ) );
    double dist = r * Math.acos( cose );

    return dist * 1000;
  }

  public IKalypsoTheme getTheme( int pos )
  {
    return (IKalypsoTheme)myThemes.elementAt( pos );
  }

  public IKalypsoTheme getTheme( String themeName )
  {
    for( int i = 0; i < myThemes.size(); i++ )
      if( themeName.equals( ( (IKalypsoTheme)myThemes.elementAt( i ) ).getName() ) )
        return (IKalypsoTheme)myThemes.elementAt( i );
    return null;
  }

  public int getThemeSize()
  {
    return myThemes.size();
  }

  public boolean isThemeActivated( IKalypsoTheme theme )
  {
    return myActiveTheme == theme;
  }

  public boolean isThemeEnabled( IKalypsoTheme theme )
  {
    return myEnabledThemeStatus.get( theme ) == THEME_ENABLED;
  }

  public void moveDown( IKalypsoTheme theme )
  {
    int pos = myThemes.indexOf( theme );
    if( pos > 0 )
      swapThemes( theme, getTheme( pos - 1 ) );
  }

  public void moveUp( IKalypsoTheme theme )
  {
    int pos = myThemes.indexOf( theme );
    if( pos + 1 < myThemes.size() )
      swapThemes( theme, getTheme( pos + 1 ) );
  }

  public void removeTheme( int pos )
  {
    removeTheme( (IKalypsoTheme)myThemes.elementAt( pos ) );
  }

  public void removeTheme( String themeName )
  {
    removeTheme( getTheme( themeName ) );
  }

  public void removeTheme( IKalypsoTheme theme )
  {
    myThemes.remove( theme );
    myEnabledThemeStatus.remove( theme );
    if( myActiveTheme == theme )
      myActiveTheme = null;
    fireModellEvent( null );
  }

  public void setBoundingBox( GM_Envelope env )
  {
    myBoundingBox = adjustBoundingBox( env );
    myProjection.setSourceRect( myBoundingBox );
    fireModellEvent( null );
  }

  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception
  {
    if( crs.equals( myCoordinatesSystem ) )
      throw new UnsupportedOperationException();
  }

  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 )
  {
    int pos1 = myThemes.indexOf( theme1 );
    int pos2 = myThemes.indexOf( theme2 );
    myThemes.set( pos1, theme2 );
    myThemes.set( pos2, theme1 );
    fireModellEvent( null );
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

  public GM_Envelope getZoomOutBoundingBox()
  {
    GeoTransform transform = getProjection();
    double ratio = getRatio();
    double gisMX = transform.getSourceX( myComponent.getWidth() / 2d );
    double gisMY = transform.getSourceY( myComponent.getHeight() / 2d );

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
    IKalypsoTheme[] themes = getAllThemes();
    boolean found = false;
    double resultMinX = 0;
    double resultMaxX = 0;

    double resultMinY = 0;
    double resultMaxY = 0;

    for( int i = 0; i < themes.length; i++ )
    {

      if( isThemeEnabled( themes[i] ) )
      {
        try
        {
          final GeoTransformer gt = new GeoTransformer( getCoordinatesSystem() );
          final GM_Envelope boundingBox = themes[i].getLayer().getBoundingBox();

          if( boundingBox != null )
          {
            final GM_Envelope env = gt.transformEnvelope( boundingBox, themes[i].getLayer()
                .getCoordinatesSystem() );
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