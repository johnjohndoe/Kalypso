package org.kalypso.ogc;

import java.awt.Graphics;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;
import org.kalypso.ogc.event.ModellEventProviderAdapter;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class MapModell implements ModellEventProvider, ModellEventListener, IMapModell
{
  private final ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private final static Boolean THEME_ENABLED = new Boolean( true );

  private final static Boolean THEME_DISABLED = new Boolean( false );

  private final Vector myThemes = new Vector();

  private final Map myEnabledThemeStatus = new HashMap();

  private final CS_CoordinateSystem myCoordinatesSystem;

  private IKalypsoTheme myActiveTheme = null;

  public MapModell( final CS_CoordinateSystem crs )
  {
    myCoordinatesSystem = crs;
  }

  public void activateTheme( IKalypsoTheme theme )
  {
    myActiveTheme = theme;
    fireModellEvent( null );
  }

  public IKalypsoTheme getActiveTheme()
  {
    return myActiveTheme;
  }

  public void addTheme( final IKalypsoTheme theme )
  {
    if( myActiveTheme == null )
      myActiveTheme = theme;
    
    myThemes.add( theme );
    
    final KalypsoFeatureLayer layer = theme.getLayer();
    if( layer != null )
      layer.setCoordinatesSystem( myCoordinatesSystem );
    
    myEnabledThemeStatus.put( theme, THEME_ENABLED );
    
    theme.addModellListener( this );
    
    fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED  ) );
  }

  public void clear()
  {
    myActiveTheme = null;
    IKalypsoTheme[] themes = getAllThemes();
    for( int i = 0; i < themes.length; i++ )
      removeTheme( themes[i] );
    fireModellEvent( null );
  }

  public void enableTheme( final IKalypsoTheme theme, final boolean status )
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

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return myCoordinatesSystem;
  }

//  /**
//   * renders the map to the passed graphic context
//   * 
//   * @param g
//   * @throws RenderException
//   *           thrown if the passed <tt>Graphic<tt> haven't
//   *                         clipbounds. use g.setClip( .. );
//   */
//  public void paint( final Graphics g ) throws RenderException
//  {
//    if( getThemeSize() == 0 )
//      return;
//    if( g.getClipBounds() == null )
//    {
//      throw new RenderException( "no clip bounds defined for graphic context" );
//    }
//
//    int x = g.getClipBounds().x;
//    int y = g.getClipBounds().y;
//    int w = g.getClipBounds().width;
//    int h = g.getClipBounds().height;
//    myProjection.setDestRect( x - 2, y - 2, w + x, h + y );
//    
//    //myScale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
//    
//    final double scale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
//    final GeoTransform p = getProjection();
//    final GM_Envelope bbox = getBoundingBox();
//    
//    for( int i = 0; i < getThemeSize(); i++ )
//    {
//      if( isThemeEnabled( getTheme( i ) ) )
//        getTheme( i ).paint( g, p, scale, bbox );
//    }
//  }

  public void paintSelected( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final int selectionId )
  {
    if( getThemeSize() == 0 )
      return;

    for( int i = 0; i < getThemeSize(); i++ )
    {
      if( isThemeEnabled( getTheme( i ) ) )
        getTheme( i ).paintSelected( g, p, scale, bbox, selectionId );
    }
  }

//  public double getScale()
//  {
//    return myScale;
//  }
//
//  public double getScale( Graphics g )
//  {
//    myScale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
//    return myScale;
//  }


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
      if( isThemeEnabled( themes[i] ) && themes[i].getLayer() != null )
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
        catch( final Exception e )
        {
          // TODO: das sollte nicht sein, exception einfach weiterwerfen
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
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    // TODO: invalidate myself??
    
    fireModellEvent( modellEvent );
  }
}