package org.kalypso.ogc.gml.map;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

import org.deegree.graphics.RenderException;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GM_Envelope_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ogc.gml.widgets.WidgetManager;
import org.kalypso.util.command.ICommandTarget;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * @author vdoemming
 *  
 */
public class MapPanel extends Canvas implements IMapModellView, ComponentListener
{
  public final static String WIDGET_ZOOM_IN = "ZOOM_IN";
  public final static String WIDGET_ZOOM_IN_RECT = "ZOOM_IN_RECT";
  public final static String WIDGET_PAN = "PAN";
  public final static String WIDGET_EDIT_FEATURE = "EDIT_FEATURE";
  public final static String WIDGET_SELECT = "SELECT";
  public final static String WIDGET_UNSELECT = "UNSELECT";
  public final static String WIDGET_TOGGLE_SELECT = "TOGGLE_SELECT";
  public final static String WIDGET_CREATE_FEATURE = "CREATE_FEATURE";
  
  public static final String WIDGET_SINGLE_SELECT = "SINGLE_SELECT";

  /** WIDGET_... -> widget */
  private Map m_widgets = new HashMap();
  
  /** widget -> WIDGET_... */
  private Map m_widgetIDs = new HashMap();

  private Image mapImage = null;

  private int xOffset = 0;

  private int yOffset = 0;

  private int myWidth = 0;

  private int myHeight = 0;

  private boolean validMap = false;

  private IMapModell myModell = null;

  private final WidgetManager myWidgetManager;

  private final GeoTransform m_projection = new WorldToScreenTransform();

  private GM_Envelope myBoundingBox = new GM_Envelope_Impl();

  private final int m_selectionID;
  private GM_Envelope m_wishBBox;

  public MapPanel( final ICommandTarget viewCommandTarget, final CS_CoordinateSystem crs, final int selectionID )
  {
    m_selectionID = selectionID;
    
    // set empty Modell:
    setMapModell( new MapModell( crs ) );
    myWidgetManager = new WidgetManager( viewCommandTarget, this );
    addMouseListener( myWidgetManager );
    addMouseMotionListener( myWidgetManager );
    addComponentListener(this);
    setVisible( true );
  }

  public void dispose()
  {
    removeMouseListener( myWidgetManager );
    removeMouseMotionListener( myWidgetManager );
    myModell.removeModellListener( this );
  }
  
  public void setWidget( final String id, final IWidget widget )
  {
    m_widgets.put( id, widget );
    m_widgetIDs.put( widget, id );
  }
  
  public void setOffset( int dx, int dy ) // used by pan method
  {
    xOffset = dx;
    yOffset = dy;

    repaint();
  }

  public void clearOffset() // used by pan method
  {
    xOffset = 0;
    yOffset = 0;

    repaint();
  }

  /**
   * @see java.awt.Component#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    paintMap( g );

    paintWidget( g );
  }

  private void paintMap( Graphics g )
  {
    //    TODO paintWMS(g); //
    if( myModell == null || myModell.getThemeSize() == 0 ) // no maps ...
    {
      g.setColor( Color.white );
      g.fillRect( 0, 0, getWidth(), getHeight() );
      g.setColor( Color.black );
      g.drawString( "Kalypso", getWidth() / 2, getHeight() / 2 );
      return;
    }

    if( getHeight() == 0 || getWidth() == 0 )
      return;

    if( getHeight() != myHeight || getWidth() != myWidth )
    { // update dimension
      myHeight = getHeight();
      myWidth = getWidth();
      //setBoundingBox( getBoundingBox() );
      //setValidAll( false );
    }

    if( !hasValidMap() || mapImage == null )
    {
      mapImage = new BufferedImage( getWidth(), getHeight(), BufferedImage.TYPE_INT_ARGB );

      final Graphics gr = mapImage.getGraphics();
      gr.setColor( Color.white );
      gr.fillRect( 0, 0, getWidth(), getHeight() );
      setValidMap( true );
      gr.setColor( Color.black );
      gr.setClip( 0, 0, getWidth(), getHeight() );

      try
      {
        if( g.getClipBounds() == null )
        {
          throw new RenderException( "no clip bounds defined for graphic context" );
        }

        int x = g.getClipBounds().x;
        int y = g.getClipBounds().y;
        int w = g.getClipBounds().width;
        int h = g.getClipBounds().height;
        m_projection.setDestRect( x - 2, y - 2, w + x, h + y );

        final GeoTransform p = getProjection();
        final GM_Envelope bbox = getBoundingBox();

        final double scale = calcScale( g.getClipBounds().width, g.getClipBounds().height );

        myModell.paintSelected( gr, p, bbox, scale, -1 );
        gr.setXORMode( Color.red );
        myModell.paintSelected( gr, p, bbox, scale, m_selectionID );
        gr.setPaintMode();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      gr.dispose();
    }
    // paint selection ?
    /*
     * if( !hasValidSelection() ) { selectionImage = new BufferedImage(
     * getWidth(), getHeight(), BufferedImage.TYPE_INT_ARGB ); Graphics gr =
     * selectionImage.getGraphics(); gr.setClip( 0, 0, getWidth(), getHeight() );
     * try { setValidSelection( true ); //myMapView.paintSelected( gr ); }
     * catch( Exception e ) { e.printStackTrace(); } gr.dispose(); } // paint
     * highlights ? if( !hasValidHighlight() ) { highlightImage = new
     * BufferedImage( getWidth(), getHeight(), BufferedImage.TYPE_INT_ARGB );
     * Graphics gr = highlightImage.getGraphics(); gr.setClip( 0, 0, getWidth(),
     * getHeight() ); try { setValidHighlight( true );
     * //myMapView.paintHighlighted( gr ); } catch( Exception e ) {
     * e.printStackTrace(); } gr.dispose(); }
     */
    if( xOffset != 0 && yOffset != 0 ) // to clear backround ...
    {
      final int left = Math.max( 0, xOffset );
      final int right = Math.min( getWidth(), xOffset + getWidth() );
      final int top = Math.max( 0, yOffset );
      final int bottom = Math.min( getHeight(), yOffset + getHeight() );

      g.setColor( getBackground() );
      //g.setColor( Color.black );

      g.fillRect( 0, 0, left, getHeight() ); // left
      g.fillRect( left, 0, right - left, top ); // top
      g.fillRect( left, bottom, right - left, getHeight() - bottom ); // bottom
      g.fillRect( right, 0, getWidth() - right, getHeight() ); // right
    }

    // draw map:
    g.drawImage( mapImage, xOffset, yOffset, null );
    // draw selection:
    //g.setXORMode( Color.red );
    //    g.drawImage( selectionImage, xOffset, yOffset, null );
    // draw highlights:
    //g.setXORMode( Color.green );
    //    g.drawImage( highlightImage, xOffset, yOffset, null );
    g.setPaintMode();
  }

  public void update( Graphics g )
  {
    paint( g );
  }

  private void paintWidget( Graphics g )
  {
    //    Image widgetImage = new BufferedImage( getWidth(), getHeight(),
    // BufferedImage.TYPE_INT_ARGB );
    //    Graphics gr = widgetImage.getGraphics();
    g.setColor( Color.red );
    g.setClip( 0, 0, getWidth(), getHeight() );

    myWidgetManager.paintWidget( g );

    //    g.drawImage( widgetImage, 0, 0, null );
    //    gr.dispose();
  }

  public void setValidMap( boolean status )
  {
    validMap = status;
  }

  private boolean hasValidMap()
  {
    return validMap;
  }

//  private void setValidSelection( boolean status )
//  {
//    validSelection = status;
//  }

//  private boolean hasValidSelection()
//  {
//    return validSelection;
//  }

//  private void setValidHighlight( boolean status )
//  {
//    validHighlight = status;
//  }

//  private boolean hasValidHighlight()
//  {
//    return validHighlight;
//  }

  private void setValidAll( boolean status )
  {
    setValidMap( status );
//    setValidSelection( status );
//    setValidHighlight( status );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public IMapModell getMapModell()
  {
    return myModell;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    if( myModell != null )
      myModell.removeModellListener( this );

    myModell = modell;

    if( myModell != null )
      myModell.addModellListener( this );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {  
    setValidAll( false );
    clearOffset();
  }

  public GM_Envelope getPanToPixelBoundingBox( double mx, double my )
  {
    double ratio = myHeight / myWidth;

    final GeoTransform transform = getProjection();

    double gisMX = transform.getSourceX( mx );
    double gisMY = transform.getSourceY( my );

    double gisDX = ( transform.getSourceX( myWidth / 2 ) - transform.getSourceX( 0 ) );
    double gisDY = gisDX * ratio;
    double gisX1 = gisMX - gisDX;
    double gisX2 = gisMX + gisDX;
    double gisY1 = gisMY - gisDY;
    double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }
  
  /**
   * calculates the current map scale (denominator) as defined in the OGC SLD 1.0.0
   * specification
   * 
   * @return scale of the map
   */  
  public double getCurrentScale()
  {
    return calcScale(getWidth(), getHeight());
  }

  /**
   * calculates the map scale (denominator) as defined in the OGC SLD 1.0.0
   * specification
   * 
   * @return scale of the map
   */
  private double calcScale( final int mapWidth, final int mapHeight )
  {
    try
    {
      final CS_CoordinateSystem epsg4326crs = myModell.getCoordinatesSystem();
      
      GM_Envelope bbox = getBoundingBox();
      if( !myModell.getCoordinatesSystem().getName().equalsIgnoreCase( "EPSG:4326" ) )
      {
        // transform the bounding box of the request to EPSG:4326
        final GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
        bbox = transformer.transformEnvelope( bbox, epsg4326crs );
      }
      
      if( bbox == null )
        return 0.0;

      final double dx = bbox.getWidth() / mapWidth;
      final double dy = bbox.getHeight() / mapHeight;

      // create a box on the central map pixel to determine its size in meters
      final GM_Position min = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
          * ( mapWidth / 2d - 1 ), bbox.getMin().getY() + dy * ( mapHeight / 2d - 1 ) );
      final GM_Position max = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
          * ( mapWidth / 2d ), bbox.getMin().getY() + dy * ( mapHeight / 2d ) );
      final double distance = calcDistance( min.getY(), min.getX(), max.getY(), max.getX() );

      // default pixel size defined in SLD specs is 28mm
      final double scale = distance / 0.00028;      
      return scale;
    }
    catch( final Exception e )
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

  public GeoTransform getProjection()
  {
    return m_projection;
  }

  public GM_Envelope getBoundingBox()
  {
    return myBoundingBox;
  }

  public void setBoundingBox( GM_Envelope wishBBox )
  {
    m_wishBBox = wishBBox;
    myBoundingBox = adjustBoundingBox( m_wishBBox );
    m_projection.setSourceRect( myBoundingBox );

    // redraw
    onModellChange( null );
    //fireModellEvent( null );
  }

  private GM_Envelope adjustBoundingBox( GM_Envelope env )
  {
    if( env == null )
      env = myModell.getFullExtentBoundingBox();
    if( env == null )
      return null;
    
    double ratio = getRatio();
    // TODO besser loesen
    if(Double.isNaN(ratio))
      return env; 
      
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

  private double getRatio()
  {
    return ( (double)getHeight() ) / ( (double)getWidth() );
  }

  public GM_Envelope getZoomOutBoundingBox()
  {
    GeoTransform transform = getProjection();
    double ratio = getRatio();
    double gisMX = transform.getSourceX( getWidth() / 2d );
    double gisMY = transform.getSourceY( getHeight() / 2d );

    double gisDX = 2 * ( gisMX - transform.getSourceX( 0 ) );
    double gisDY = gisDX * ratio;
    double gisX1 = gisMX - gisDX;
    double gisX2 = gisMX + gisDX;
    double gisY1 = gisMY - gisDY;
    double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  public void changeWidget( final String widgetID )
  {
    if( myWidgetManager != null )
      myWidgetManager.changeWidget( (IWidget)m_widgets.get( widgetID ) );
  }

  public String getActualWidgetID()
  {
    if( myWidgetManager != null )
    {
      final IWidget actualWidget = myWidgetManager.getActualWidget();
      return (String)m_widgetIDs.get( actualWidget );
    }

    return null;
  }

  public int getSelectionID()
  {
    return m_selectionID;
  }

  /**
   * @see java.awt.event.ComponentListener#componentHidden(java.awt.event.ComponentEvent)
   */
  public void componentHidden( ComponentEvent e )
  {
  //  
  }

  /**
   * @see java.awt.event.ComponentListener#componentMoved(java.awt.event.ComponentEvent)
   */
  public void componentMoved( ComponentEvent e )
  {
  //  
  }

  /**
   * @see java.awt.event.ComponentListener#componentResized(java.awt.event.ComponentEvent)
   */
  public void componentResized( ComponentEvent e )
  {
    if( m_wishBBox != null )
      setBoundingBox( m_wishBBox );
    else
    setBoundingBox(getBoundingBox());
  }

  /**
   * @see java.awt.event.ComponentListener#componentShown(java.awt.event.ComponentEvent)
   */
  public void componentShown( ComponentEvent e )
  {
    if( m_wishBBox != null )
      setBoundingBox( m_wishBBox );
    else
    setBoundingBox(getBoundingBox());
  }
}