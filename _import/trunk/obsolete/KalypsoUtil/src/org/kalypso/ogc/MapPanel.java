package org.kalypso.ogc;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.widgets.WidgetManager;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * @author vdoemming
 *  
 */
public class MapPanel extends Canvas implements IMapModellView
{
  private Image highlightImage = null;

  private Image mapImage = null;

  private Image selectionImage = null;

  private int xOffset = 0;

  private int yOffset = 0;

  private int myWidth = 0;

  private int myHeight = 0;

  private boolean validHighlight = false;

  private boolean validMap = false;

  private boolean validSelection = false;

  private MapModell myModell = null;

  private final WidgetManager myWidgetManager;

  public MapPanel( final CS_CoordinateSystem crs )
  {
    super();
    // set empty Modell:
    setMapModell( new MapModell( this, crs ) );
    myWidgetManager = new WidgetManager( this );
    addMouseListener( myWidgetManager );
    addMouseMotionListener( myWidgetManager );

    setVisible( true );
  }

  public void dispose()
  {
    removeMouseListener( myWidgetManager );
    removeMouseMotionListener( myWidgetManager );
    myModell.removeModellListener( this );
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
      myModell.setBoundingBox( myModell.getBoundingBox() );
      setValidAll( false );
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
        myModell.paint( gr );
        gr.setXORMode( Color.red );
        myModell.paintSelected( gr, 10 );
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

  private void setValidSelection( boolean status )
  {
    validSelection = status;
  }

  private boolean hasValidSelection()
  {
    return validSelection;
  }

  private void setValidHighlight( boolean status )
  {
    validHighlight = status;
  }

  private boolean hasValidHighlight()
  {
    return validHighlight;
  }

  private void setValidAll( boolean status )
  {
    setValidMap( status );
    setValidSelection( status );
    setValidHighlight( status );
  }

  public WidgetManager getWidgetManager()
  {
    return myWidgetManager;
  }

  /**
   * 
   * @see org.kalypso.ogc.IMapModellView#getMapModell()
   */
  public MapModell getMapModell()
  {
    return myModell;
  }

  /**
   * 
   * @see org.kalypso.ogc.IMapModellView#setMapModell(org.kalypso.ogc.MapModell)
   */
  public void setMapModell( final MapModell modell )
  {
    if( myModell != null )
      myModell.removeModellListener( this );
    
    myModell = modell;
    
    if( myModell != null )
      myModell.addModellListener( this );
  }

  /**
   * 
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    setValidAll( false );
    clearOffset();
  }

  public GM_Envelope getPanToPixelBoundingBox( double mx, double my )
  {
    double ratio = myHeight / myWidth;

    GeoTransform transform = myModell.getProjection();

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
}