/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.contribs.java.awt;

import java.awt.Color;
import java.awt.Composite;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.RenderingHints.Key;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImageObserver;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.Map;

/**
 * HighlightGraphics
 * 
 * @author doemming (08.06.2005)
 */
public class HighlightGraphics extends Graphics2D
{
  public Color getColor()
  {
    return toExternColor( m_graphics.getColor() );
  }

  public void setColor( Color c )
  {
    m_graphics.setColor( toInternColor( c ) );
  }

  private final Graphics2D m_graphics;

  // Andreas: was ist damit?
  // @Gernot: lass bitte mal drin, ich will das
  //          Heighlighting verbessern, muss aber gerade
  //          an anderen baustellen arbeiten
  //          (Andreas)
  // unused! but protected, so we have no yellow thingies
  protected final int m_dr;

  protected final int m_dg;

  protected final int m_db;

  protected final int m_da;

  /**
   * @param da
   */
  public HighlightGraphics( Graphics2D graphics, int dr, int dg, int db, int da )
  {
    m_dr = dr;
    m_graphics = graphics;
    m_dg = dg;
    m_db = db;
    m_da = da;
    setColor( graphics.getColor() );
  }

  private Color toInternColor( Color color )
  {
    return color.brighter();
    //    int r = ( color.getRed() + m_dr );
    //    int g = color.getGreen() + m_dg;
    //    int b = color.getBlue() + m_db;
    //    int a = color.getAlpha() + m_da;
    //
    //    return new Color( r > 255 ? 255 : r, g > 255 ? 255 : g, b > 255 ? 255 :
    // b, a > 255 ? 255 : a );
  }

  private Color toExternColor( Color color )
  {
    return color.darker();
    //    int r = ( color.getRed() - m_dr );
    //    int g = color.getGreen() - m_dg;
    //    int b = color.getBlue() - m_db;
    //    int a = color.getAlpha() - m_da;
    //    return new Color( r < 0 ? 0 : r, g < 0 ? 0 : g, b < 0 ? 0 : b, a < 0 ? 0
    // : a );
  }

  public void addRenderingHints( Map hints )
  {
    m_graphics.addRenderingHints( hints );
  }

  public void clearRect( int x, int y, int width, int height )
  {
    m_graphics.clearRect( x, y, width, height );
  }

  public void clip( Shape s )
  {
    m_graphics.clip( s );
  }

  public void clipRect( int x, int y, int width, int height )
  {
    m_graphics.clipRect( x, y, width, height );
  }

  public void copyArea( int x, int y, int width, int height, int dx, int dy )
  {
    m_graphics.copyArea( x, y, width, height, dx, dy );
  }

  public Graphics create()
  {
    return m_graphics.create();
  }

  /**
   * @see java.awt.Graphics#create(int, int, int, int)
   */
  public Graphics create( int x, int y, int width, int height )
  {
    return m_graphics.create( x, y, width, height );
  }

  public void dispose()
  {
    m_graphics.dispose();
  }

  public void draw( Shape s )
  {
    m_graphics.draw( s );
  }

  /**
   * @see java.awt.Graphics2D#draw3DRect(int, int, int, int, boolean)
   */
  public void draw3DRect( int x, int y, int width, int height, boolean raised )
  {
    m_graphics.draw3DRect( x, y, width, height, raised );
  }

  public void drawArc( int x, int y, int width, int height, int startAngle, int arcAngle )
  {
    m_graphics.drawArc( x, y, width, height, startAngle, arcAngle );
  }

  /**
   * @see java.awt.Graphics#drawBytes(byte[], int, int, int, int)
   */
  public void drawBytes( byte[] data, int offset, int length, int x, int y )
  {
    m_graphics.drawBytes( data, offset, length, x, y );
  }

  /**
   * @see java.awt.Graphics#drawChars(char[], int, int, int, int)
   */
  public void drawChars( char[] data, int offset, int length, int x, int y )
  {
    m_graphics.drawChars( data, offset, length, x, y );
  }

  public void drawGlyphVector( GlyphVector g, float x, float y )
  {
    m_graphics.drawGlyphVector( g, x, y );
  }

  public boolean drawImage( Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2,
      Color bgcolor, ImageObserver observer )
  {
    return m_graphics.drawImage( img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, bgcolor, observer );
  }

  public boolean drawImage( Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2,
      ImageObserver observer )
  {
    return m_graphics.drawImage( img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, observer );
  }

  public boolean drawImage( Image img, int x, int y, int width, int height, Color bgcolor, ImageObserver observer )
  {
    return m_graphics.drawImage( img, x, y, width, height, bgcolor, observer );
  }

  public boolean drawImage( Image img, int x, int y, int width, int height, ImageObserver observer )
  {
    return m_graphics.drawImage( img, x, y, width, height, observer );
  }

  public boolean drawImage( Image img, int x, int y, Color bgcolor, ImageObserver observer )
  {
    return m_graphics.drawImage( img, x, y, bgcolor, observer );
  }

  public boolean drawImage( Image img, int x, int y, ImageObserver observer )
  {
    return m_graphics.drawImage( img, x, y, observer );
  }

  public boolean drawImage( Image img, AffineTransform xform, ImageObserver obs )
  {
    return m_graphics.drawImage( img, xform, obs );
  }

  public void drawImage( BufferedImage img, BufferedImageOp op, int x, int y )
  {
    m_graphics.drawImage( img, op, x, y );
  }

  public void drawLine( int x1, int y1, int x2, int y2 )
  {
    m_graphics.drawLine( x1, y1, x2, y2 );
  }

  public void drawOval( int x, int y, int width, int height )
  {
    m_graphics.drawOval( x, y, width, height );
  }

  public void drawPolygon( int[] xPoints, int[] yPoints, int nPoints )
  {
    m_graphics.drawPolygon( xPoints, yPoints, nPoints );
  }

  /**
   * @see java.awt.Graphics#drawPolygon(java.awt.Polygon)
   */
  public void drawPolygon( Polygon p )
  {
    m_graphics.drawPolygon( p );
  }

  public void drawPolyline( int[] xPoints, int[] yPoints, int nPoints )
  {
    m_graphics.drawPolyline( xPoints, yPoints, nPoints );
  }

  /**
   * @see java.awt.Graphics#drawRect(int, int, int, int)
   */
  public void drawRect( int x, int y, int width, int height )
  {
    m_graphics.drawRect( x, y, width, height );
  }

  public void drawRenderableImage( RenderableImage img, AffineTransform xform )
  {
    m_graphics.drawRenderableImage( img, xform );
  }

  public void drawRenderedImage( RenderedImage img, AffineTransform xform )
  {
    m_graphics.drawRenderedImage( img, xform );
  }

  public void drawRoundRect( int x, int y, int width, int height, int arcWidth, int arcHeight )
  {
    m_graphics.drawRoundRect( x, y, width, height, arcWidth, arcHeight );
  }

  public void drawString( String s, float x, float y )
  {
    m_graphics.drawString( s, x, y );
  }

  public void drawString( String str, int x, int y )
  {
    m_graphics.drawString( str, x, y );
  }

  public void drawString( AttributedCharacterIterator iterator, float x, float y )
  {
    m_graphics.drawString( iterator, x, y );
  }

  public void drawString( AttributedCharacterIterator iterator, int x, int y )
  {
    m_graphics.drawString( iterator, x, y );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    return m_graphics.equals( obj );
  }

  public void fill( Shape s )
  {
    m_graphics.fill( s );
  }

  /**
   * @see java.awt.Graphics2D#fill3DRect(int, int, int, int, boolean)
   */
  public void fill3DRect( int x, int y, int width, int height, boolean raised )
  {
    m_graphics.fill3DRect( x, y, width, height, raised );
  }

  public void fillArc( int x, int y, int width, int height, int startAngle, int arcAngle )
  {
    m_graphics.fillArc( x, y, width, height, startAngle, arcAngle );
  }

  public void fillOval( int x, int y, int width, int height )
  {
    m_graphics.fillOval( x, y, width, height );
  }

  public void fillPolygon( int[] xPoints, int[] yPoints, int nPoints )
  {
    m_graphics.fillPolygon( xPoints, yPoints, nPoints );
  }

  /**
   * @see java.awt.Graphics#fillPolygon(java.awt.Polygon)
   */
  public void fillPolygon( Polygon p )
  {
    m_graphics.fillPolygon( p );
  }

  public void fillRect( int x, int y, int width, int height )
  {
    m_graphics.fillRect( x, y, width, height );
  }

  public void fillRoundRect( int x, int y, int width, int height, int arcWidth, int arcHeight )
  {
    m_graphics.fillRoundRect( x, y, width, height, arcWidth, arcHeight );
  }

  public Color getBackground()
  {
    return m_graphics.getBackground();
  }

  public Shape getClip()
  {
    return m_graphics.getClip();
  }

  public Rectangle getClipBounds()
  {
    return m_graphics.getClipBounds();
  }

  /**
   * @see java.awt.Graphics#getClipBounds(java.awt.Rectangle)
   */
  public Rectangle getClipBounds( Rectangle r )
  {
    return m_graphics.getClipBounds( r );
  }

  /**
   * @deprecated this is deprecated
   * @see java.awt.Graphics#getClipRect()
   */
  public Rectangle getClipRect()
  {
    return m_graphics.getClipRect();
  }

  public Composite getComposite()
  {
    return m_graphics.getComposite();
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    return m_graphics.getDeviceConfiguration();
  }

  public Font getFont()
  {
    return m_graphics.getFont();
  }

  /**
   * @see java.awt.Graphics#getFontMetrics()
   */
  public FontMetrics getFontMetrics()
  {
    return m_graphics.getFontMetrics();
  }

  public FontMetrics getFontMetrics( Font f )
  {
    return m_graphics.getFontMetrics( f );
  }

  public FontRenderContext getFontRenderContext()
  {
    return m_graphics.getFontRenderContext();
  }

  public Paint getPaint()
  {
    return m_graphics.getPaint();
  }

  public Object getRenderingHint( Key hintKey )
  {
    return m_graphics.getRenderingHint( hintKey );
  }

  public RenderingHints getRenderingHints()
  {
    return m_graphics.getRenderingHints();
  }

  public Stroke getStroke()
  {
    return m_graphics.getStroke();
  }

  public AffineTransform getTransform()
  {
    return m_graphics.getTransform();
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    return m_graphics.hashCode();
  }

  public boolean hit( Rectangle rect, Shape s, boolean onStroke )
  {
    return m_graphics.hit( rect, s, onStroke );
  }

  /**
   * @see java.awt.Graphics#hitClip(int, int, int, int)
   */
  public boolean hitClip( int x, int y, int width, int height )
  {
    return m_graphics.hitClip( x, y, width, height );
  }

  public void rotate( double theta )
  {
    m_graphics.rotate( theta );
  }

  public void rotate( double theta, double x, double y )
  {
    m_graphics.rotate( theta, x, y );
  }

  public void scale( double sx, double sy )
  {
    m_graphics.scale( sx, sy );
  }

  public void setBackground( Color color )
  {
    m_graphics.setBackground( color );
  }

  public void setClip( int x, int y, int width, int height )
  {
    m_graphics.setClip( x, y, width, height );
  }

  public void setClip( Shape clip )
  {
    m_graphics.setClip( clip );
  }

  public void setComposite( Composite comp )
  {
    m_graphics.setComposite( comp );
  }

  public void setFont( Font font )
  {
    m_graphics.setFont( font );
  }

  public void setPaint( Paint paint )
  {
    m_graphics.setPaint( paint );
  }

  public void setPaintMode()
  {
    m_graphics.setPaintMode();
  }

  public void setRenderingHint( Key hintKey, Object hintValue )
  {
    m_graphics.setRenderingHint( hintKey, hintValue );
  }

  public void setRenderingHints( Map hints )
  {
    m_graphics.setRenderingHints( hints );
  }

  public void setStroke( Stroke s )
  {
    m_graphics.setStroke( s );
  }

  public void setTransform( AffineTransform Tx )
  {
    m_graphics.setTransform( Tx );
  }

  public void setXORMode( Color c1 )
  {
    m_graphics.setXORMode( c1 );
  }

  public void shear( double shx, double shy )
  {
    m_graphics.shear( shx, shy );
  }

  /**
   * @see java.awt.Graphics#toString()
   */
  public String toString()
  {
    return m_graphics.toString();
  }

  public void transform( AffineTransform Tx )
  {
    m_graphics.transform( Tx );
  }

  public void translate( double tx, double ty )
  {
    m_graphics.translate( tx, ty );
  }

  public void translate( int x, int y )
  {
    m_graphics.translate( x, y );
  }
}
