/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

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

Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de

Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de

                 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.graphics.sld;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.*;

import org.deegree.xml.Marshallable;
import org.deegree.graphics.sld.*;
import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.tools.Debug;


/**
 * A Mark takes a "shape" and applies coloring to it. The shape can be derived
 * either from a well-known name (such as "square"), an external URL in various
 * formats (such as, perhaps GIF), or from a glyph of a font. Multiple external
 * formats may be used with the semantic that they all contain the equivalent
 * shape in different formats. If an image format is used that has inherent
 * coloring, the coloring is discarded and only the opacity channel (or
 * equivalent) is used. A Halo, Fill, and/or Stroke is applied as appropriate
 * for the shape's source format.
 * <p>
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public class Mark_Impl implements Mark, Marshallable {
    private BufferedImage image = null;
    private Fill fill = null;
    private String wellKnownName = null;
    private Stroke stroke = null;

    /**
     * Constructor for the default <tt>Mark</tt>.
     */
    Mark_Impl() {
    }

    /**
     * constructor initializing the class with the <Mark>
     */
    Mark_Impl( String wellKnownName, Stroke stroke, Fill fill ) {
        setWellKnownName( wellKnownName );
        setStroke( stroke );
        setFill( fill );
    }

    /**
     * Gives the well known name of a Mark's shape. Allowed values include at
     * least "square", "circle", "triangle", "star", "cross", and "x", though map
     * servers may draw a different symbol instead if they don't have a shape for
     * all of these. Renderings of these marks may be made solid or hollow
     * depending on Fill and Stroke parameters. The default value is "square".
     * @return the WK-Name of the mark
     */
    public String getWellKnownName() {
        return wellKnownName;
    }

    /**
     * Sets the well known name of a Mark's shape. Allowed values include at
     * least "square", "circle", "triangle", "star", "cross", and "x", though map
     * servers may draw a different symbol instead if they don't have a shape for
     * all of these. Renderings of these marks may be made solid or hollow
     * depending on Fill and Stroke parameters. The default value is "square".
     * @param wellKnownName the WK-Name of the mark
     */
    public void setWellKnownName( String wellKnownName ) {
        this.wellKnownName = wellKnownName;
    }

    /**
     * A Fill allows area geometries to be filled. There are two types of fills:
     * solid-color and repeated GraphicFill. In general, if a Fill element is
     * omitted in its containing element, no fill will be rendered. The default
     * is a solid 50%-gray (color "#808080") opaque fill.
     * @return the fill of the mark
     */
    public Fill getFill() {
        return fill;
    }

    /**
     * sets the <Fill>
     * @param fill the fill of the mark
     */
    public void setFill( Fill fill ) {
        this.fill = fill;
    }

    /**
     * A Stroke allows a string of line segments (or any linear geometry) to be
     * rendered. There are three basic types of strokes: solid Color, GraphicFill
     * (stipple), and repeated GraphicStroke. A repeated graphic is plotted
     * linearly and has its graphic symbol bended around the curves of the line
     * string. The default is a solid black line (Color "#000000").
     * @return the stroke of the mark
     */
    public Stroke getStroke() {
        return stroke;
    }

    /**
     * sets <Stroke>
     * @param stroke the stroke of the mark
     */
    public void setStroke( Stroke stroke ) {
        this.stroke = stroke;
    }

    /**
     * DOCUMENT ME!
     *
     * @param size DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public BufferedImage getAsImage( Feature feature, int size ) throws FilterEvaluationException {
        double fillOpacity = 1.0;
        double strokeOpacity = 1.0;
        Color fillColor = new Color( 128, 128, 128 );
        Color strokeColor = new Color( 0, 0, 0 );

        if ( fill != null ) {
            fillOpacity = fill.getOpacity( feature );
            fillColor = fill.getFill( feature );
        }

        if ( stroke != null ) {
            strokeOpacity = stroke.getOpacity( feature );
            strokeColor = stroke.getStroke( feature );
        }

        if ( wellKnownName == null ) {
            wellKnownName = "square";
        }

        if ( wellKnownName.equalsIgnoreCase( "circle" ) ) {
            image = drawCircle( size, fillOpacity, fillColor, strokeOpacity, strokeColor );
        } else if ( wellKnownName.equalsIgnoreCase( "triangle" ) ) {
            image = drawTriangle( size, fillOpacity, fillColor, strokeOpacity, strokeColor );
        } else if ( wellKnownName.equalsIgnoreCase( "cross" ) ) {
            image = drawCross1( size, strokeOpacity, strokeColor );
        } else if ( wellKnownName.equalsIgnoreCase( "x" ) ) {
            image = drawCross2( size, strokeOpacity, strokeColor );
        } else {
            image = drawSquare( size, fillOpacity, fillColor, strokeOpacity, strokeColor );
        }

        return image;
    }
    
   /**
    * Sets the mark as an image. Rhis method is not part of the sld
    * specifications but it is added to speed up applications.
    * @param bufferedImage the bufferedImage to be set for the mark
    */     
    public void setAsImage (BufferedImage bufferedImage)  {
        this.image = bufferedImage;
    }

    /**
     * Draws a scaled instance of a triangle mark according to the given
     * parameters.
     *
     * @param size resulting image's height and widthh
     * @param fillOpacity opacity value for the filled parts of the image
     * @param fillColor <tt>Color</tt>to be used for the fill
     * @param strokeOpacity opacity value for the stroked parts of the image
     * @param strokeColor <tt>Color</tt>to be used for the strokes
     *
     * @return image displaying a triangle
     */
    public BufferedImage drawTriangle( int size, double fillOpacity, Color fillColor, 
                                       double strokeOpacity, Color strokeColor ) {
        BufferedImage image = new BufferedImage( size, size, BufferedImage.TYPE_INT_ARGB );

        int[] x_ = new int[3];
        int[] y_ = new int[3];
        x_[0] = 0;
        y_[0] = 0;
        x_[1] = size / 2; 
        y_[1] = size - 1;
        x_[2] = size - 1;
        y_[2] = 0;

        Graphics2D g2D = (Graphics2D)image.getGraphics();
        setColor( g2D, fillColor, fillOpacity );
        g2D.fillPolygon( x_, y_, 3 );
        setColor( g2D, strokeColor, strokeOpacity );
        g2D.drawPolygon( x_, y_, 3 );

        return image;
    }

    /**
     * Draws a scaled instance of a circle mark according to the given
     * parameters.
     *
     * @param size resulting image's height and widthh
     * @param fillOpacity opacity value for the filled parts of the image
     * @param fillColor <tt>Color</tt>to be used for the fill
     * @param strokeOpacity opacity value for the stroked parts of the image
     * @param strokeColor <tt>Color</tt>to be used for the strokes
     *
     * @return image displaying a circle
     */
    public BufferedImage drawCircle( int size, double fillOpacity, Color fillColor, 
                                     double strokeOpacity, Color strokeColor ) {
        BufferedImage image = new BufferedImage( size, size, BufferedImage.TYPE_INT_ARGB );

        Graphics2D g2D = (Graphics2D)image.getGraphics();
        setColor( g2D, fillColor, fillOpacity );
        g2D.fillOval( 0, 0, size, size );

        setColor( g2D, strokeColor, strokeOpacity );
        g2D.drawOval( 0, 0, size, size );

        return image;
    }

    /**
     * Draws a scaled instance of a square mark according to the given
     * parameters.
     *
     * @param size resulting image's height and widthh
     * @param fillOpacity opacity value for the filled parts of the image
     * @param fillColor <tt>Color</tt>to be used for the fill
     * @param strokeOpacity opacity value for the stroked parts of the image
     * @param strokeColor <tt>Color</tt>to be used for the strokes
     *
     * @return image displaying a square
     */
    public BufferedImage drawSquare( int size, double fillOpacity, Color fillColor, 
                                     double strokeOpacity, Color strokeColor ) {
        BufferedImage image = new BufferedImage( size, size, BufferedImage.TYPE_INT_ARGB );

        Graphics2D g2D = (Graphics2D)image.getGraphics();
        setColor( g2D, fillColor, fillOpacity );
        g2D.fillRect( 0, 0, size, size );

        setColor( g2D, strokeColor, strokeOpacity );
        // TODO!
        // this is all unclear! should be 0, 0, size, size
        // there is still an unknown bug.
        g2D.drawRect( 0, 0, size-1, size-1 );

        return image;
    }

    /**
     * Draws a scaled instance of a cross mark (a "+") according to the given
     * parameters.
     *
     * @param size resulting image's height and widthh
     * @param strokeOpacity opacity value for the stroked parts of the image
     * @param strokeColor <tt>Color</tt>to be used for the strokes
     *
     * @return image displaying a cross (a "+")
     */
    public BufferedImage drawCross1( int size, double strokeOpacity, Color strokeColor ) {
        BufferedImage image = new BufferedImage( size, size, BufferedImage.TYPE_INT_ARGB );

        Graphics2D g2D = (Graphics2D)image.getGraphics();

        setColor( g2D, strokeColor, strokeOpacity );
        g2D.drawLine( 0, size / 2, size - 1, size / 2 );
        g2D.drawLine( size / 2, 0, size / 2, size - 1 );

        return image;
    }

    /**
     * Draws a scaled instance of a cross mark (an "X") according to the given
     * parameters.
     *
     * @param size resulting image's height and widthh
     * @param strokeOpacity opacity value for the stroked parts of the image
     * @param strokeColor <tt>Color</tt>to be used for the strokes
     *
     * @return image displaying a cross (a "X")
     */
    public BufferedImage drawCross2( int size, double strokeOpacity, Color strokeColor ) {
        BufferedImage image = new BufferedImage( size, size, BufferedImage.TYPE_INT_ARGB );

        Graphics2D g2D = (Graphics2D)image.getGraphics();

        setColor( g2D, strokeColor, strokeOpacity );
        g2D.drawLine( 0, 0, size - 1, size - 1 );
        g2D.drawLine( 0, size - 1, size - 1, 0 );

        return image;
    }

    /**
     *
     *
     * @param g2D 
     * @param color 
     * @param opacity 
     */
    private void setColor( Graphics2D g2D, Color color, double opacity ) {
        if ( opacity < 0.999 ) {
            final int alpha = (int)Math.round( opacity * 255 );
            final int red = color.getRed();
            final int green = color.getGreen();
            final int blue = color.getBlue();
            color = new Color( red, green, blue, alpha );
        }

        g2D.setColor( color );
    }
    
    /**
     * exports the content of the Mark as XML formated String
     *
     * @return xml representation of the Mark
     */
    public String exportAsXML() {
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer(1000);
        sb.append( "<Mark>" );
        if ( wellKnownName != null&& !wellKnownName.equals("") ) {
            sb.append( "<WellKnownName>" ).append( wellKnownName );
            sb.append( "</WellKnownName>" );
        }
        if ( fill != null ) {
            sb.append( ((Marshallable)fill).exportAsXML() );
        }
        if ( stroke != null ) {
            sb.append( ((Marshallable)stroke).exportAsXML() );
        }
        
        sb.append( "</Mark>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }
    
    //    private void drawUnicode(Graphics2D g2, int x, int y, double rotation,
    //    double size, String m, Mark mark) {
    //        int sz = (int)size;
    //        double fo = mark.getFill().getOpacity();
    //        double so = mark.getStroke().getOpacity();
    //        
    //        java.awt.Font font = new java.awt.Font("sans serif", java.awt.Font.PLAIN, sz);
    //        g2.setFont( font );
    //        FontMetrics fm = g2.getFontMetrics();
    //        
    //        char c = (char)m.charAt(0);
    //        int w = fm.charWidth(c);
    //        int h = fm.getHeight();
    //        
    //        g2 = setColor( g2, mark.getFill().getFill(), fo );
    //        g2.fillRect( x-w/2, y-h/2, w, h);
    //        g2 = setColor( g2, mark.getStroke().getStroke(), so );
    //        
    //        String s = "" + c;
    //        g2.drawString( s, x-w/2, y+h/2-fm.getDescent());
    //    }
    // else {
    //            
    //            Mark[] marks = sym.getGraphic().getMarks();
    //            double rotation = sym.getGraphic().getRotation();
    //            double size = sym.getGraphic().getSize();
    //            if (marks != null) {
    //                
    //                for (int k = 0; k > marks.length; k++) {
    //                    
    //                    float w = (float)marks[k].getStroke().getWidth();
    //                    g2.setStroke( new BasicStroke( w ) );
    //                    
    //                    if (marks[k].getWellKnownName().equalsIgnoreCase("triangle") ) {
    //                        drawTriangle( g2, x, y, rotation, size, marks[k] );
    //                    }
    //                    else
    //                        if (marks[k].getWellKnownName().equalsIgnoreCase("circle") ) {
    //                            drawCircle( g2, x, y, rotation, size, marks[k] );
    //                        }
    //                        else
    //                            if (marks[k].getWellKnownName().equalsIgnoreCase("square") ) {
    //                                drawSquare( g2, x, y, rotation, size, marks[k] );
    //                            }
    //                            else
    //                                if (marks[k].getWellKnownName().equalsIgnoreCase("cross") ) {
    //                                    drawCross1( g2, x, y, rotation, size, marks[k] );
    //                                }
    //                                else
    //                                    if (marks[k].getWellKnownName().equalsIgnoreCase("x") ) {
    //                                        drawCross2( g2, x, y, rotation, size, marks[k] );
    //                                    }
    //                                    else
    //                                        if (marks[k].getWellKnownName().length() == 0 ) {
    //                                            drawSquare( g2, x, y, rotation, size, marks[k] );
    //                                        }
    //                                        else {
    //                                            drawUnicode( g2, x, y, rotation, size,
    //                                            marks[k].getWellKnownName(), marks[k] );
    //                                        }
    //                }
    //            }
    //        }    
}