/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Halo;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Incarnation of a sld:Halo-element. A Halo is a type of Fill that is applied
 * to the backgrounds of font glyphs. The use of halos greatly improves the
 * readability of text labels.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class Halo_Impl implements Halo, Marshallable
{
  private Fill fill = null;

  private ParameterValueType radius = null;

  private Stroke stroke = null;

  /**
   * Create a new <tt>Halo</tt> -instance.
   * <p>
   * 
   * @param radius
   *          radius to be used for the halo, use null for a rectangle styled
   *          halo
   * @param fill
   *          defines the fill style, use null for default style
   * @param stroke
   *          defines the stroke style, use null for default style
   */
  Halo_Impl( ParameterValueType radius, Fill fill, Stroke stroke )
  {
    setRadius( radius );
    setFill( fill );
    setStroke( stroke );
  }

  /**
   * A Fill allows area geometries to be filled. There are two types of fills:
   * solid-color and repeated GraphicFill. In general, if a Fill element is
   * omitted in its containing element, no fill will be rendered. The default is
   * a solid 50%-gray (color "#808080") opaque fill.
   * <p>
   * 
   * @return the underlying <tt>Fill</tt> -object or null
   */
  public Fill getFill()
  {
    return fill;
  }

  /**
   * Sets the underlying <tt>Fill</tt> -instance.
   * <p>
   * 
   * @param fill
   *          defines the fill color and pattern
   */
  public void setFill( Fill fill )
  {
    this.fill = fill;
  }

  /**
   * The Radius element gives the absolute size of a halo radius in pixels
   * encoded as a floating-point number. The radius is taken from the outside
   * edge of a font glyph to extend the area of coverage of the glyph (and the
   * inside edge of holes in the glyphs). The halo of a text label is considered
   * to be a single shape. The default radius is one pixel. Negative values are
   * not allowed.
   * <p>
   * 
   * @return the radius definition as <tt>ParameterValueType</tt>, or null if
   *         it has not been specified
   */
  public ParameterValueType getRadius()
  {
    return radius;
  }

  /**
   * Sets the value for the radius of the halo.
   * <p>
   * 
   * @param radius
   *          radius to be used for the halo, use null for a rectangle styled
   *          halo
   */
  public void setRadius( ParameterValueType radius )
  {
    this.radius = radius;
  }

  /**
   * The Radius element gives the absolute size of a halo radius in pixels
   * encoded as a floating-point number. The radius is taken from the outside
   * edge of a font glyph to extend the area of coverage of the glyph (and the
   * inside edge of holes in the glyphs). The halo of a text label is considered
   * to be a single shape. The default radius is one pixel. Negative values are
   * not allowed.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the radius value, or -1 if it has not been specified
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  public double getRadius( Feature feature ) throws FilterEvaluationException
  {
    if( radius == null )
    {
      return -1.0;
    }

    String stringValue = null;
    double radiusVal;

    try
    {
      stringValue = radius.evaluate( feature );
      radiusVal = Double.parseDouble( stringValue );
    }
    catch( NumberFormatException e )
    {
      throw new FilterEvaluationException( "Given value ('" + stringValue
          + "') for radius of Halo does not denote a number." );
    }

    return radiusVal;
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.sld.Halo_Impl#getRadius(Feature)
   *      <p>
   * @param radius
   *          radius to be set for the halo
   */
  public void setRadius( double radius )
  {
    ParameterValueType pvt = null;
    if( radius > 0 )
    {
      pvt = StyleFactory.createParameterValueType( "" + radius );
      this.radius = pvt;
    }
  }

  /**
   * Returns the underlying <tt>Stroke</tt> -instance.
   * <p>
   * 
   * @return the underlying <tt>Stroke</tt> -object or null
   */
  public Stroke getStroke()
  {
    return stroke;
  }

  /**
   * Sets the underlying <tt>Stroke</tt> -instance.
   * <p>
   * 
   * @param stroke
   *          defines the stroke color and pattern
   */
  public void setStroke( Stroke stroke )
  {
    this.stroke = stroke;
  }

  /**
   * exports the content of the Halo as XML formated String
   * 
   * @return xml representation of the Halo
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<Halo>" );
    if( radius != null )
    {
      sb.append( "<Radius>" );
      sb.append( ( (Marshallable)radius ).exportAsXML() );
      sb.append( "</Radius>" );
    }
    if( fill != null )
    {
      sb.append( ( (Marshallable)fill ).exportAsXML() );
    }
    if( stroke != null )
    {
      sb.append( ( (Marshallable)stroke ).exportAsXML() );
    }
    sb.append( "</Halo>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}