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
package org.deegree_impl.model.resources;

// Miscellaneous
import java.awt.geom.Dimension2D;
import java.io.Serializable;

/**
 * Implement float and double version of {@link Dimension2D}. This class is
 * only temporary; it will disaspear if <em>JavaSoft</em> implements
 * <code>Dimension2D.Float</code> and <code>Dimension2D.Double</code>.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public final class XDimension2D
{
  /**
   * Do not allow instantiation of this class.
   */
  private XDimension2D()
  {}

  /**
   * Implement float version of {@link Dimension2D}. This class is temporary;
   * it will disaspear if <em>JavaSoft</em> implements
   * <code>Dimension2D.Float</code> and <code>Dimension2D.Double</code>.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  public static final class Float extends Dimension2D implements Serializable
  {
    /**
     * Largeur de la dimension.
     */
    public float width;

    /**
     * Hauteur de la dimension.
     */
    public float height;

    /**
     * Construit un objet avec les dimensions (0,0).
     */
    public Float()
    {}

    /**
     * Construit un objet avec les dimensions spécifiées.
     * 
     * @param w
     *          largeur.
     * @param h
     *          hauteur.
     */
    public Float( final float w, final float h )
    {
      width = w;
      height = h;
    }

    /**
     * Change les dimensions de cet objet.
     * 
     * @param w
     *          largeur.
     * @param h
     *          hauteur.
     */
    public void setSize( final double w, final double h )
    {
      width = (float)w;
      height = (float)h;
    }

    /**
     * Retourne la largeur.
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Retourne la hauteur.
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Retourne la dimension sous forme de chaîne de caractères. La chaîne sera
     * de la forme "<code>Dimension2D[45,76]</code>".
     */
    public String toString()
    {
      return "Dimension2D[" + width + ',' + height + ']';
    }
  }

  /**
   * Implement double version of {@link Dimension2D}. This class is temporary;
   * it will disaspear if <em>JavaSoft</em> implements
   * <code>Dimension2D.Float</code> and <code>Dimension2D.Double</code>.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  public static final class Double extends Dimension2D implements Serializable
  {
    /**
     * Largeur de la dimension.
     */
    public double width;

    /**
     * Hauteur de la dimension.
     */
    public double height;

    /**
     * Construit un objet avec les dimensions (0,0).
     */
    public Double()
    {}

    /**
     * Construit un objet avec les dimensions spécifiées.
     * 
     * @param w
     *          largeur.
     * @param h
     *          hauteur.
     */
    public Double( final double w, final double h )
    {
      width = w;
      height = h;
    }

    /**
     * Change les dimensions de cet objet.
     * 
     * @param w
     *          largeur.
     * @param h
     *          hauteur.
     */
    public void setSize( final double w, final double h )
    {
      width = w;
      height = h;
    }

    /**
     * Retourne la largeur.
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Retourne la hauteur.
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Retourne la dimension sous forme de chaîne de caractères. La chaîne sera
     * de la forme "<code>Dimension2D[45,76]</code>".
     */
    public String toString()
    {
      return "Dimension2D[" + width + ',' + height + ']';
    }
  }
}