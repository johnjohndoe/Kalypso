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
package org.deegree_impl.services.wms.capabilities;

import org.deegree.services.wms.capabilities.ScaleHint;
import org.deegree.xml.Marshallable;

/**
 * Layers may include a <ScaleHint>element that suggests minimum and maximum
 * scales for which it is appropriate to display this layer. Because WMS output
 * is destined for output devices of arbitrary size and resolution, the usual
 * definition of scale as the ratio of map size to real-world size is not
 * appropriate here. The following definition of Scale Hint is recommended.
 * Consider a hypothetical map with a given Bounding Box, width and height. The
 * central pixel of that map (or the pixel just to the northwest of center) will
 * have some size, which can be expressed as the ground distance in meters of
 * the southwest to northeast diagonal of that pixel. The two values in
 * ScaleHint are the minimum and maximum recommended values of that diagonal. It
 * is recognized that this definition is not geodetically precise, but at the
 * same time the hope is that by including it conventions will develop that can
 * be later specified more clearly.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
class ScaleHint_Impl implements ScaleHint, Marshallable
{
  private double max = Double.MAX_VALUE;

  private double min = 0;

  /**
   * default constructor
   */
  ScaleHint_Impl()
  {}

  /**
   * constructor initializing the class with the <ScaleHint>
   */
  ScaleHint_Impl( double min, double max )
  {
    setMin( min );
    setMax( max );
  }

  /**
   * returns the minimum scale for which a layer is defined
   */
  public double getMin()
  {
    return min;
  }

  /**
   * sets the minimum scale for which a layer is defined
   */
  public void setMin( double min )
  {
    this.min = min;
  }

  /**
   * returns the maximum scale for which a layer is defined
   */
  public double getMax()
  {
    return max;
  }

  /**
   * sets the maximum scale for which a layer is defined
   */
  public void setMax( double max )
  {
    this.max = max;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "min = " + min + "\n";
    ret += ( "max = " + max + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<ScaleHint min=\"" ).append( min ).append( "\" max=\"" ).append( max ).append(
        "\"/>" );

    return sb.toString();
  }
}