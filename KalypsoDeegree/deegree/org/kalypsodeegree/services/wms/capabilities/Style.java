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
package org.deegree.services.wms.capabilities;

import java.net.URL;

import org.deegree.graphics.sld.UserStyle;

/**
 * Zero or more Styles may be advertised for a Layer or collection of layers
 * using <Style>elements, each of which shall have <Name>and <Title>elements.
 * The style's Name is used in the Map request STYLES parameter. The Title is a
 * human-readable string. If only a single style is available, that style is
 * known as the "default" style and need not be advertised by the server.
 * <p>
 * </p>
 * A Style may contain several other elements in the Capabilities XML DTD. In
 * particular, <Abstract>provides a narrative description while <LegendURL>
 * contains the location of an image of a map legend appropriate to the
 * enclosing Style. A <Format>element in LegendURL indicates the MIME type of
 * the logo image, and the attributes width and height state the size of the
 * image in pixels.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public interface Style
{
  /**
   * returns the name - machine code - of the style
   */
  String getName();

  /**
   * returns the title (human readable) of the style
   */
  String getTitle();

  /**
   * returns a short narrative description of the style
   */
  String getAbstract();

  /**
   * returns an array of LegendURL objects that defines the location, size and
   * format of the legend graphics
   * 
   * @return
   */
  LegendURL[] getLegendURL();

  /**
   * returns the style URLs associated to the style
   */
  StyleSheetURL getStyleSheetURL();

  /**
   * 
   * 
   * @return
   */
  StyleURL getStyleURL();

  /**
   * returns the URL where to access the XML (SLD) document definingthe style
   * 
   * @return
   */
  URL getStyleResource();

  /**
   * returns the content of the style as SLD style object
   * 
   * @return instance of UserStyle
   */
  UserStyle getStyleContent();

}