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

package org.deegree.gml;

/**
 * <p>
 * As an alternative to the sequence of &lt;coords&gt;, coordinates can also be
 * conveyed by a single string. By default the coordinates in a tuple are
 * separated by commas, and successive tuples are separated by a space character
 * (#x20). While these delimiters are specified by several attributes, a user is
 * free to define a localized coordinates list that is derived by restriction
 * from gml:CoordinatesType. An instance document could then employ the xsi:type
 * attribute to substitute the localized coordinates list wherever a
 * <coordinates>element is expected; such a subtype could employ other
 * delimiters to reflect local usage.
 * <p>
 * It is expected that a specialized client application will extract and
 * validate string content, as these functions will not be performed by a
 * general XML parser. The formatting attributes will assume their default
 * values if they are not specified for a particular instance; the
 * &lt;coordinates&gt; element must conform to the XML Schema fragments.
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public interface GMLCoordinates
{

  static final char DEFAULT_DECIMAL = '.';

  static final char DEFAULT_CS = ',';

  static final char DEFAULT_TS = ' ';

  /**
   * returns the coordinates in the &lt;coordinates&gt; tag
   */
  public String getCoordinates();

  /**
   * sets the coordinates in the &lt;coordinates&gt; tag
   */
  public void setCoordinates( String coordinates );

  /**
   * return the character used as decimal seperator
   */
  public char getDecimalSeperator();

  /**
   * @see #getDecimalSeperator
   */
  public void setDecimalSeperator( char decimalSeperator );

  /**
   * return the character used as coordinate seperator
   */
  public char getCoordinateSeperator();

  /**
   * @see #getCoordinateSeperator
   */
  public void setCoordinateSeperator( char coordinateSeperator );

  /**
   * return the character used as tuple seperator
   */
  public char getTupleSeperator();

  /**
   * @see #getTupleSeperator
   */
  public void setTupleSeperator( char tupleSeperator );

}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:40  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:22 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:45 poth no message
 * 
 * Revision 1.2 2002/08/19 15:59:20 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 * 
 * Revision 1.3 2001/11/26 16:43:25 axel as
 * 
 * Revision 1.2 2001/11/23 10:41:26 axel as: CVS change-log comment added as/ap:
 * getDocument, setDocument deleted
 * 
 *  
 */
