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
package org.deegree_impl.gml;

import org.deegree.gml.GMLNameSpace;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public class GMLNameSpace_Impl implements GMLNameSpace
{
  private String nameSpace = null;

  /**
   * Creates a new GMLNameSpace_Impl object.
   */
  public GMLNameSpace_Impl()
  {
    nameSpace = "";
  }

  /**
   * Creates a new GMLNameSpace_Impl object.
   * 
   * @param nameSpace
   */
  public GMLNameSpace_Impl( String nameSpace )
  {
    this.nameSpace = nameSpace;
  }

  /**
   * returns the name of the name space.
   * <p>
   * if the name space is 'xmlns:gml="http://www.opengis.net/gml"', 'xmlns' will
   * be returned
   */
  public String getNameSpaceName()
  {
    Debug.debugMethodBegin( this, "getNameSpaceName" );

    int pos = nameSpace.indexOf( ":" );

    if( pos < 0 )
    {
      pos = nameSpace.indexOf( "=" );
    }

    Debug.debugMethodEnd();

    return nameSpace.substring( 0, pos ).trim();
  }

  /**
   * returns the name of the sub space.
   * <p>
   * if the name space is 'xmlns:gml="http://www.opengis.net/gml"', 'gml' will
   * be returned
   */
  public String getSubSpaceName()
  {
    Debug.debugMethodBegin( this, "getSubSpaceName" );

    int pos1 = nameSpace.indexOf( ":" );
    int pos2 = nameSpace.indexOf( "=" );

    Debug.debugMethodEnd();

    return nameSpace.substring( pos1 + 1, pos2 ).trim();
  }

  /**
   * returns the value of the name space.
   * <p>
   * if the name space is 'xmlns:gml="http://www.opengis.net/gml"',
   * 'http://www.opengis.net/gml' will be returned
   */
  public String getNameSpaceValue()
  {
    Debug.debugMethodBegin( this, "getNameSpaceValue" );

    int pos = nameSpace.indexOf( "=" );

    Debug.debugMethodEnd();

    return nameSpace.substring( pos + 1, nameSpace.length() ).trim();
  }

  /**
   * @see org.deegree_impl.gml.GMLNameSpace_Impl#getNameSpaceValue()
   */
  public void setNameSpaceValue( String nameSpaceValue )
  {}

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return nameSpace;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:58  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:24
 * doemming backup of local modified deegree sources
 * 
 * Revision 1.4 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.3 2004/01/03 13:46:45 poth no message
 * 
 * Revision 1.2 2003/04/23 15:44:40 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:05 poth no message
 * 
 * Revision 1.4 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.3 2002/08/05 16:11:02 ap no message
 * 
 *  
 */
