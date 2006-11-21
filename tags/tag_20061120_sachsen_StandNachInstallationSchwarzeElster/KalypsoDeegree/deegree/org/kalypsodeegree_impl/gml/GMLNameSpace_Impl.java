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
package org.kalypsodeegree_impl.gml;

import org.kalypsodeegree.gml.GMLNameSpace;
import org.kalypsodeegree_impl.tools.Debug;

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
  private final String m_nameSpace;

  private final String m_shortName;

  /**
   * Creates a new GMLNameSpace_Impl object.
   */
  //  public GMLNameSpace_Impl()
  //  {
  //    nameSpace = "";
  //  }
  /**
   * Creates a new GMLNameSpace_Impl object.
   * 
   * @param nameSpace
   */
  public GMLNameSpace_Impl( String shortName, String nameSpace )
  {
    m_shortName = shortName;
    m_nameSpace = nameSpace;
  }

  /**
   * returns the name of the name space.
   * <p>
   * if the name space is 'xmlns:gml="http://www.opengis.net/gml"', 'xmlns' will be returned
   */
  public String getNameSpaceName()
  {

    Debug.debugMethodBegin( this, "getNameSpaceName" );
    Debug.debugMethodEnd();
    return "xmlns";
    //    int pos = nameSpace.indexOf( ":" );
    //
    //    if( pos < 0 )
    //    {
    //      pos = nameSpace.indexOf( "=" );
    //    }

    //    return nameSpace.substring( 0, pos ).trim();
  }

  /**
   * returns the name of the sub space.
   * <p>
   * if the name space is 'xmlns:gml="http://www.opengis.net/gml"', 'gml' will be returned
   * 
   * @return null, if no subspace is given
   */
  public String getSubSpaceName()
  {
    Debug.debugMethodBegin( this, "getSubSpaceName" );

    //    int pos1 = nameSpace.indexOf( ":" );
    //    int pos2 = nameSpace.indexOf( "=" );
    //
    //    if( pos1 == -1 || pos2 == -1 || pos1>pos2)
    //      return null;

    Debug.debugMethodEnd();

    //    return nameSpace.substring( pos1 + 1, pos2 ).trim();
    return m_shortName;
  }

  /**
   * returns the value of the name space.
   * <p>
   * if the name space is 'xmlns:gml="http://www.opengis.net/gml"', 'http://www.opengis.net/gml' will be returned
   */
  public String getNameSpaceValue()
  {
    Debug.debugMethodBegin( this, "getNameSpaceValue" );

    //    int pos = nameSpace.indexOf( "=" );

    Debug.debugMethodEnd();

    //    return nameSpace.substring( pos + 1, nameSpace.length() ).trim();
    return m_nameSpace;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLNameSpace_Impl#getNameSpaceValue()
   */
  public void setNameSpaceValue( String nameSpaceValue )
  {
  // TODO?
  }

  /**
   * 
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    if( m_shortName != null )
      return "xmlns:" + m_shortName + "=" + m_nameSpace;
    return "xmlns=" + m_nameSpace;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.9  2005/06/20 14:07:45  belger
 * Formatierung
 * Revision 1.8 2005/04/19 14:48:43 belger *** empty log message ***
 * 
 * Revision 1.7 2005/03/08 11:01:03 doemming *** empty log message ***
 * 
 * Revision 1.6 2005/01/18 12:50:42 doemming *** empty log message ***
 * 
 * Revision 1.5 2004/11/22 01:29:50 doemming *** empty log message *** Revision 1.4 2004/11/17 14:48:41 belger *** empty
 * log message ***
 * 
 * Revision 1.3 2004/10/07 14:09:14 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:56:58 doemming *** empty log message *** Revision 1.3 2004/08/31 13:03:30 doemming ***
 * empty log message *** Revision 1.4 2004/03/02 07:38:14 poth no message
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
