// $Header:
// /cvsroot/deegree/deegree/org/deegree/ogcbasic/CommonNamespaces.java,v 1.2
// 2004/05/07 12:04:12 poth Exp $
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
package org.deegree.ogcbasic;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.0
 */
public interface CommonNamespaces
{
  public static String GMLNS = "http://www.opengis.net/gml";

  public static String WFSNS = "http://www.opengis.net/wfs";

  public static String WCSNS = "http://www.opengis.net/wcs";

  public static String OGCNS = "http://www.opengis.net/ogc";

  public static String SLDNS = "http://www.opengis.net/sld";

  public static String XLNNS = "http://www.w3.org/1999/xlink";

  public static String CNTXTNS = "http://www.opengis.net/context";

  public static String DGCNTXTNS = "http://www.deegree.org/context";
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * CommonNamespaces.java,v $ Revision 1.2 2004/05/07 12:04:12 poth no message
 * 
 * Revision 1.1 2004/04/07 11:43:55 poth no message
 * 
 * Revision 1.1 2004/04/05 07:35:53 poth no message
 * 
 * 
 *  
 ******************************************************************************/