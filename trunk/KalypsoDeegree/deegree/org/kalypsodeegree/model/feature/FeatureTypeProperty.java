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

package org.deegree.model.feature;

import java.util.Map;

/**
 * 
 * the interface describes a property entry of a feature type. the name of the
 * property must be equal to the name of the corresponding feature property.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */

public interface FeatureTypeProperty
{
  
   /**
   * returns the name of the property
   */
  public String getName();

  /**
   * returns the name of the data type of the property
   */
  public String getType();

  public Annotation getAnnotation(String lang);
  /**
   * returns true if the property value is allowed to be null
   */
  public boolean isNullable();
  
  public String getNamespace();
  public boolean isGeometryProperty();

  public Map getAnnotationMap();
}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.5  2005/01/18 12:50:43  doemming
 * *** empty log message ***
 *
 * Revision 1.4  2004/10/09 18:41:20  belger
 * *** empty log message ***
 *
 * Revision 1.3  2004/10/07 14:09:24  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:12  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01 doemming
 * *** empty log message *** Revision 1.2 2004/02/09 07:57:01 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:47 poth no message
 * 
 * Revision 1.2 2002/08/15 10:02:25 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 * 
 * Revision 1.4 2002/03/04 10:20:31 ap no message
 * 
 * Revision 1.3 2001/10/23 13:41:52 ap no message
 * 
 * Revision 1.2 2001/10/15 14:48:19 ap no message
 * 
 * Revision 1.1 2001/10/05 15:19:43 ap no message
 *  
 */
