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

package org.deegree_impl.model.feature;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.deegree.model.feature.Annotation;
import org.deegree.model.feature.FeatureTypeProperty;

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
public class FeatureTypeProperty_Impl implements FeatureTypeProperty, Serializable
{
  private final Map m_annotationMap;

  private final String m_namespace;

  private final String m_name;

  private final String m_type;

  private final boolean m_nullable;

  /**
   * initializes a FeatureTypeProperty with its name its associated type and a
   * boolean variable that says if the propetry maybe <tt>null</tt>
   */
  protected FeatureTypeProperty_Impl( String name, String namespace, String type, boolean nullable,
      Map annotationMap )
  {
    m_annotationMap = annotationMap == null ? new HashMap() : annotationMap;
    m_name = name;
    m_namespace = namespace;
    m_type = type;
    m_nullable = nullable;

    final String localKey = Locale.getDefault().getLanguage();
    if( !m_annotationMap.containsKey( localKey ) )
      m_annotationMap.put( localKey, new Annotation( localKey, m_name, "", m_namespace + ":"
          + m_name ) );
  }

  /**
   * returns the name of the property
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * returns the name of the data type of the property
   */
  public String getType()
  {
    return m_type;
  }

  /**
   * returns true if the property value is allowed to be null
   */
  public boolean isNullable()
  {
    return m_nullable;
  }

  public String toString()
  {
    String ret = null;
    ret = "name = " + m_name + "\n";
    ret += "type = " + m_type + "\n";
    ret += "nullable = " + m_nullable + "\n";
    return ret;
  }

  public String getNamespace()
  {
    return m_namespace;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#isGeometryProperty()
   */
  public boolean isGeometryProperty()
  {
    return m_type.startsWith( "org.deegree.model.geometry." ) && !m_type.endsWith( "Envelope" );
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getAnnotation(java.lang.String)
   */
  public Annotation getAnnotation( String lang )
  {
    return (Annotation)m_annotationMap.get( lang );
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getAnnotationMap()
   */
  public Map getAnnotationMap()
  {
    return m_annotationMap;
  }

}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.8  2005/02/20 18:56:50  doemming
 * *** empty log message ***
 * Revision 1.7 2005/01/26 17:55:31
 * doemming *** empty log message *** Revision 1.6 2005/01/18 12:50:42 doemming
 * *** empty log message ***
 * 
 * Revision 1.5 2004/10/11 14:44:28 doemming *** empty log message *** Revision
 * 1.4 2004/10/09 18:41:20 belger *** empty log message ***
 * 
 * Revision 1.3 2004/10/07 14:09:16 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:57:07 doemming *** empty log message *** Revision
 * 1.3 2004/08/31 14:07:12 doemming *** empty log message *** Revision 1.3
 * 2004/02/09 07:59:57 poth no message
 * 
 * Revision 1.2 2002/11/25 09:32:41 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:00:38 poth no message
 * 
 * Revision 1.4 2002/08/15 10:00:00 ap no message
 * 
 * Revision 1.3 2002/05/21 16:05:51 ap no message
 * 
 * Revision 1.2 2002/04/05 09:41:40 ap no message
 * 
 * Revision 1.1 2002/04/04 16:22:41 ap no message
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
