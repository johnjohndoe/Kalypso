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

package org.deegree.model.feature;

import java.util.Map;


/**
 * The FeatureType interface is intended to provide details of the type of a
 * Feature that are described as Feature Schema in the Abstract Specification's
 * Essential Model, specifically the names and types of the properties
 * associated with each instance of a Feature of the given FeatureType.
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface FeatureType
{
  public static final int UNBOUND_OCCURENCY = -1;
  
  /**
   * returns the direct parents of the FeatureType. If it hasn't a parent null
   * should be returned
   */
  public FeatureType[] getParents();

  /**
   * returns the direct children of the FeatureType
   * @deprecated Don't use it!
   */
  public FeatureType[] getChildren();

  /**
   * returns the name of the FeatureType
   */
  public String getName();

  /**
   * returns the properties of this feature type
   */
  public FeatureTypeProperty[] getProperties();

  /**
   * returns a property of this feature type identified by its name
   */
  public FeatureTypeProperty getProperty( String name );

  public String getNamespace();
  public int getMinOccurs(int pos);
  public int getMaxOccurs(int pos);
 

  public int getPropertyPosition( String name );
  public int getDefaultGeometryPropertyPosition();
  
  public Annotation getAnnotation(String langKey);
  public Map getAnnotationMap();
}