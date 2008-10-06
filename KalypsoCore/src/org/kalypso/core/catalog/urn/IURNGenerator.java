/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.core.catalog.urn;

/**
 * URNGenerator generates well known URNs for objects. In combination with the usage of catalog-resolvers well-known
 * URNs can be used to build repositories of well known resources
 * @author doemming
 */
public interface IURNGenerator
{
  /**
   * @return the class type, for that urn will be generated
   */
  public Class getSupportingClass( );

  /**
   * IURNGenerator can also generate URNs from objects of other classes than the supporting class, as long as they
   * support the supporting class. <br>
   * Example: <br>
   * URNGenerator for the class IFeatureType can produce URNs for objects of IFeatureType, but also for QNames
   * 
   * @return true if this is a urn-generator for the object
   */
  public boolean isURNGeneratorFor( final Object object );

  /**
   * generates the proper URN for the object<br>
   * the URN can be used as id to put a uri into the catalog or get a uri from a catalog
   * 
   * @return urn for object
   */
  public String generateURNFor( final Object object );

  /**
   * Generate a URNPattern for a related object<br>
   * In some usecases it is usefull to query for available things in the catalog<br>
   * e.g. give me all FeatureTypeStyles that are available for a special FeatureType<br>
   * this might return "urn:ogc:gml:featuretype:ftNamespace:ftLocalname:sld:*" <br>
   * the returned URNPattern can be used with the Catalog<br>
   * 
   * @param related
   * @return URNPattern
   */
  public String generateURNPatternForRelated( final Object related );

  /**
   * Generate a default URN for a related object<br>
   * In some usecases it is usefull to query the catalog for a default object<br>
   * e.g. give me the standard FeatureTypeStyles that is available for a special FeatureType<br>
   * this might return "urn:ogc:gml:featuretype:ftNamespace:ftLocalname:sld:default" <br>
   * the returned URN can be used with the Catalog<br>
   * 
   * @param related
   * @return URNPattern
   */
  public String generateDefaultURNForRelated( final Object related );

  // TODO pattern

}
