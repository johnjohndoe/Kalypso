/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

package org.kalypso.kalypsosimulationmodel.schema;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * Catalog which provides the url to the roughness schema.
 * 
 * @author Patrice Congo
 */
public class UrlCatalogRoughness extends AbstractUrlCatalog
{
  /**
   * Latest version for schema
   */
  final static public String CURRENT_VERSION = "v0.0.0"; //$NON-NLS-1$

  /**
   * path for simulation base schema file
   */
  final static public String REL_PATH_ROUGHNESS_DB = CURRENT_VERSION + "/roughness_model.xsd"; //$NON-NLS-1$

  /**
   * Url for simulation base model schema
   */
  final static public URL ROUGHNESS_MODEL_SCHEMA_URL = UrlCatalogRoughness.class.getResource( REL_PATH_ROUGHNESS_DB );

  /**
   * Namespace for simulation model base
   */
  final static public String NS_ROUGHNESS_MODEL = "http://www.tu-harburg.de/wb/kalypso/schemata/roughness"; //$NON-NLS-1$

  /**
   * Prefix for the simulation model base
   */
  final static public String NS_PREFIX_ROUGHNESS = "wbr"; //$NON-NLS-1$

  /**
   * Fills the catalog with the roughness base schemas path namespace and location
   * 
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class<?> myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    catalog.put( NS_ROUGHNESS_MODEL, myClass.getResource( REL_PATH_ROUGHNESS_DB ) );
    prefixes.put( NS_ROUGHNESS_MODEL, NS_PREFIX_ROUGHNESS );
  }

}
