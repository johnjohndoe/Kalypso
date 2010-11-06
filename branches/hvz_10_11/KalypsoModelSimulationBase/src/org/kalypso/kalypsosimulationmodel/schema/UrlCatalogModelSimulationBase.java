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
 * Catalog which provides the url to the simulation model base schema. This schema contains base type and elements to
 * constract other gml simulation model schema
 * 
 * @author Patrice Congo
 */
public class UrlCatalogModelSimulationBase extends AbstractUrlCatalog
{
  /**
   * Latest version for schema
   */
  final static public String CURRENT_VERSION = "v0.0.0"; //$NON-NLS-1$

  /**
   * path for simulation base schema file
   */
  final static public String SIM_MODEL_REL_PATH = CURRENT_VERSION + "/simulation_model_base.xsd"; //$NON-NLS-1$

  /**
   * Namespace for simulation model base
   */
  final static public String SIM_MODEL_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase"; //$NON-NLS-1$

  /**
   * Prefix for the simulation model base
   */
  final static public String SIM_MODEL_NS_PREFIX = "simBase"; //$NON-NLS-1$

  /**
   * path for simulation base result metadata schema file
   */
  final static public String SIM_MODEL_RESULT_REL_PATH = CURRENT_VERSION + "/simulation_model_result.xsd"; //$NON-NLS-1$

  /**
   * Namespace for simulation model base result metadata
   */
  final static public String SIM_MODEL_RESULT_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase/result/meta"; //$NON-NLS-1$

  /**
   * Prefix for the simulation model base result metadata
   */
  final static public String SIM_MODEL_RESULT_NS_PREFIX = "resultMeta"; //$NON-NLS-1$


  /**
   * Fills the catalog with the roughness and model simulation base schemas schema elements
   * 
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    catalog.put( SIM_MODEL_NS, myClass.getResource( SIM_MODEL_REL_PATH ) );
    prefixes.put( SIM_MODEL_NS, SIM_MODEL_NS_PREFIX );

    catalog.put( SIM_MODEL_RESULT_NS, myClass.getResource( SIM_MODEL_RESULT_REL_PATH ) );
    prefixes.put( SIM_MODEL_RESULT_NS, SIM_MODEL_RESULT_NS_PREFIX );
  }

}
