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

package org.kalypso.kalypsomodel1d2d.schema;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * Catalog which provides the url to the 1d2d schema
 * 
 * @author Patrice Congo
 */
public class UrlCatalog1D2D extends AbstractUrlCatalog
{
  final static public String MODEL_1D2D_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/1d2d"; //$NON-NLS-1$

  final static public String MODEL_1D2DControl_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/1d2dControl"; //$NON-NLS-1$

  final static public String MODEL_1D2DOperational_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel"; //$NON-NLS-1$

  final static public String MODEL_1D2DObservation_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/observation"; //$NON-NLS-1$

  final static public String MODEL_1D2DResult_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResultMeta"; //$NON-NLS-1$

  final static public String MODEL_1D2DResults_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResults"; //$NON-NLS-1$

  final static public String MODEL_1D2DSIMMETA_NS = "http://www.tu-harburg.de/wb/kalypso/schemata/simulationmetadatamodel"; //$NON-NLS-1$

  final static public String MODEL_1D2D_NS_PREFIX = "wb1d2d"; //$NON-NLS-1$

  final static public String MODEL_1D2DControl_NS_PREFIX = "c1d2d"; //$NON-NLS-1$

  final static public String MODEL_1D2DOperational_NS_PREFIX = "op1d2d"; //$NON-NLS-1$

  final static public String MODEL_1D2DResult_NS_PREFIX = "resultMeta1d2d"; //$NON-NLS-1$

  final static public String MODEL_1D2DResults_NS_PREFIX = "res1d2d"; //$NON-NLS-1$

  final static public String MODEL_1D2DSIMMETA_NS_PREFIX = "simMeta1d2d"; //$NON-NLS-1$

  /**
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, final Map<String, String> prefixes )
  {
    catalog.put( MODEL_1D2D_NS, myClass.getResource( "v0.0/sim_1d2d_model.xsd" ) ); //$NON-NLS-1$
    catalog.put( MODEL_1D2DControl_NS, myClass.getResource( "v0.0/sim_1d2d_control.xsd" ) ); //$NON-NLS-1$
    catalog.put( MODEL_1D2DOperational_NS, myClass.getResource( "v0.0/sim_1d2d_operational.xsd" ) ); //$NON-NLS-1$
    catalog.put( MODEL_1D2DObservation_NS, myClass.getResource( "v0.0/sim_1d2d_observation.xsd" ) ); //$NON-NLS-1$
    catalog.put( MODEL_1D2DResult_NS, myClass.getResource( "v0.0/sim_1d2d_result.xsd" ) ); //$NON-NLS-1$
    catalog.put( MODEL_1D2DResults_NS, myClass.getResource( "v0.0/sim_1d2d_results.xsd" ) ); //$NON-NLS-1$
    catalog.put( MODEL_1D2DSIMMETA_NS, myClass.getResource( "v0.0/sim_1d2d_model_metadata.xsd" ) ); //$NON-NLS-1$
    prefixes.put( MODEL_1D2D_NS, MODEL_1D2D_NS_PREFIX );
    prefixes.put( MODEL_1D2DControl_NS, MODEL_1D2DControl_NS_PREFIX );
    prefixes.put( MODEL_1D2DOperational_NS, MODEL_1D2DOperational_NS_PREFIX );
    prefixes.put( MODEL_1D2DResult_NS, MODEL_1D2DResult_NS_PREFIX );
    prefixes.put( MODEL_1D2DResults_NS, MODEL_1D2DResults_NS_PREFIX );
    prefixes.put( MODEL_1D2DSIMMETA_NS, MODEL_1D2DSIMMETA_NS_PREFIX );
  }
}
