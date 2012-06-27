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
package org.kalypso.risk.model.schema;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * FloodRisk namespace catalog
 *
 * @author Dejan Antanaskovic
 */
public class KalypsoRiskSchemaCatalog extends AbstractUrlCatalog
{
  public static final String NS_PREDEFINED_DATASET = "http://www.tu-harburg.de/wb/kalypso/risk/schemata/predefinedDataset"; //$NON-NLS-1$

  public static final String PREFIX_PREDEFINED_DATASET = "krpd"; //$NON-NLS-1$

  public static final String NS_RASTERIZATION_CONTROL_MODEL = "http://www.tu-harburg.de/wb/kalypso/risk/schemata/rasterizationControlModel"; //$NON-NLS-1$

  public static final String PREFIX_RASTERIZATION_CONTROL_MODEL = "krrcm"; //$NON-NLS-1$

  public static final String NS_VECTOR_DATA_MODEL = "http://www.tu-harburg.de/wb/kalypso/risk/schemata/vectorDataModel"; //$NON-NLS-1$

  public static final String PREFIX_VECTOR_DATA_MODEL = "krvdm"; //$NON-NLS-1$

  public static final String NS_RASTER_DATA_MODEL = "http://www.tu-harburg.de/wb/kalypso/risk/schemata/rasterDataModel"; //$NON-NLS-1$

  public static final String PREFIX_RASTER_DATA_MODEL = "krrdm"; //$NON-NLS-1$

  public static final String NS_RASTER_COMMONS = "http://www.tu-harburg.de/wb/kalypso/risk/schemata/common"; //$NON-NLS-1$

  public static final String PREFIX_RASTER_COMMONS = "kr_common"; //$NON-NLS-1$

  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, final Map<String, String> prefixes )
  {
    catalog.put( NS_PREDEFINED_DATASET, myClass.getResource( "schemata/PredefinedDataset.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NS_PREDEFINED_DATASET, PREFIX_PREDEFINED_DATASET );
    catalog.put( NS_RASTERIZATION_CONTROL_MODEL, myClass.getResource( "schemata/RasterizationControlModel.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NS_RASTERIZATION_CONTROL_MODEL, PREFIX_RASTERIZATION_CONTROL_MODEL );
    catalog.put( NS_VECTOR_DATA_MODEL, myClass.getResource( "schemata/VectorDataModel.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NS_VECTOR_DATA_MODEL, PREFIX_VECTOR_DATA_MODEL );
    catalog.put( NS_RASTER_DATA_MODEL, myClass.getResource( "schemata/RasterDataModel.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NS_RASTER_DATA_MODEL, PREFIX_RASTER_DATA_MODEL );
    catalog.put( NS_RASTER_COMMONS, myClass.getResource( "schemata/Common.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NS_RASTER_COMMONS, PREFIX_RASTER_DATA_MODEL );
  }

}