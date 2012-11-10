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

package org.kalypso.model.hydrology.internal.gml;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding._11_6.NAControl;
import org.kalypso.model.hydrology.binding._11_6.NAModellControl;

/**
 * class UrlCatalogNA provides the schemas for kalypso rainfall runoff simulation created by
 * 
 * @author doemming (08.05.2005)
 */
public class UrlCatalogNA extends AbstractUrlCatalog
{
  public static final String PREFIX_RRM = "rrm";//$NON-NLS-1$

  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, final Map<String, String> prefixes )
  {
    catalog.put( NAControl.NS_NAMETA, myClass.getResource( "/etc/schema/gml/11.6/control.xsd" ) ); //$NON-NLS-1$
    catalog.put( NAModellControl.NS_NACONTROL, myClass.getResource( "/etc/schema/gml/11.6/nacontrol.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAHYDROTOP_11_6, myClass.getResource( "/etc/schema/gml/11.6/hydrotop.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NALANDUSE_11_6, myClass.getResource( "/etc/schema/gml/11.6/landuse.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAGEOLOGIE_11_6, myClass.getResource( "/etc/schema/gml/11.6/geologie.xsd" ) ); //$NON-NLS-1$

    catalog.put( NaModelConstants.NS_NASUDS, myClass.getResource( "/etc/schema/gml/11.6/suds.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NASUDS, "rrmSuds" ); //$NON-NLS-1$

    catalog.put( NaModelConstants.NS_NAHYDROTOPETYPES, myClass.getResource( "/etc/schema/gml/hydrotopeTypes.xsd" ) ); //$NON-NLS-1$

    catalog.put( NaModelConstants.NS_NAMETA, myClass.getResource( "/etc/schema/gml/control.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAMODELL, myClass.getResource( "/etc/schema/gml/namodell.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NACONTROL, myClass.getResource( "/etc/schema/gml/nacontrol.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAHYDROTOP, myClass.getResource( "/etc/schema/gml/hydrotop.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAPARAMETER, myClass.getResource( "/etc/schema/gml/parameter.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_OMBROMETER, myClass.getResource( "/etc/schema/gml/11.6/ombrometer.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_SYNTHN, myClass.getResource( "/etc/schema/gml/synthN.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_INIVALUES, myClass.getResource( "/etc/schema/gml/initialValues.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAFORTRANLOG, myClass.getResource( "/etc/schema/gml/NAFortranLog.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAOPTIMIZE, myClass.getResource( "/etc/schema/gml/naoptimize.xsd" ) ); //$NON-NLS-1$

    catalog.put( NaModelConstants.NS_NALANDUSE, myClass.getResource( "/etc/schema/gml/landuse.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAPEDOLOGIE, myClass.getResource( "/etc/schema/gml/pedologie.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAGEOLOGY, myClass.getResource( "/etc/schema/gml/geologie.xsd" ) ); //$NON-NLS-1$
    catalog.put( NaModelConstants.NS_NAOVERLAY, myClass.getResource( "/etc/schema/gml/hydrotopeOverlay.xsd" ) ); //$NON-NLS-1$

    // REMARK: these prefix definition are crucial for the optimisation, as the
    // sce xpathes rely on this special prefix.
    prefixes.put( NaModelConstants.NS_NAMETA, "rrmMeta" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAMODELL, PREFIX_RRM );
    prefixes.put( NaModelConstants.NS_NACONTROL, "rrmControl" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAHYDROTOP, "rrmHydrotop" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAPARAMETER, "rrmParam" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_OMBROMETER, "rrmOmbro" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_SYNTHN, "syn" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_INIVALUES, "ini" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAFORTRANLOG, "naLog" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAOPTIMIZE, "rrmOptimize" ); //$NON-NLS-1$

    prefixes.put( NaModelConstants.NS_NALANDUSE, "rrmLanduse" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAPEDOLOGIE, "rrmPedo" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAGEOLOGY, "rrmGeo" ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_NAOVERLAY, "rrmHydo" ); //$NON-NLS-1$

    catalog.put( NaModelConstants.NS_TIMESERIES_MANAGEMENT, myClass.getResource( "/etc/schema/gml/timeseriesManagement.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "tm" ); //$NON-NLS-1$

    catalog.put( NaModelConstants.NS_CATCHMENT_MODEL, myClass.getResource( "/etc/schema/gml/catchmentModel.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_CATCHMENT_MODEL, "cmrrm" ); //$NON-NLS-1$

    catalog.put( NaModelConstants.NS_TIMESERIES_MAPPING, myClass.getResource( "/etc/schema/gml/timeseriesMapping.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NaModelConstants.NS_TIMESERIES_MAPPING, "tmrrm" ); //$NON-NLS-1$
  }
}
