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
package org.kalypso.model.wspm.ui.featureview;

import java.net.URL;
import java.util.Properties;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.chart.factory.configuration.ChartConfigurationLoader;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureviewControlFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.ksp.chart.factory.ChartType;

/**
 * A feature control which shows a chart. The chart configuration comes from the parameters of the extension, its
 * context will be the current feature.
 * <p>
 * Supported arguments:
 * <ul>
 * <li>configuration: urn to the configuration file for the chart. The urn will be resolved against the catalogue
 * mechanism and after that relative to the current url.context (TODO: the last thing does not work yet).</li>
 * <li>featureKeyName: The key-name under which the current feature will be put into the ChartDataProvider. </li>
 * </ul>
 * 
 * @author Gernot Belger
 */
public class ChartFeatureControlFactory implements IFeatureviewControlFactory
{
  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureviewControlFactory#createFeatureControl(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.util.Properties)
   */
  public IFeatureControl createFeatureControl( final Feature feature, final IPropertyType pt, final Properties arguments )
  {
    final String configurationUrn = arguments.getProperty( "configuration", null );
    final String featureKeyName = arguments.getProperty( "featureKeyName", null );

    final String configurationUrl = KalypsoCorePlugin.getDefault().getCatalogManager().getBaseCatalog().resolve( configurationUrn, configurationUrn );

    try
    {
      final URL configUrl = new URL( configurationUrl );
      
      ChartConfigurationLoader ccl=new ChartConfigurationLoader(configUrl);
      ChartType[] chartTypes = ccl.getCharts();

      ChartDataProvider.FEATURE_MAP.put( featureKeyName, feature );
      return new ChartFeatureControl( feature, pt, chartTypes, configUrl );
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    }

    return null;
  }


}
