/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.SWT;
import org.kalypso.chart.factory.configuration.ChartConfigurationLoader;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureviewControlFactory;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.model.feature.Feature;

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
    final String cmdIds = arguments.getProperty( "commands", "" );
    final String cmdStyles = arguments.getProperty( "commandStyles", "" );

    final String[] commandIds = cmdIds.split( ";" );
    final String[] commandStyles = cmdStyles.split( ";" );

    final Map<String, Integer> commands = new LinkedHashMap<String, Integer>();

    if( commandIds.length != commandStyles.length )
    {
      final String msg = String.format( "Number of command-ids (%s - %d) is not queal to number of command styles (%s - %d)", commandIds, commandIds.length, cmdStyles, commandStyles.length );
      StatusUtilities.createStatus( IStatus.WARNING, msg, null );
    }
    else
    {
      for( int i = 0; i < commandIds.length; i++ )
      {
        final String cmdId = commandIds[i].trim();
        final String cmdStyle = commandStyles[i].trim();

        if( cmdId.length() > 0 && cmdStyle.length() > 0 )
        {
          final int styleFromString = SWTUtilities.createStyleFromString( cmdStyle );
          final int style = styleFromString == 0 ? SWT.PUSH : styleFromString;
          commands.put( cmdId, style );
        }
      }
    }

    final String configurationUrl = KalypsoCorePlugin.getDefault().getCatalogManager().getBaseCatalog().resolve( configurationUrn, configurationUrn );

    try
    {
      // TODO: it should also be possible to use a url-context here in order to load .kod relative to the gft file
      // IN order to do this, we need the context-url from the framework.
      final URL configUrl = new URL( configurationUrl );

      final ChartConfigurationLoader ccl = new ChartConfigurationLoader( configUrl );

      ChartDataProvider.FEATURE_MAP.put( featureKeyName, feature );

      return new ChartFeatureControl( feature, pt, ccl, configUrl, commands );
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    }

    return null;
  }

}
