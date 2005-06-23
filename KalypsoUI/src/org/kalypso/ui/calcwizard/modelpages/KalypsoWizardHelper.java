/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.calcwizard.modelpages;

import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;

import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;
import org.kalypso.ui.calcwizard.Arguments;

/**
 * Provides some convenience methods for dealing with the stuff in Kalypso Wizards.
 * 
 * @author schlienger
 */
public class KalypsoWizardHelper
{
  private KalypsoWizardHelper()
  {
  // not to be instanciated
  }

  /**
   * Parses the properties by fetching all timeserieX elements out (X being whatever, as long as different from element
   * to element). A TimeserieFeatureProps is created for each of these elements.
   * 
   * <pre>
   * 
   *  &lt;arg name=&quot;timeserie1&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   *  &lt;arg name=&quot;timeserie2&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   *  
   * </pre>
   * 
   * @param props
   * @return array of TimeserieFeatureProps
   */
  public static TimeserieFeatureProps[] parseTimeserieFeatureProps( final Arguments props )
  {
    final ArrayList l = new ArrayList();

    for( final Iterator names = props.keySet().iterator(); names.hasNext(); )
    {
      final String pName = (String)names.next();

      if( pName.startsWith( ObservationMapTableDiagWizardPage.PROP_TIMEPROPNAME ) )
        l.add( new TimeserieFeatureProps( PropertiesHelper.parseFromString( (String)props.get( pName ), '#' ) ) );
    }

    return (TimeserieFeatureProps[])l.toArray( new TimeserieFeatureProps[0] );
  }

  /**
   * Updates the diagram template for the given TimeserieFeatureProps and features
   * 
   * @param view
   * @param links
   * @param context
   * @param ignoreExceptions
   * @param ignoreType
   */
  public static void updateZMLView( final ObsView view, final TSLinkWithName[] links, final URL context,
      final boolean ignoreExceptions, final String ignoreType )
  {
    view.removeAllItems();

    for( int i = 0; i < links.length; i++ )
    {
      final TSLinkWithName link = links[i];
      view.loadObservation( context, link.href, ignoreExceptions, ignoreType, link.name, new ObsView.ItemData( true,
          link.color ) );
    }
  }
}