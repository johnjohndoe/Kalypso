/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor;

import org.apache.commons.configuration.Configuration;
import org.kalypso.metadoc.configuration.ConfigurationUtils;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/**
 * Extends metadata destinated for the metadoc service with information that it gets from an observation
 * 
 * @author schlienger
 */
public class MetadataExtenderWithObservation
{
  private MetadataExtenderWithObservation()
  {
  // not intended to be instanciated
  }

  /**
   * Helper that calls extendMetadata( Configuration, IObservation ) for each observation of the given items
   */
  public static void extendMetadata( final Configuration metadata, final ObsViewItem[] items )
  {
    for( int i = 0; i < items.length; i++ )
      extendMetadata( metadata, items[i].getObservation() );
  }

  /**
   * Extend the metadata that can be found in conf with information that it gets from the given observation
   * 
   * @param metadata
   *          this represents the metadata that might get extended
   * @param obs
   *          observation used to get some extra information
   */
  public static void extendMetadata( final Configuration metadata, final IObservation obs )
  {
    if( obs == null )
      return;

    final MetadataList md = obs.getMetadataList();

    // currently we only take this property, but the thing
    // could easily be extended with more
    final String kennz = md.getProperty( TimeserieConstants.MD_KENNZIFFER );
    
    ConfigurationUtils.addPropertyDistinct( metadata, TimeserieConstants.MD_KENNZIFFER, kennz );
  }
}
