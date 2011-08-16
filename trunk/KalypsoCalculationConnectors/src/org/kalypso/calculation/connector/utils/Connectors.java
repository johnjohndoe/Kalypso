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
package org.kalypso.calculation.connector.utils;

import java.net.URL;

import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_NA_WSPM;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dirk Kuch
 */
public final class Connectors
{
  private Connectors( )
  {
  }

  public static GMLWorkspace getWorkspace( final ISimulationDataProvider provider, final String input )
  {
    try
    {

      final URL url = getURL( provider, input );

      return GmlSerializer.createGMLWorkspace( url, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }

  public static URL getURL( final ISimulationDataProvider provider, final String input ) throws SimulationException
  {
    final Object property = provider.getInputForID( input );
    if( !(property instanceof URL) )
      return null;

    final URL url = (URL) property;

    return url;
  }

  public static IRunOffEvent findRunOffEvent( final GMLWorkspace workspaceWSPM, final ISimulationDataProvider inputProvider ) throws SimulationException
  {
    final String runOffEventID = inputProvider.getInputForID( MODELSPEC_CONNECTOR_NA_WSPM.WSPM_RunoffEventID.name() ).toString();

    final IFeatureType wspmRunoffEventFeatureType = workspaceWSPM.getGMLSchema().getFeatureType( IRunOffEvent.FEATURE_RUNOFF_EVENT );
    final Feature[] wspmRunoffEvents = workspaceWSPM.getFeatures( wspmRunoffEventFeatureType );
    for( final Feature feature : wspmRunoffEvents )
    {
      if( !(feature instanceof IRunOffEvent) )
      {
        continue;
      }

      final IRunOffEvent event = (IRunOffEvent) feature;
      if( runOffEventID.equals( event.getId() ) )
        return event;
    }

    return null;
  }
}
