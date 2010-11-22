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
package org.kalypso.model.hydrology;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.model.hydrology.internal.NaOptimizeLoader;
import org.kalypso.model.hydrology.internal.NaSimulationData;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationDataUtils;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author Gernot Belger
 */
public class NaSimulationDataFactory
{
  public static final INaSimulationData load( final URL modelUrl, final URL controlURL, final URL metaUrl, final URL parameterUrl, final URL hydrotopUrl, final URL sudsUrl, final URL syntNUrl, final URL lzsimUrl, final NaOptimizeLoader optimizeLoader ) throws Exception
  {
    return new NaSimulationData( modelUrl, controlURL, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl, optimizeLoader );
  }

  public static final INaSimulationData load( final ISimulationDataProvider inputProvider ) throws Exception
  {
    final URL modelUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final URL controlURL = (URL) inputProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
    final URL metaUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_META_ID );
    final URL parameterUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    final URL hydrotopUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    final URL syntNUrl = (URL) inputProvider.getInputForID( NaModelConstants.IN_RAINFALL_ID );
    final URL lzsimUrl = getStartConditionFile( inputProvider );
    final URL sudsUrl = (URL) SimulationDataUtils.getInputOrNull( inputProvider, NaModelConstants.IN_SUDS_ID );

    final NaOptimizeLoader optimizeLoader = new NaOptimizeLoader( inputProvider );

    return new NaSimulationData( modelUrl, controlURL, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl, optimizeLoader );
  }

  public static URL getStartConditionFile( final ISimulationDataProvider inputProvider ) throws SimulationException
  {
    if( !inputProvider.hasID( NaModelConstants.IN_LZSIM_IN_ID ) )
      return null;

    final URL iniValuesFolderURL = (URL) inputProvider.getInputForID( NaModelConstants.IN_LZSIM_IN_ID );
    try
    {
      // TODO: crude way to create the new URL, necessary as probably we do not have a '/' at the end of the path
      return new URL( iniValuesFolderURL.toExternalForm() + "/lzsim.gml" ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      throw new SimulationException( "Failed to read start condition file", e );
    }
  }
}
