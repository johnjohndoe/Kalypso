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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.Platform;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.swtchart.configuration.NewConfigLoader;
import org.ksp.chart.configuration.AxisType;
import org.ksp.chart.configuration.ChartType;
import org.ksp.chart.configuration.ConfigurationType;

/**
 * 
 * @author Gernot Belger, Alex Burtscher
 * 
 */
public class LaengsschnittHelper
{
  private LaengsschnittHelper( )
  {
    // Helper class, dont instantiate
  }

  @SuppressWarnings("unchecked")
  public static void createDiagram( final File diagFile, final IObservation<TupleResult> lsObs, final boolean isDirectionUpstreams ) throws JAXBException, IOException
  {
    // Check if optional bundle is installed
    if( Platform.getBundle( "org.kalypso.chart" ) == null )
      return;

    /* We just load the template and tweak the direction of the station-axis */
    final Unmarshaller unmarshaller = NewConfigLoader.JC.createUnmarshaller();
    final JAXBElement<ConfigurationType> config = (JAXBElement<ConfigurationType>) unmarshaller.unmarshal( LaengsschnittHelper.class.getResource( "resources/lengthSection.kod" ) );
    
    List<Object> chartOrLayerOrAxis = config.getValue().getChartOrLayerOrAxis();
    for( Object object : chartOrLayerOrAxis )
    {
      if (object instanceof ChartType)
      {
        ChartType ct=(ChartType) object;
        ct.setTitle( lsObs.getName() );
        ct.setDescription( lsObs.getDescription() );
      }
      else if (object instanceof AxisType)
      {
        AxisType axis=(AxisType) object;
        if( axis.getName().equals( "Station_Axis" ) )
        {
          if( isDirectionUpstreams )
            axis.setDirection( "NEGATIVE" );
          else
            axis.setDirection( "POSITIVE" );
        }
      }
    }

    final Marshaller marshaller = JaxbUtilities.createMarshaller( NewConfigLoader.JC, true );

    OutputStream os = null;

    try
    {
      os = new FileOutputStream( diagFile );
      marshaller.marshal( config, os );
      os.close();
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

}
