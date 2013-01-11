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
/*
 *
 * @author huebsch
 */
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypsodeegree.model.feature.Feature;

public class NutzungWriter
{
  private final File m_nutzungDir;

  private final INaSimulationData m_data;

  public NutzungWriter( final INaSimulationData data, final File nutzungDir )
  {
    m_data = data;
    m_nutzungDir = nutzungDir;
  }

  public void writeFile( final ParameterHash landuseHash ) throws IOException, NAPreprocessorException
  {
    if( landuseHash == null )
      return;

    try
    {
      final Parameter parameter = m_data.getParameter();
      final List<Feature> list = (List<Feature>)parameter.getProperty( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
      for( final Feature nutzungFE : list )
      {
        writeFeature( nutzungFE, landuseHash );
      }
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      throw new NAPreprocessorException( Messages.getString( "NutzungWriter_0" ), e ); //$NON-NLS-1$
    }
  }

  private void writeFeature( final Feature nutzung, final ParameterHash landuseHash ) throws IOException, SensorException
  {
    final String nutzName = landuseHash.getLanduseFeatureShortedName( nutzung );
    final File outputFile = new File( m_nutzungDir, nutzName + ".nuz" ); //$NON-NLS-1$
    final FileWriter writer = new FileWriter( outputFile );

    final Feature linkedIdealLanduseFE = nutzung.getMember( NaModelConstants.PARA_LANDUSE_PROP_LANDUSE_LINK );

    writer.write( linkedIdealLanduseFE.getName() );
    writer.write( "\nidealisierter jahresgang" );// "ideali" ist Kennung! //$NON-NLS-1$
    writer.write( "\nxxdatum     F EVA    We    BIMAX\n" ); //$NON-NLS-1$
    final Object idealLanduseProp = linkedIdealLanduseFE.getProperty( NaModelConstants.PARA_IDEAL_LANDUSE_ZML );
    writeIdealLanduse( (IObservation)idealLanduseProp, writer );
    writer.write( "993456789012345678901234567890" ); //$NON-NLS-1$
    IOUtils.closeQuietly( writer );
  }

  private void writeIdealLanduse( final IObservation observation, final Writer zmlWriter ) throws SensorException, IOException
  {
    final IAxis[] axisList = observation.getAxes();
    final IAxis idleDateAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_DATE );
    final IAxis kcAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_KC );
    final IAxis wtAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_WT );
    final IAxis laiAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_LAI );
    final ITupleModel values = observation.getValues( null );
    final int count = values.size();
    final int yearOffset = 2001;
    for( int row = 0; row < count; row++ )
    {
      final Date date = (Date)values.get( row, idleDateAxis );
      final Calendar calendar = NATimeSettings.getInstance().getCalendar( date );

      final int year = calendar.get( Calendar.YEAR ) - yearOffset;
      final int month = calendar.get( Calendar.MONTH ) + 1;
      final int day = calendar.get( Calendar.DATE );
      zmlWriter.write( FortranFormatHelper.printf( day, "i2" ).replaceAll( " ", "0" ) ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
      zmlWriter.write( "." ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( month, "i2" ).replaceAll( " ", "0" ) ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
      zmlWriter.write( "." ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( year, "i2" ).replaceAll( " ", "0" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      final Double kc = (Double)values.get( row, kcAxis );
      final Double wt = (Double)values.get( row, wtAxis );
      final Double lai = (Double)values.get( row, laiAxis );

      zmlWriter.write( FortranFormatHelper.printf( kc, "f8.2" ) ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( wt, "f8.2" ) ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( lai, "f8.2" ) ); //$NON-NLS-1$
      zmlWriter.write( "\n" ); //$NON-NLS-1$
    }
  }
}