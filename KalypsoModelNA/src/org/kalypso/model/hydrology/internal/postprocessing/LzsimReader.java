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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.KalypsoCorePreferences;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;

import com.google.common.base.Charsets;

/**
 * @author huebsch
 */
public class LzsimReader
{
  private final DateFormat m_dateTimeInstance = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final DateFormat m_formatFileName = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMdd'.gml'" ) ); //$NON-NLS-1$

  private final Date[] m_initialDates;

  private final File m_outputDir;

  public LzsimReader( final Date[] initialDates, final File outputDir )
  {
    m_initialDates = initialDates;
    m_outputDir = outputDir;
    m_dateTimeInstance.setTimeZone( KalypsoCorePreferences.getTimeZone() );
  }

  /**
   * Reads the initial values back from the ascii files, if any have been ordered.
   */
  public IStatus readInitialValues( final IDManager idManager, final ICatchmentInfos catchmentData, final File lzsimDir ) throws NaPostProcessingException
  {
    if( m_initialDates.length == 0 )
      return Status.OK_STATUS;

    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    for( final Date initialDate : m_initialDates )
    {
      final LzsToGml lzsToGml = new LzsToGml( lzsimDir, initialDate, idManager, catchmentData );
      final Pair<InitialValues, IStatus> result = lzsToGml.readLzs();

      final InitialValues initialValues = result.getLeft();
      final IStatus status = result.getRight();
      if( !status.isOK() )
        log.add( status );

      if( !status.matches( IStatus.ERROR ) )
      {
        final IStatus gmlStatus = writeGml( initialValues );
        if( !gmlStatus.isOK() )
          log.add( gmlStatus );
      }

      initialValues.getWorkspace().dispose();
    }

    return log.asMultiStatus( Messages.getString( "LzsimReader_0" ) ); //$NON-NLS-1$
  }

  // REMARK: we omit any hour here, as the calculation core does not support it. Probably this is a bug of the
  // calculation core, even if the people responsible for this do not recognize it. In the input file of the
  // calculation core the hour is specified, but the produced date is always written without hour information ('00').
  public IStatus writeGml( final InitialValues initialValues )
  {
    final Date initialDate = initialValues.getInitialDate();
    final String initialDateText = m_dateTimeInstance.format( initialDate );

    // TODO: check, if we read any data for this date, else do not write gml
    final int catchments = initialValues.getCatchments().size();
    final int channels = initialValues.getChannels().size();
    if( catchments == 0 && channels == 0 )
    {
      final String message = String.format( Messages.getString( "LzsimReader_1" ), initialDateText ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, message );
    }

    try
    {

      final String resultFilename = m_formatFileName.format( initialDate ); //$NON-NLS-1$
      final File resultFile = new File( m_outputDir, resultFilename );
      resultFile.getParentFile().mkdirs();

      GmlSerializer.serializeWorkspace( resultFile, initialValues.getWorkspace(), Charsets.UTF_8.name() );

      // final String iniDate = m_dateFormat.format( initialDate );
      // m_logger.info( Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.42", iniDate ) ); //$NON-NLS-1$

      return Status.OK_STATUS;
    }
    catch( IOException | GmlSerializeException e )
    {
      final String message = String.format( Messages.getString( "LzsimReader_2" ), initialDateText ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, message, e );
    }
  }
}