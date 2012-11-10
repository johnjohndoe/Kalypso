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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.module.conversion.AbstractProjectConverter;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.utils.log.GeoStatusLog;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class RrmProjectConverter11_06to12_02 extends AbstractProjectConverter
{
  private final File m_sourceDir;

  private final File m_targetDir;

  public RrmProjectConverter11_06to12_02( final File sourceDir, final File targetDir, final Version targetVersion )
  {
    super( String.format( Messages.getString( "RrmProjectConverter103to230_0" ), sourceDir.getName(), targetVersion ) ); //$NON-NLS-1$

    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  @Override
  public IStatus preConversion( final Shell shell )
  {
    return Status.OK_STATUS;
  }

  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    /* Monitor. */
    monitor.beginTask( String.format( Messages.getString( "RrmProjectConverter103to230_1" ), m_sourceDir.getName() ), 100 ); //$NON-NLS-1$

    try
    {
      /* Convert basic model. */
      monitor.subTask( Messages.getString( "RrmProjectConverter103to230_2" ) ); //$NON-NLS-1$
      final BasicModelConverter basicModelConverter = new BasicModelConverter( m_sourceDir, m_targetDir );
      final IStatus basicStatus = basicModelConverter.execute( new SubProgressMonitor( monitor, 33 ) );
      getLog().add( basicStatus );

      /* Build global data. */
      final TimeseriesIndex timeseriesIndex = basicModelConverter.getTimeseriesIndex();

      /* read mapping files */
      final MappingData mappingData = new MappingData( m_sourceDir, timeseriesIndex );
      final IStatus mappingStatus = mappingData.read();
      getLog().add( mappingStatus );

      final GlobalConversionData globalData = new GlobalConversionData( m_sourceDir, m_targetDir, timeseriesIndex, mappingData );

      /* Convert calc cases. */
      monitor.subTask( Messages.getString( "RrmProjectConverter103to230_3" ) ); //$NON-NLS-1$
      final CalcCasesConverter casesConverter = new CalcCasesConverter( globalData );
      final IStatus calcCaseStatus = casesConverter.execute( new SubProgressMonitor( monitor, 67 ) );
      getLog().add( calcCaseStatus );

      /* convert invalid timeseries parameter types */
      final TimeseriesParameterTypeConverter timeseriesParameterTypeConverter = new TimeseriesParameterTypeConverter( m_targetDir, globalData.getConversionMap() );
      final IStatus timeseriesParameterTypeConverterStatus = timeseriesParameterTypeConverter.execute( new SubProgressMonitor( monitor, globalData.getConversionMap().size() ) );
      getLog().add( timeseriesParameterTypeConverterStatus );

    }
    finally
    {
      /* Save the log as text file. */
      saveLogQuietly( m_sourceDir.getName() );

      /* Monitor. */
      monitor.done();
    }
  }

  private void saveLogQuietly( final String projectName )
  {
    try
    {
      /* Save the log as text file. */
      final GeoStatusLog log = new GeoStatusLog( new File( m_targetDir, "conversionLog.gml" ) ); //$NON-NLS-1$
      log.log( getLog().asMultiStatus( String.format( Messages.getString( "RrmProjectConverter103to230_1" ), projectName ) ) ); //$NON-NLS-1$
      log.serialize();
    }
    catch( final Exception ex )
    {
      /* Ignore exception. */
      ex.printStackTrace();
    }
  }
}