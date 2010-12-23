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
package org.kalypso.ui.rrm.wizards.conversion.to10_10;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaSimulationDataFactory;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Converts one calc case.
 * 
 * @author Gernot Belger
 */
public class CalcCaseConverter extends AbstractLoggingOperation
{
  private final File m_targetCalcCaseDir;

  private final File m_sourceDir;

  private INaSimulationData m_data;

  private final String m_chosenExe;

  private final File m_targetProjectDir;

  public CalcCaseConverter( final File sourceDir, final File targetProjectDir, final File targetcalcCaseDir, final String chosenExe )
  {
    super( sourceDir.getName() );

    m_sourceDir = sourceDir;
    m_targetProjectDir = targetProjectDir;
    m_targetCalcCaseDir = targetcalcCaseDir;
    m_chosenExe = chosenExe;
  }

  /**
   * @see org.kalypso.ui.rrm.wizards.conversion.AbstractLoggingOperation#doExecute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    try
    {
      copyData();
    }
    finally
    {
      ProgressUtilities.done( monitor );

      if( m_data != null )
        m_data.dispose();
    }
  }

  private void readBasicData( ) throws Exception
  {
    final File naModelFile = new File( m_targetCalcCaseDir, INaCalcCaseConstants.CALC_CASE );
    final File naControlFile = new File( m_targetCalcCaseDir, INaCalcCaseConstants.DOT_CALCULATION );
    final File naLzsimFile = new File( m_targetCalcCaseDir, INaCalcCaseConstants.ANFANGSWERTE_FILE );

    final URL naModelLocation = naModelFile.toURI().toURL();
    final URL naControlLocation = naControlFile.toURI().toURL();
    final URL naLzsimLocation = naLzsimFile.toURI().toURL();

    m_data = NaSimulationDataFactory.load( naModelLocation, null, naControlLocation, null, null, null, null, naLzsimLocation, null, null );
  }

  private void copyData( ) throws Exception
  {
    m_targetCalcCaseDir.mkdirs();

    copyBasicData();

    readBasicData();

    renameOldResults();

    /* Copy additional files */
    // TODO: wir könnten alles 'unbekannte' z.B. alles Vorlagentypen grundsätzlich mitkopieren...

    /* Tweak model data */
    tweakCalculation();

    /* Make sure that all timeseries are long enough */
    extendTimeseries();
  }

  private void renameOldResults( )
  {
    final File aktuellDir = new File( m_targetCalcCaseDir, INaCalcCaseConstants.AKTUELL_DIR );
    final File resultDir = new File( m_targetCalcCaseDir, INaCalcCaseConstants.ERGEBNISSE_DIR );
    final File origCurrentResultDir = new File( resultDir, Messages.getString("CalcCaseConverter.0") ); //$NON-NLS-1$
    if( aktuellDir.isDirectory() )
    {
      aktuellDir.renameTo( origCurrentResultDir );
      final String msg = String.format( Messages.getString("CalcCaseConverter.1"), origCurrentResultDir.getName() ); //$NON-NLS-1$
      getLog().add( IStatus.INFO, msg );
      return;
    }
  }

  private void copyBasicData( ) throws IOException
  {
    /* Copy top level gml files (everything else in this path will be ignored) */
    copyFile( INaCalcCaseConstants.CALC_CASE );
    copyHydrotopeFile();

    copyFile( INaCalcCaseConstants.CALC_PARAMETER );
    copyFile( INaCalcCaseConstants.EXPERT_CONTROL );
    copyFile( INaCalcCaseConstants.DOT_CALCULATION );

    /* Copy special directories */
    copyDir( INaCalcCaseConstants.ANFANGSWERTE_DIR );
    copyDir( INaCalcCaseConstants.KLIMA_DIR );
    copyDir( INaCalcCaseConstants.NIEDERSCHLAG_DIR );
    copyDir( INaCalcCaseConstants.PEGEL_DIR );
    copyZufluss();

    // TODO: Benutzer entscheiden lassen:
    // - ergebnisse übernehmen?
    // - nur 'aktuell' oder alle ergebnisse?
    // Momentan: es wird immer alles kopiert
    copyDir( INaCalcCaseConstants.ERGEBNISSE_DIR );

    getLog().add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("CalcCaseConverter_0") ) ); //$NON-NLS-1$
  }

  private void copyZufluss( ) throws IOException
  {
    final File modelSourceDir = new File( m_sourceDir, "Zufluss" ); //$NON-NLS-1$
    final File modelTargetDir = new File( m_targetCalcCaseDir, "Zufluss" ); //$NON-NLS-1$
    if( modelSourceDir.isDirectory() )
      FileUtils.copyDirectory( modelSourceDir, modelTargetDir, true );
    else
      getLog().add( IStatus.INFO, Messages.getString("CalcCaseConverter.6") ); //$NON-NLS-1$
  }

  /**
   * Special case: the hydrotope file may not exists in al calc cases (mainly in 2.2 versions).
   */
  private void copyHydrotopeFile( ) throws IOException
  {
    final File hydrotopeSourceFile = new File( m_sourceDir, INaCalcCaseConstants.CALC_HYDROTOP );
    if( hydrotopeSourceFile.isFile() )
      copyFile( INaCalcCaseConstants.CALC_HYDROTOP );
    else
    {
      final File basicHydrotopeFile = new File( m_targetProjectDir, INaProjectConstants.GML_HYDROTOP_PATH );
      final File targetHydrotopeFile = new File( m_targetCalcCaseDir, INaCalcCaseConstants.CALC_HYDROTOP );
      FileUtils.copyFile( basicHydrotopeFile, targetHydrotopeFile, false );

      getLog().add( IStatus.WARNING, Messages.getString("CalcCaseConverter.7") ); //$NON-NLS-1$
    }
  }

  private void copyFile( final String path ) throws IOException
  {
    copyFile( path, path );
  }

  private void copyFile( final String sourcePath, final String targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceDir, sourcePath );
    final File modelTargetFile = new File( m_targetCalcCaseDir, targetPath );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );
  }

  private void copyDir( final String path ) throws IOException
  {
    copyDir( path, path );
  }

  private void copyDir( final String sourcePath, final String targetPath ) throws IOException
  {
    final File modelSourceDir = new File( m_sourceDir, sourcePath );
    final File modelTargetDir = new File( m_targetCalcCaseDir, targetPath );

    FileUtils.copyDirectory( modelSourceDir, modelTargetDir, true );
  }

  private void tweakCalculation( ) throws Exception
  {
    tweakStartTime();

    tweakExeVersion();
  }

  private void tweakStartTime( ) throws IOException, GmlSerializeException
  {
    final NAControl metaControl = m_data.getMetaControl();
    /* The simulation time is irrelevant for syntetic events -> no need for correction */
    if( m_data.getMetaControl().isUsePrecipitationForm() )
      return;

    /* We are going to set the hours to 0 in the current time zone */
    final DateFormat displayDateTimeFormat = KalypsoGisPlugin.getDefault().getDisplayDateTimeFormat();

    /* Tweak simulation start */
    final Date simulationStart = metaControl.getSimulationStart();
    final Date newSimulationStart = tweakStartDate( simulationStart );
    if( newSimulationStart != null )
    {
      metaControl.setSimulationStart( newSimulationStart );
      final String simulationStartText = displayDateTimeFormat.format( simulationStart );
      final String statusMsg = String.format( Messages.getString("CalcCaseConverter.2"), simulationStartText ); //$NON-NLS-1$
      final int severity = IStatus.WARNING;
      saveModel( metaControl, INaCalcCaseConstants.DOT_CALCULATION, severity, statusMsg );
    }

    /* Tweak start condition */
    final InitialValues initialValues = m_data.getInitialValues();
    if( initialValues == null )
      return;

    final Date initialDate = initialValues.getInitialDate();
    final Date newInitialDate = tweakStartDate( initialDate );
    if( newInitialDate != null )
    {
      initialValues.setInitialDate( newInitialDate );
      final String initialValuesText = displayDateTimeFormat.format( initialDate );
      final String statusMsg = String.format( Messages.getString("CalcCaseConverter.3"), initialValuesText ); //$NON-NLS-1$
      final int severity = IStatus.WARNING;
      saveModel( initialValues, INaCalcCaseConstants.ANFANGSWERTE_FILE, severity, statusMsg );
    }
  }

  private Date tweakStartDate( final Date oldDate )
  {
    final TimeZone timeZone = KalypsoCorePlugin.getDefault().getTimeZone();
    final Calendar cal = Calendar.getInstance( timeZone );
    cal.setTime( oldDate );

    final int oldHour = cal.get( Calendar.HOUR_OF_DAY );
    if( oldHour == 0 )
      return null;

    cal.set( Calendar.HOUR_OF_DAY, 0 );

    if( oldHour > 12 )
    {
      // should we round up in this case (i.e. add one day)?
    }

    return cal.getTime();
  }

  private void tweakExeVersion( ) throws IOException, GmlSerializeException
  {
    final NAControl metaControl = m_data.getMetaControl();
    final String exeVersion = metaControl.getExeVersion();
    if( m_chosenExe != null )
    {
      metaControl.setExeVersion( m_chosenExe );
      // FIXME: we should write in the same encoding as we read the file
      final String statusMsg = Messages.getString( "CalcCaseConverter_2", m_chosenExe, exeVersion ); //$NON-NLS-1$
      final int severity = IStatus.OK;
      saveModel( metaControl, INaCalcCaseConstants.DOT_CALCULATION, severity, statusMsg );
    }
    else
    {
      final String statusMsg = String.format( Messages.getString("CalcCaseConverter_3"), exeVersion ); //$NON-NLS-1$
      getLog().add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), statusMsg ) );
    }

    metaControl.getWorkspace().dispose();
  }

  private void saveModel( final Feature model, final String path, final int severity, final String statusMsg ) throws IOException, GmlSerializeException
  {
    final File naControlFile = new File( m_targetCalcCaseDir, path );
    GmlSerializer.serializeWorkspace( naControlFile, model.getWorkspace(), "UTF-8" ); //$NON-NLS-1$
    getLog().add( new Status( severity, KalypsoUIRRMPlugin.getID(), statusMsg ) );
  }

  /**
   * Extends all timeseries, so they cover the calculatin span.<br/>
   * This is necessary, as the newer calculatin core versions throw a severe error, if the calculatin span is not
   * completely covered.
   */
  private void extendTimeseries( ) throws Exception
  {
    if( m_data.getMetaControl().isUsePrecipitationForm() )
    {
      final String okMsg = Messages.getString("CalcCaseConverter_4"); //$NON-NLS-1$
      getLog().add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), okMsg ) );
      return;
    }

    final Interval simulationRange = getSimulationRange();

    /* Read gml-model */
    final NaModell naModel = m_data.getNaModel();

    /* recurse through all net elements for timeseries links */
    final TimeseriesExtendVisitor visitor = new TimeseriesExtendVisitor( simulationRange );
    naModel.getWorkspace().accept( visitor, naModel, FeatureVisitor.DEPTH_INFINITE );
    final IStatus log = visitor.getStatus();
    getLog().add( log );
  }

  private Interval getSimulationRange( )
  {
    /* Read calculation time span */
    final NAControl metaControl = m_data.getMetaControl();

    final Date simulationStart = metaControl.getSimulationStart();
    final Date simulationEnd = metaControl.getSimulationEnd();
    final Integer minutesOfTimestep = metaControl.getMinutesOfTimestep();
    final Period step = findPeriod( minutesOfTimestep );

    DateTime start = new DateTime( simulationStart );
    DateTime end = new DateTime( simulationEnd );

    /*
     * HACK: really hacky: the newer calc core versions need bigger timeseries than their actual simulation range (1
     * steps backwards, 3 forwards)
     */
    start = start.minus( step );
    end = end.plus( step );
    end = end.plus( step );
    end = end.plus( step );

    return new Interval( start, end );
  }

  // FIXME: move to na utils
  /**
   * Hacky: we need a period depending on the actual meaning of the timestep (is it days, hours, or minutes)?<br/>
   * If we just use minutes, we get problems with step years/minutes and so on....
   */
  private Period findPeriod( final Integer minutes )
  {
    /* Days */
    if( minutes % (60 * 24) == 0 )
      return new Period( 0, 0, 0, minutes / (60 * 24), 0, 0, 0, 0 );

    /* Hours */
    if( minutes % 60 == 0 )
      return new Period( minutes / 60, 0, 0, 0 );

    /* Minutes */
    return new Period( 0, minutes, 0, 0 );
  }
}
