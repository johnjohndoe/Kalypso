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
import java.io.IOException;
import java.util.Date;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.model.hydrology.binding.InitialValue;
import org.kalypso.model.hydrology.binding._11_6.InitialValues;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.conversion.ITimeseriesVisitor;
import org.kalypso.ui.rrm.internal.conversion.TimeseriesWalker;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Converts one calc case.
 *
 * @author Gernot Belger
 */
public class CalcCaseConverter extends AbstractLoggingOperation
{
  private final String DOT_CALCULATION = ".calculation"; //$NON-NLS-1$

  private final String CALC_CASE = "calcCase.gml"; //$NON-NLS-1$

  private final String CALC_HYDROTOP = "calcHydrotop.gml"; //$NON-NLS-1$

  private final String CALC_PARAMETER = "calcParameter.gml"; //$NON-NLS-1$

  private final File m_sourceCalcCaseDir;

  private final File m_targetCalcCaseDir;

  private final GlobalConversionData m_globalData;

  private final ConverterData m_data;

  public CalcCaseConverter( final File sourceCalcCaseDir, final File targetCalcCaseDir, final GlobalConversionData globalData )
  {
    super( sourceCalcCaseDir.getName() );

    m_sourceCalcCaseDir = sourceCalcCaseDir;
    m_targetCalcCaseDir = targetCalcCaseDir;
    m_globalData = globalData;

    m_data = new ConverterData( m_targetCalcCaseDir );
  }

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
    }
  }

  private void copyData( ) throws Exception
  {
    m_targetCalcCaseDir.mkdirs();

    copyBasicData();

    renameOldResults();

    /* Convert old control files */
    final NAControl newControl = convertControls();

    final NaModell naModel = m_data.loadNaModel();

    fixTimeseriesLinks( naModel, getLog() );

    final CatchmentModelBuilder catchmentModelBuilder = guessCatchmentModel( naModel );

    m_data.saveModel( naModel, INaProjectConstants.GML_MODELL_PATH );
    naModel.getWorkspace().dispose();

    /* Add the simulation. */
    final NAControl simulation = addSimulation( newControl, catchmentModelBuilder );

    /* Save the calculation.gml. */
    m_data.saveModel( newControl, INaCalcCaseConstants.CALCULATION_GML_PATH );

    /* Verify timeseries of the catchment models. */
    final CatchmentModelVerifier verifier = new CatchmentModelVerifier( m_globalData, simulation, m_targetCalcCaseDir.getParentFile() );
    final IStatus verifierStatus = verifier.execute();
    getLog().add( verifierStatus );
  }

  private void copyBasicData( ) throws IOException
  {
    /* Copy top level gml files (everything else in this path will be ignored) */
    copyFile( CALC_CASE, INaProjectConstants.GML_MODELL_PATH );
    copyFile( CALC_HYDROTOP, INaProjectConstants.GML_HYDROTOP_PATH );
    copyFile( CALC_PARAMETER, INaProjectConstants.GML_PARAMETER_PATH );
    copyFile( INaCalcCaseConstants.EXPERT_CONTROL_FILE, INaCalcCaseConstants.EXPERT_CONTROL_PATH );
    copyFile( DOT_CALCULATION, INaCalcCaseConstants.CALCULATION_GML_PATH );

    /* Copy special directories */
    copyDir( INaCalcCaseConstants.ANFANGSWERTE_DIR, INaCalcCaseConstants.ANFANGSWERTE_DIR );
    copyDir( INaCalcCaseConstants.KLIMA_DIR );
    copyDir( INaCalcCaseConstants.NIEDERSCHLAG_DIR );
    copyPegel();
    copyZufluss();

    copyDir( INaCalcCaseConstants.ERGEBNISSE_DIR );

    getLog().add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "CalcCaseConverter_0" ) ) ); //$NON-NLS-1$
  }

  private void renameOldResults( )
  {
    final File aktuellDir = new File( m_targetCalcCaseDir, INaCalcCaseConstants.AKTUELL_DIR );
    final File resultDir = new File( m_targetCalcCaseDir, INaCalcCaseConstants.ERGEBNISSE_DIR );
    final File origCurrentResultDir = new File( resultDir, Messages.getString( "CalcCaseConverter.0" ) ); //$NON-NLS-1$
    if( aktuellDir.isDirectory() )
    {
      aktuellDir.renameTo( origCurrentResultDir );
      final String msg = String.format( Messages.getString( "CalcCaseConverter.1" ), origCurrentResultDir.getName() ); //$NON-NLS-1$
      getLog().add( IStatus.INFO, msg );
      return;
    }
  }

  private void copyPegel( ) throws IOException
  {
    final File modelSourceDir = new File( m_sourceCalcCaseDir, INaCalcCaseConstants.PEGEL_DIR ); //$NON-NLS-1$
    final File modelTargetDir = new File( m_targetCalcCaseDir, INaCalcCaseConstants.PEGEL_DIR ); //$NON-NLS-1$
    if( modelSourceDir.isDirectory() )
      FileUtils.copyDirectory( modelSourceDir, modelTargetDir, true );
    else
    {
      getLog().add( IStatus.INFO, Messages.getString( "CalcCaseConverter.5" ) ); //$NON-NLS-1$
      modelTargetDir.mkdirs();
    }
  }

  private void copyZufluss( ) throws IOException
  {
    final File modelSourceDir = new File( m_sourceCalcCaseDir, "Zufluss" ); //$NON-NLS-1$
    final File modelTargetDir = new File( m_targetCalcCaseDir, "Zufluss" ); //$NON-NLS-1$
    if( modelSourceDir.isDirectory() )
      FileUtils.copyDirectory( modelSourceDir, modelTargetDir, true );
    else
      getLog().add( IStatus.INFO, Messages.getString( "CalcCaseConverter.6" ) ); //$NON-NLS-1$
  }

  private void copyFile( final String sourcePath, final String targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceCalcCaseDir, sourcePath );
    final File modelTargetFile = new File( m_targetCalcCaseDir, targetPath );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );
  }

  private void copyDir( final String path ) throws IOException
  {
    copyDir( path, path );
  }

  private void copyDir( final String sourcePath, final String targetPath ) throws IOException
  {
    final File modelSourceDir = new File( m_sourceCalcCaseDir, sourcePath );
    final File modelTargetDir = new File( m_targetCalcCaseDir, targetPath );

    FileUtils.copyDirectory( modelSourceDir, modelTargetDir, true );
  }

  private NAControl convertControls( ) throws Exception
  {
    /* Convert Meta control */
    final org.kalypso.model.hydrology.binding._11_6.NAControl metaControl = m_data.loadMetaControl();
    final NAControl newMetaControl = convertMetaControl( metaControl );

    tweakExeVersion( newMetaControl );

    m_data.saveModel( newMetaControl, INaCalcCaseConstants.CALCULATION_GML_PATH );

    /* Convert modell control */
    final org.kalypso.model.hydrology.binding._11_6.NAModellControl modelControl = m_data.loadControl();
    final NAModellControl newModelControl = convertModelControl( modelControl );

    m_data.saveModel( newModelControl, INaCalcCaseConstants.EXPERT_CONTROL_PATH );

    return newMetaControl;
  }

  private NAControl convertMetaControl( final org.kalypso.model.hydrology.binding._11_6.NAControl oldControl ) throws GMLSchemaException
  {
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( NAControl.FEATURE_NACONTROL, null, null );
    final NAControl newControl = (NAControl) workspace.getRootFeature();

    /* Metadata */
    newControl.setDescription( oldControl.getDescription2() );
    newControl.setEditor( oldControl.getEditor() );
    newControl.setComment( oldControl.getComment() );
    newControl.setCreationTime( oldControl.getCalcTime() );

    /* Control parameters */
    newControl.setSimulationEnd( oldControl.getSimulationEnd() );
    newControl.setSimulationStart( oldControl.getSimulationStart() );
    newControl.setReturnPeriod( oldControl.getReturnPeriod() );
    newControl.setExeVersion( oldControl.getExeVersion() );
    newControl.setMinutesOfTimestep( oldControl.getMinutesOfTimestep() );

    /* Synthetic precipitation */
    newControl.setDurationMinutes( oldControl.getDurationMinutes() );
    newControl.setUsePrecipitationForm( oldControl.isUsePrecipitationForm() );
    newControl.setPrecipitationForm( oldControl.getPrecipitationForm() );

    getLog().add( IStatus.OK, Messages.getString( "CalcCaseConverter.8" ) ); //$NON-NLS-1$

    return newControl;
  }

  private NAModellControl convertModelControl( final org.kalypso.model.hydrology.binding._11_6.NAModellControl oldControl ) throws GMLSchemaException
  {
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( NAModellControl.FEATURE_NA_MODELL_CONTROL, null, null );
    final NAModellControl newControl = (NAModellControl) workspace.getRootFeature();

    /* Copy all boolean values that exists in both models */
    final IFeatureType oldType = oldControl.getFeatureType();
    final IFeatureType newType = newControl.getFeatureType();

    final IPropertyType[] oldTypes = oldType.getProperties();
    for( final IPropertyType oldProperty : oldTypes )
    {
      final IPropertyType newProperty = newType.getProperty( new QName( NAModellControl.NS_NACONTROL, oldProperty.getQName().getLocalPart() ) );
      if( oldProperty instanceof IValuePropertyType && newProperty instanceof IValuePropertyType )
      {
        if( ((IValuePropertyType) oldProperty).getValueClass() == Boolean.class )
        {
          final Object oldValue = oldControl.getProperty( oldProperty );
          newControl.setProperty( newProperty, oldValue );
        }
      }
    }

    /* Copy initial values */
    final IFeatureBindingCollection<InitialValue> newInitialValues = newControl.getInitialValues();
    final IFeatureBindingCollection<InitialValues> oldInitialValues = oldControl.getInitialValues();
    for( final InitialValues oldInitialValue : oldInitialValues )
    {
      final InitialValue newInitialValue = newInitialValues.addNew( InitialValue.FEATURE_INITIAL_VALUE );

      final Date date = oldInitialValue.getInitialDate();
      final boolean isActive = oldInitialValue.doWrite();

      newInitialValue.setInitialDate( date );
      newInitialValue.setActive( isActive );
    }

    getLog().add( IStatus.OK, Messages.getString( "CalcCaseConverter.9" ) ); //$NON-NLS-1$

    return newControl;
  }

  private void tweakExeVersion( final NAControl newMetaControl )
  {
    final String exeVersion = newMetaControl.getExeVersion();
    final String chosenExe = m_globalData.getChosenExe();
    if( chosenExe != null )
    {
      newMetaControl.setExeVersion( chosenExe );

      final String statusMsg = Messages.getString( "CalcCaseConverter_2", chosenExe, exeVersion ); //$NON-NLS-1$
      getLog().add( IStatus.OK, statusMsg );
    }
    else
    {
      final String statusMsg = String.format( Messages.getString( "CalcCaseConverter_3" ), exeVersion ); //$NON-NLS-1$
      getLog().add( IStatus.OK, statusMsg );
    }

    newMetaControl.getWorkspace().dispose();
  }

  /**
   * Add an additional '../' to every timeseries path.
   */
  static void fixTimeseriesLinks( final NaModell naModel, final IStatusCollector log ) throws Exception
  {
    final IStatusCollector localLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    visitModel( naModel, new FixDotDotTimeseriesVisitor(), localLog );

    final IStatus status = localLog.asMultiStatus( "Anpassen der Zeitreihenreferenzen" );
    log.add( status );
  }

  private CatchmentModelBuilder guessCatchmentModel( final NaModell naModel )
  {
    final ICatchmentModel catchmentModel = m_globalData.getCatchmentModel();
    if( catchmentModel == null )
      return null;

    final TimeseriesIndex timeseriesIndex = m_globalData.getTimeseriesIndex();

    final CatchmentModelBuilder builder = new CatchmentModelBuilder( naModel, catchmentModel, m_targetCalcCaseDir, timeseriesIndex );

    gueCatchmentModel( builder, Catchment.PROP_PRECIPITATION_LINK, ITimeseriesConstants.TYPE_RAINFALL );
    gueCatchmentModel( builder, Catchment.PROP_EVAPORATION_LINK, ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED );
    gueCatchmentModel( builder, Catchment.PROP_TEMPERATURE_LINK, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE );

    return builder;
  }

  private void gueCatchmentModel( final CatchmentModelBuilder builder, final QName propLink, final String parameterType )
  {
    final IStatus status = builder.execute( propLink, parameterType );
    getLog().add( status );
  }

  private NAControl addSimulation( final NAControl newControl, final CatchmentModelBuilder catchmentModelBuilder )
  {
    try
    {
      if( catchmentModelBuilder != null )
      {
        final String cmRefN = catchmentModelBuilder.getGeneratorPath( ITimeseriesConstants.TYPE_RAINFALL );
        newControl.setGeneratorReferenceN( cmRefN );

        final String cmRefE = catchmentModelBuilder.getGeneratorPath( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED );
        newControl.setGeneratorReferenceE( cmRefE );

        final String cmRefT = catchmentModelBuilder.getGeneratorPath( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE );
        newControl.setGeneratorReferenceT( cmRefT );
      }

      final SimulationCollection simulations = m_globalData.getSimulations();
      final NAControl simulation = simulations.getSimulations().addNew( NAControl.FEATURE_NACONTROL );
      FeatureHelper.copyData( newControl, simulation );
      simulation.setDescription( m_targetCalcCaseDir.getName() );

      return simulation;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      getLog().add( IStatus.WARNING, Messages.getString( "CalcCaseConverter.10" ), e ); //$NON-NLS-1$
      return null;
    }
  }

  public static IParameterTypeIndex collectTimeseriesParameterTypes( final NaModell naModel, final File sourceModelDir )
  {
    final IStatusCollector localLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final ParameterTypeIndexVisitor visitor = new ParameterTypeIndexVisitor( sourceModelDir );
    visitModel( naModel, visitor, localLog );

    return visitor;
  }

  private static void visitModel( final NaModell naModel, final ITimeseriesVisitor visitor, final IStatusCollector log )
  {
    final TimeseriesWalker walker = new TimeseriesWalker( visitor, log );
    naModel.getWorkspace().accept( walker, naModel, FeatureVisitor.DEPTH_INFINITE );
  }
}