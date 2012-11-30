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
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.model.hydrology.binding.InitialValue;
import org.kalypso.model.hydrology.binding._11_6.InitialValues;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
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
 * @author Holger Albert
 */
public class CalcCaseConverter extends AbstractLoggingOperation
{
  private final String DOT_CALCULATION = ".calculation"; //$NON-NLS-1$

  private final String CALC_CASE = "calcCase.gml"; //$NON-NLS-1$

  private final String CALC_HYDROTOP = "calcHydrotop.gml"; //$NON-NLS-1$

  private final String CALC_PARAMETER = "calcParameter.gml"; //$NON-NLS-1$

  /**
   * The directory of the source calc case.
   */
  private final File m_sourceCalcCaseDir;

  /**
   * The directory of the target scenario.
   */
  private final File m_targetScenarioDir;

  /**
   * The global conversion data.
   */
  private final GlobalConversionData m_globalData;

  /**
   * The converter data.
   */
  private final ConverterData m_data;

  private final String m_simulationPath;

  /**
   * @param sourceCalcCaseDir
   *          The directory of the source calc case.
   * @param targetScenarioDir
   *          The directory of the target scenario.
   * @param globalData
   *          The global conversion data.
   */
  public CalcCaseConverter( final File sourceCalcCaseDir, final File targetScenarioDir, final GlobalConversionData globalData )
  {
    super( sourceCalcCaseDir.getName() );

    m_sourceCalcCaseDir = sourceCalcCaseDir;
    m_targetScenarioDir = targetScenarioDir;
    m_globalData = globalData;
    m_data = new ConverterData( m_targetScenarioDir );
    m_simulationPath = RrmScenario.FOLDER_SIMULATIONEN + "/" + sourceCalcCaseDir.getName(); //$NON-NLS-1$
  }

  @Override
  protected void doExecute( IProgressMonitor monitor ) throws Exception
  {
    /* If no monitor is given, take a null progress monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( String.format( Messages.getString( "CalcCaseConverter.12" ), m_sourceCalcCaseDir.getName() ), 1000 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "CalcCaseConverter.13" ) ); //$NON-NLS-1$

      /* Create the scenario. */
      createScenario();

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "CalcCaseConverter.14" ) ); //$NON-NLS-1$

      /* Copy the basic data. */
      copyBasicData();

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "CalcCaseConverter.15" ) ); //$NON-NLS-1$

      /* Rename the old results. */
      renameOldResults();

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "CalcCaseConverter.16" ) ); //$NON-NLS-1$

      /* Convert old meta control. */
      final boolean hasSynth = convertMetaControl();

      /* Convert old expert control. */
      convertExpertControl();

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( Messages.getString( "CalcCaseConverter.17" ) ); //$NON-NLS-1$

      /* Build the paths. */
      final String modelPath = RrmScenario.FOLDER_MODELS + '/' + RrmScenario.FILE_MODELL_GML;
      final String catchmentModelPath = RrmScenario.FOLDER_MODELS + '/' + RrmScenario.FILE_CATCHMENT_MODELS_GML;
      final String timeseriesMappingPath = RrmScenario.FOLDER_MODELS + '/' + RrmScenario.FILE_TIMESERIES_MAPPINGS_GML;

      /* Load the models. */
      final NaModell naModel = m_data.loadModel( modelPath );
      final ICatchmentModel catchmentModel = m_data.loadModel( catchmentModelPath );
      final ITimeseriesMappingCollection mappings = m_data.loadModel( timeseriesMappingPath );

      /* Fix timeseries link: relative to simulations model folder, and new sub directories */
      fixTimeseriesLinks( naModel, getLog() );

      /* Do the timeseries mappings. */
      final IStatusCollector mappingLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      final CatchmentModelBuilder catchmentModelBuilder = guessCatchmentModel( hasSynth, naModel, catchmentModel, mappingLog );
      final TimeseriesMappingBuilder timeseriesMappingBuilder = guessTimeseriesMappings( hasSynth, naModel, mappings, mappingLog );

      getLog().add( mappingLog.asMultiStatus( Messages.getString( "CalcCaseConverter.18" ) ) ); //$NON-NLS-1$

      /* IMPORTANT: Update the categories before the links have been fixed. */
      naModel.getNodes().accept( new UpdateResultCategoriesVisitor() );

      /* SPECIAL CASE: Must save and copy the modell.gml before emptying the timeseries links. */
      final String simulationModelPath = RrmSimulation.FOLDER_MODELS + '/' + RrmSimulation.FILE_MODELL_GML;
      m_data.saveModel( m_simulationPath + '/' + simulationModelPath, naModel );

      /* Empty timeseries links. */
      BasicModelConverter.emptyTimeseriesLinks( naModel, getLog() );

      /* Save the models. */
      m_data.saveModel( modelPath, naModel );
      m_data.saveModel( catchmentModelPath, catchmentModel );
      m_data.saveModel( timeseriesMappingPath, mappings );

      /* Dispose them, because of the links. */
      naModel.getWorkspace().dispose();
      catchmentModel.getWorkspace().dispose();
      mappings.getWorkspace().dispose();

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "CalcCaseConverter.20" ) ); //$NON-NLS-1$

      /* Add the simulation. */
      final NAControl simulation = addSimulation( catchmentModelBuilder, timeseriesMappingBuilder );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "CalcCaseConverter.21" ) ); //$NON-NLS-1$

      /* Finalize the simulation. */
      finalizeSimulation();

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "CalcCaseConverter.22" ) ); //$NON-NLS-1$

      /* Verify timeseries of the catchment models of the created simulation. */
      final CatchmentModelVerifier verifier = new CatchmentModelVerifier( hasSynth, m_data, simulation, new File( m_targetScenarioDir, RrmScenario.FOLDER_SIMULATIONEN ) );
      final IStatus verifierStatus = verifier.execute();
      getLog().add( verifierStatus );

      /* Monitor. */
      monitor.worked( 200 );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function creates the scenario.
   */
  private void createScenario( ) throws IOException, JAXBException
  {
    /* Create the directory. */
    m_targetScenarioDir.mkdirs();

    /* Copy all data from the base scenario except the "Szenarien" folder. */
    FileUtils.copyDirectory( m_globalData.getBaseScenarioDir(), m_targetScenarioDir, new ScenariosExclusionFileFilter() );

    /* Update the cases file. */
    m_globalData.updateCasesFile( m_targetScenarioDir );
  }

  /**
   * This function copies the basic data.
   */
  private void copyBasicData( ) throws IOException
  {
    getLog().add( IStatus.OK, Messages.getString( "CalcCaseConverter_0" ) ); //$NON-NLS-1$

    final IPath modelsPath = new Path( RrmScenario.FOLDER_MODELS );

    final String calculationGmlPath = getCaclulationGmlPath();

    /* Copy gml files into the .models folder of the scenario. */
    copyFile( DOT_CALCULATION, calculationGmlPath );
    copyFile( CALC_CASE, modelsPath.append( RrmScenario.FILE_MODELL_GML ).toOSString() );
    final File hydrotope = copyFile( CALC_HYDROTOP, modelsPath.append( RrmScenario.FILE_HYDROTOP_GML ).toOSString() );
    copyFile( CALC_PARAMETER, modelsPath.append( RrmScenario.FILE_PARAMETER_GML ).toOSString() );
    copyFile( INaCalcCaseConstants.EXPERT_CONTROL_FILE, modelsPath.append( RrmScenario.FILE_EXPERT_CONTROL_GML ).toOSString() );

    /* This file may not exist. */
    final File calcSynthN = new File( m_sourceCalcCaseDir, INaCalcCaseConstants.NIEDERSCHLAG_DIR + "/calcSynthN.gml" ); //$NON-NLS-1$
    if( calcSynthN.exists() )
      copyFile( INaCalcCaseConstants.NIEDERSCHLAG_DIR + "/calcSynthN.gml", modelsPath.append( RrmScenario.FILE_SYNTHN_GML ).toOSString() ); //$NON-NLS-1$

    /* Convert the hydrotopes. */
    final ConvertHydrotopesOperation convertHydrotopesOperation = new ConvertHydrotopesOperation( hydrotope );
    final IStatus converteHydrotopesStatus = convertHydrotopesOperation.execute( new NullProgressMonitor() );
    getLog().add( converteHydrotopesStatus );

    /* Copy special directories into the calccases/calccase folder of the scenario. */
    copyDir( INaCalcCaseConstants.ANFANGSWERTE_DIR, m_simulationPath + '/' + RrmSimulation.FOLDER_ANFANGSWERTE, false );
    copyDir( INaCalcCaseConstants.ERGEBNISSE_DIR, m_simulationPath + '/' + RrmSimulation.FOLDER_RESULTS, false );
    copyDir( INaCalcCaseConstants.KLIMA_DIR, m_simulationPath + '/' + RrmSimulation.FOLDER_KLIMA, false );
    copyDir( INaCalcCaseConstants.NIEDERSCHLAG_DIR, m_simulationPath + '/' + RrmSimulation.FOLDER_NIEDERSCHLAG, false );
    copyDir( INaCalcCaseConstants.PEGEL_DIR, m_simulationPath + '/' + RrmSimulation.FOLDER_PEGEL, true );
    copyDir( INaCalcCaseConstants.ZUFLUSS_DIR, m_simulationPath + '/' + RrmSimulation.FOLDER_ZUFLUSS, true );
  }

  private String getCaclulationGmlPath( )
  {
    return m_simulationPath + '/' + RrmSimulation.FOLDER_MODELS + '/' + RrmSimulation.FILE_CALCULATION_GML;
  }

  /**
   * This function renames the old results.
   */
  private void renameOldResults( )
  {
    final File resultDir = new File( m_targetScenarioDir, m_simulationPath + '/' + RrmSimulation.FOLDER_RESULTS );
    final File[] subFolders = resultDir.listFiles();
    for( final File subFolder : subFolders )
    {
      if( subFolder.isDirectory() )
      {
        final String name = subFolder.getName();

        if( "Logs".equals( name ) ) //$NON-NLS-1$
        {
          FileUtils.deleteQuietly( subFolder );
          continue;
        }

        /* Rename old german sub folder names */
        renameSubFolder( subFolder, "Anfangswerte", RrmCalculationResult.FOLDER_INITIAL_VALUES ); //$NON-NLS-1$
        renameSubFolder( subFolder, "Bilanz/Bilanz.txt", RrmCalculationResult.FOLDER_REPORT + '/' + RrmCalculationResult.FILE_BILANZ_TXT ); //$NON-NLS-1$
        renameSubFolder( subFolder, "Knoten", RrmCalculationResult.FOLDER_NODE ); //$NON-NLS-1$
        renameSubFolder( subFolder, "Teilgebiet", RrmCalculationResult.FOLDER_CATCHMENT ); //$NON-NLS-1$
        renameSubFolder( subFolder, "Speicherstrang", RrmCalculationResult.FOLDER_STROAGE_CHANNEL ); //$NON-NLS-1$

        /* Rename last current dir, so it is not automatically overwritten on next calculation */
        if( name.equals( "Aktuell" ) ) //$NON-NLS-1$
        {
          final File origCurrentResultDir = new File( resultDir, Messages.getString( "CalcCaseConverter.0" ) ); //$NON-NLS-1$
          subFolder.renameTo( origCurrentResultDir );

          final String msg = String.format( Messages.getString( "CalcCaseConverter.1" ), name, origCurrentResultDir.getName() ); //$NON-NLS-1$
          getLog().add( IStatus.INFO, msg );
        }
      }
    }
  }

  private void renameSubFolder( final File parent, final String oldName, final String newName )
  {
    final File oldFile = new File( parent, oldName );
    final File newFile = new File( parent, newName );
    if( oldFile.exists() )
      oldFile.renameTo( newFile );
  }

  private File copyFile( final String sourcePath, final String targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceCalcCaseDir, sourcePath );
    final File modelTargetFile = new File( m_targetScenarioDir, targetPath );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );

    return modelTargetFile;
  }

  private void copyDir( final String sourcePath, final String targetPath, final boolean createEmpty ) throws IOException
  {
    final File modelSourceDir = new File( m_sourceCalcCaseDir, sourcePath );
    final File modelTargetDir = new File( m_targetScenarioDir, targetPath );

    if( modelSourceDir.isDirectory() )
    {
      FileUtils.copyDirectory( modelSourceDir, modelTargetDir, true );
      return;
    }

    if( createEmpty )
    {
      getLog().add( IStatus.INFO, String.format( Messages.getString( "CalcCaseConverter.23" ), sourcePath ) ); //$NON-NLS-1$
      modelTargetDir.mkdirs();
      return;
    }
  }

  /**
   * This function converts the old meta control to the new meta control. It saves the new meta control to the file of
   * the old meta control.
   * 
   * @return True, if this simulation uses design rainfall.
   */
  private boolean convertMetaControl( ) throws Exception
  {
    final String calculationGmlPath = getCaclulationGmlPath();

    /* Load the old meta control. */
    final org.kalypso.model.hydrology.binding._11_6.NAControl oldControl = m_data.loadModel( calculationGmlPath );

    /* Convert the old meta control to the new meta control. */
    final NAControl newControl = convertMetaControl( oldControl );

    /* tweaks start and end time */
    tweakStartTime( newControl );

    /* Save the new meta control, overwriting the file with the old meta control. */
    m_data.saveModel( calculationGmlPath, newControl );

    /* Has this simulation design rainfall? */
    final boolean usePrecipitationForm = newControl.isUsePrecipitationForm();

    /* Dispose the workspace. */
    newControl.getWorkspace().dispose();

    return usePrecipitationForm;
  }

  /**
   * This function converts the old meta control to the new meta control.
   * 
   * @param oldControl
   *          The old meta control.
   * @return The new meta control.
   */
  private NAControl convertMetaControl( final org.kalypso.model.hydrology.binding._11_6.NAControl oldControl ) throws GMLSchemaException
  {
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( NAControl.FEATURE_NACONTROL, null, null );
    final NAControl newControl = (NAControl)workspace.getRootFeature();

    /* Metadata. */
    newControl.setDescription( oldControl.getDescription2() );
    newControl.setEditor( oldControl.getEditor() );
    newControl.setComment( oldControl.getComment() );
    newControl.setCreationTime( oldControl.getCalcTime() );

    /* Control parameters. */
    newControl.setSimulationStart( oldControl.getSimulationStart() );
    newControl.setSimulationEnd( oldControl.getSimulationEnd() );
    newControl.setMinutesOfTimestep( oldControl.getMinutesOfTimestep() );
    newControl.setExeVersion( oldControl.getExeVersion() );
    newControl.setReturnPeriod( oldControl.getReturnPeriod() );

    /* Synthetic precipitation. */
    newControl.setUsePrecipitationForm( oldControl.isUsePrecipitationForm() );
    newControl.setProperty( NAControl.PROP_XJAH, oldControl.getProperty( org.kalypso.model.hydrology.binding._11_6.NAControl.PROP_XJAH ) );
    newControl.setDurationMinutes( oldControl.getDurationMinutes() );
    newControl.setPrecipitationForm( oldControl.getPrecipitationForm() );

    getLog().add( IStatus.OK, Messages.getString( "CalcCaseConverter.8" ) ); //$NON-NLS-1$

    return newControl;
  }

  /**
   * This function converts the old expert control to the new expert control. It saves the new expert control to the
   * file of the old expert control.
   */
  private void convertExpertControl( ) throws Exception
  {
    final String exportControlPath = RrmSimulation.FOLDER_MODELS + '/' + RrmSimulation.FILE_EXPERT_CONTROL_GML;

    /* Load the old expert control. */
    final org.kalypso.model.hydrology.binding._11_6.NAModellControl oldControl = m_data.loadModel( exportControlPath );

    /* Convert the old expert control to the new expert control. */
    final NAModellControl newControl = convertExpertControl( oldControl );

    /* Save the new expert control, overwriting the file with the old expert control. */
    m_data.saveModel( exportControlPath, newControl );
  }

  /**
   * This function converts the old expert control to the new expert control.
   * 
   * @param oldControl
   *          The old expert control.
   * @return The new expert control.
   */
  private NAModellControl convertExpertControl( final org.kalypso.model.hydrology.binding._11_6.NAModellControl oldControl ) throws GMLSchemaException
  {
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( NAModellControl.FEATURE_NA_MODELL_CONTROL, null, null );
    final NAModellControl newControl = (NAModellControl)workspace.getRootFeature();

    /* Copy all boolean values that exists in both models. */
    final IFeatureType oldType = oldControl.getFeatureType();
    final IFeatureType newType = newControl.getFeatureType();

    final IPropertyType[] oldTypes = oldType.getProperties();
    for( final IPropertyType oldProperty : oldTypes )
    {
      final IPropertyType newProperty = newType.getProperty( new QName( NAModellControl.NS_NACONTROL, oldProperty.getQName().getLocalPart() ) );
      if( oldProperty instanceof IValuePropertyType && newProperty instanceof IValuePropertyType )
      {
        if( ((IValuePropertyType)oldProperty).getValueClass() == Boolean.class )
        {
          final Object oldValue = oldControl.getProperty( oldProperty );
          newControl.setProperty( newProperty, oldValue );
        }
      }
    }

    /* Copy initial values. */
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

  private CatchmentModelBuilder guessCatchmentModel( final boolean hasSynth, final NaModell naModel, final ICatchmentModel catchmentModel, final IStatusCollector mappingLog ) throws Exception
  {
    /* If there is no model, return null. */
    if( catchmentModel == null )
      return null;

    /* Create the builder. */
    final CatchmentModelBuilder builder = new CatchmentModelBuilder( naModel, catchmentModel, new File( m_targetScenarioDir, m_simulationPath ), m_globalData );

    /* If there is no design rainfall, create the catchment models. */
    if( !hasSynth )
    {
      guessCatchmentModel( builder, Catchment.PROP_PRECIPITATION_LINK, ITimeseriesConstants.TYPE_RAINFALL, mappingLog );
      guessCatchmentModel( builder, Catchment.PROP_EVAPORATION_LINK, ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED, mappingLog );
      guessCatchmentModel( builder, Catchment.PROP_TEMPERATURE_LINK, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE, mappingLog );
    }

    return builder;
  }

  private void guessCatchmentModel( final CatchmentModelBuilder builder, final QName propLink, final String parameterType, final IStatusCollector mappingLog )
  {
    final IStatus status = builder.execute( propLink, parameterType );
    mappingLog.add( status );
  }

  private TimeseriesMappingBuilder guessTimeseriesMappings( final boolean hasSynth, final NaModell naModel, final ITimeseriesMappingCollection mappings, final IStatusCollector mappingLog ) throws Exception
  {
    /* If there is no model, return null. */
    if( mappings == null )
      return null;

    /* Create the builder. */
    final TimeseriesMappingBuilder builder = new TimeseriesMappingBuilder( m_globalData, naModel, mappings, new File( m_targetScenarioDir, m_simulationPath ) );

    /* Always create the node inflow timeseries mapping. */
    guessTimeseriesMapping( builder, TimeseriesMappingType.nodeInflow, mappingLog );

    /* If there is no design rainfall, create the other timeseries mappings. */
    if( !hasSynth )
    {
      guessTimeseriesMapping( builder, TimeseriesMappingType.gaugeMeasurement, mappingLog );
      guessTimeseriesMapping( builder, TimeseriesMappingType.storageEvaporation, mappingLog );
    }

    return builder;
  }

  private void guessTimeseriesMapping( final TimeseriesMappingBuilder builder, final TimeseriesMappingType mappingType, final IStatusCollector mappingLog )
  {
    final IStatus status = builder.execute( mappingType );
    mappingLog.add( status );
  }

  private NAControl addSimulation( final CatchmentModelBuilder catchmentModelBuilder, final TimeseriesMappingBuilder timeseriesMappingBuilder )
  {
    try
    {
      final String calculationGmlPath = getCaclulationGmlPath();

      final NAControl metaControl = m_data.loadModel( calculationGmlPath );

      /* Adjust with the catchments model builder. */
      if( catchmentModelBuilder != null )
      {
        final String cmRefN = catchmentModelBuilder.getGeneratorPath( ITimeseriesConstants.TYPE_RAINFALL );
        metaControl.setGeneratorReferenceN( cmRefN );

        final String cmRefE = catchmentModelBuilder.getGeneratorPath( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED );
        metaControl.setGeneratorReferenceE( cmRefE );

        final String cmRefT = catchmentModelBuilder.getGeneratorPath( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE );
        metaControl.setGeneratorReferenceT( cmRefT );
      }

      /* Adjust with the timeseries mapping builder. */
      if( timeseriesMappingBuilder != null )
      {
        final String tmRefGauge = timeseriesMappingBuilder.getMappingPath( TimeseriesMappingType.gaugeMeasurement );
        metaControl.setMappingReferenceGauge( tmRefGauge );

        final String tmRefInflow = timeseriesMappingBuilder.getMappingPath( TimeseriesMappingType.nodeInflow );
        metaControl.setMappingReferenceNodeInflow( tmRefInflow );

        final String tmRefStorageEvaporation = timeseriesMappingBuilder.getMappingPath( TimeseriesMappingType.storageEvaporation );
        metaControl.setMappingReferenceStorageEvaporation( tmRefStorageEvaporation );
      }

      /* Save the meta control. */
      m_data.saveModel( calculationGmlPath, metaControl );

      /* Load the simulations. */
      final String simulationsPath = RrmScenario.FOLDER_MODELS + '/' + RrmScenario.FILE_SIMULATIONS_GML; //$NON-NLS-1$

      final SimulationCollection simulations = m_data.loadModel( simulationsPath );

      /* Add the meta control. */
      final NAControl simulation = simulations.getSimulations().addNew( NAControl.FEATURE_NACONTROL );
      FeatureHelper.copyData( metaControl, simulation );
      simulation.setDescription( m_sourceCalcCaseDir.getName() );

      /* Save the simulations. */
      m_data.saveModel( simulationsPath, simulations );

      return simulation;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      getLog().add( IStatus.WARNING, Messages.getString( "CalcCaseConverter.10" ), e ); //$NON-NLS-1$
      return null;
    }
  }

  /**
   * This function finalizes a simulation. It copies some files for reference and fixes links in one of them.
   */
  private void finalizeSimulation( ) throws IOException, Exception
  {
    /* Copy the expertMappings.gml into the simulation for reference. */
    final File exportControlScenario = new File( m_targetScenarioDir, RrmScenario.FOLDER_MODELS + '/' + RrmScenario.FILE_EXPERT_CONTROL_GML );
    final File exportControlSimulation = new File( m_targetScenarioDir, m_simulationPath + '/' + RrmSimulation.FOLDER_MODELS + '/' + RrmSimulation.FILE_EXPERT_CONTROL_GML );
    FileUtils.copyFile( exportControlScenario, exportControlSimulation, true ); //$NON-NLS-1$

    /* The calculation.gml is already saved there. */
    /* The model.gml is already saved there, because it must be copied before its timeseries links are emptied. */
  }

  private void fixTimeseriesLinks( final NaModell naModel, final IStatusCollector log ) throws Exception
  {
    final IStatusCollector localLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final TimeseriesWalker walker = new TimeseriesWalker( new FixDotDotTimeseriesVisitor(), localLog );
    naModel.getWorkspace().accept( walker, naModel, FeatureVisitor.DEPTH_INFINITE );

    final IStatus status = localLog.asMultiStatus( Messages.getString( "CalcCaseConverter.28" ) ); //$NON-NLS-1$
    log.add( status );
  }

  private void tweakStartTime( final NAControl metaControl )
  {
    /* The simulation time is irrelevant for syntetic events -> no need for correction */
    if( metaControl.isUsePrecipitationForm() )
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
      final String statusMsg = String.format( Messages.getString( "CalcCaseConverter.2" ), simulationStartText ); //$NON-NLS-1$
      getLog().add( IStatus.WARNING, statusMsg, null ); //$NON-NLS-1$
    }

    /* Tweak simulation end */
    final Date simulationEnd = metaControl.getSimulationEnd();
    final Date newSimulationEnd = tweakStartDate( simulationEnd );
    if( newSimulationEnd != null )
    {
      metaControl.setSimulationEnd( newSimulationEnd );
      final String simulationEndText = displayDateTimeFormat.format( simulationEnd );
      final String statusMsg = String.format( "Simulationsende der Rechenvariante muss 00:00 Uhr sein. Zeit (%s) wurde automatisch angepasst.", simulationEndText );
      getLog().add( IStatus.WARNING, statusMsg, null ); //$NON-NLS-1$
    }

//    /* Tweak start condition */
//    final InitialValues initialValues = m_data.getInitialValues();
//    if( initialValues == null )
//      return;
//
//    final Date initialDate = initialValues.getInitialDate();
//    final Date newInitialDate = tweakStartDate( initialDate );
//    if( newInitialDate != null )
//    {
//      initialValues.setInitialDate( newInitialDate );
//      final String initialValuesText = displayDateTimeFormat.format( initialDate );
//      final String statusMsg = String.format( Messages.getString( "CalcCaseConverter.3" ), initialValuesText ); //$NON-NLS-1$
//      final int severity = IStatus.WARNING;
//      saveModel( initialValues, INaCalcCaseConstants.ANFANGSWERTE_FILE, severity, statusMsg );
//    }
  }

  private Date tweakStartDate( final Date oldDate )
  {
    if( oldDate == null )
    {
      getLog().add( IStatus.WARNING, "Starting date is not set. Check calculation case." );
      return null;
    }

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
}