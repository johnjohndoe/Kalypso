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
package org.kalypso.ui.rrm.internal.calccase;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.Date;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.logging.SystemOutLogger;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.rcm.IRainfallModelProvider;
import org.kalypso.model.rcm.RainfallGenerationOperation;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypso.model.rcm.util.PlainRainfallModelProvider;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.merged.Source;
import org.kalypso.simulation.core.ant.copyobservation.CopyObservationFeatureVisitor;
import org.kalypso.simulation.core.ant.copyobservation.ICopyObservationSource;
import org.kalypso.simulation.core.ant.copyobservation.source.FeatureCopyObservationSource;
import org.kalypso.simulation.core.ant.copyobservation.target.CopyObservationTargetFactory;
import org.kalypso.simulation.core.ant.copyobservation.target.ICopyObservationTarget;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.visitors.MonitorFeatureVisitor;

/**
 * @author Gernot Belger
 */
public class UpdateCalcCaseOperation extends WorkspaceModifyOperation
{
  final QName MAPPING_MEMBER = new QName( "http://org.kalypso.updateObservationMapping", "mappingMember" );

  private final IFolder[] m_calcCases;

  public UpdateCalcCaseOperation( final IFolder[] calcCases )
  {
    // Find least common container and use as scheduling rule

    m_calcCases = calcCases;
  }

  @Override
  protected void execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Updating calc case(s)", m_calcCases.length );

    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    for( final IFolder calcCaseFolder : m_calcCases )
    {
      try
      {
        final IStatus status = updateCalcCase( calcCaseFolder, new SubProgressMonitor( monitor, 1 ) );
        collector.add( status );
      }
      catch( final CoreException e )
      {
        collector.add( e.getStatus() );
      }

      /* Check for cancel */
      ProgressUtilities.worked( monitor, 0 );
    }

    final IStatus resultStatus = collector.asMultiStatusOrOK( "Problem(s) occured during update" );
    if( !resultStatus.isOK() )
      throw new CoreException( resultStatus );
  }

  /**
   * <pre>
   *     <target name="updateOmbrometer" depends="setProperties">
   *         <!-- updateOmbrometer: ombrometer von Datacenter (oder lokal)-abholen -->
   *         <echo message="aktualisiere Ombrometer Zeitreihen im Rechenfall" />
   *         <delete dir="${calc.dir}/Niederschlag" />
   *         <mkdir dir="${calc.dir}/Niederschlag" />
   *         <kalypso.copyObservation gml="${project.url}/.model/observationConf/ombrometer.gml" featurePath="ombrometerMember" context="${calc.url}" targetobservationdir="${calc.dir}/Niederschlag" >
   *             <source property="NRepository" from="${startsim}" to="${stopsim}" />
   *         </kalypso.copyObservation>
   *     </target>
   *
   *     <target name="updateObsT" depends="setProperties">
   *         <echo message="aktualisiere Temperaturen (Messung)" />
   *         <delete dir="${calc.dir}/Klima" />
   *         <mkdir dir="${calc.dir}/Klima" />
   *         <kalypso.copyObservation gml="${project.url}/.model/observationConf/ObsTMapping.gml" featurePath="mappingMember" context="${calc.url}" targetobservationdir="${calc.dir}/Klima">
   *             <source property="inObservationLink" from="${startsim}" to="${stopsim}" />
   *         </kalypso.copyObservation>
   *     </target>
   *
   * </pre>
   */
  private IStatus updateCalcCase( final IFolder calcCaseFolder, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Updating calc case", 120 );

    /* Read models */
    monitor.subTask( "Reading control file..." );
    final NAControl control = readControlFile( calcCaseFolder );
    final NaModell model = readModelFile( calcCaseFolder );
    ProgressUtilities.worked( monitor, 20 );

    final DateRange range = getRange( control );

    /* Copy observations for pegel and zufluss */
    copyPegelTimeseries( calcCaseFolder, range, new SubProgressMonitor( monitor, 20 ) );

    copyZuflussTimeseries( calcCaseFolder, range, new SubProgressMonitor( monitor, 20 ) );

    /* Execute catchment models */
    copyEvaporationTimeseries( calcCaseFolder, range, new SubProgressMonitor( monitor, 20 ) );

    // FIXME
    // executeCatchmentModel( model, control.getGeneratorN(), Catchment.PROP_PRECIPITATION_LINK, range );
    // executeCatchmentModel( model, control.getGeneratorT(), Catchment.PROP_TEMPERATURE_LINK, range );
    // executeCatchmentModel( model, control.getGeneratorE(), Catchment.PROP_EVAPORATION_LINK, range );

    return Status.OK_STATUS;
  }

  private DateRange getRange( final NAControl control )
  {
    final Date simulationStart = control.getSimulationStart();
    final Date simulationEnd = control.getSimulationEnd();

    return new DateRange( simulationStart, simulationEnd );
  }

  private NAControl readControlFile( final IFolder calcCaseFolder ) throws CoreException
  {
    try
    {
      final IFile controlFile = calcCaseFolder.getFile( INaCalcCaseConstants.CALCULATION_GML_PATH );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlFile );
      return (NAControl) controlWorkspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to read control file", e );
      throw new CoreException( status );
    }
  }

  private NaModell readModelFile( final IFolder calcCaseFolder ) throws CoreException
  {
    try
    {
      final IFile controlFile = calcCaseFolder.getFile( INaProjectConstants.GML_MODELL_PATH );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlFile );
      return (NaModell) controlWorkspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to read model file", e );
      throw new CoreException( status );
    }
  }

  private void copyPegelTimeseries( final IFolder calcCaseFolder, final DateRange sourceRange, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      /* Read mapping */
      final FeatureList mappingFeatures = readMapping( calcCaseFolder, "ObsQMapping.gml", MAPPING_MEMBER );

      /* Prepare visitor */
      final CopyObservationFeatureVisitor visitor = prepareVisitor( calcCaseFolder, "Pegel", "inObservationLink", sourceRange ); //$NON-NLS-1$ //$NON-NLS-2$

      /* Execute visitor */
      final int count = mappingFeatures.size();
      final MonitorFeatureVisitor wrappedVisitor = new MonitorFeatureVisitor( monitor, count, visitor );
      mappingFeatures.accept( wrappedVisitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to execute pegel mapping", e );
      throw new CoreException( status );
    }
  }

  private void copyZuflussTimeseries( final IFolder calcCaseFolder, final DateRange sourceRange, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      /* Read mapping */
      final FeatureList mappingFeatures = readMapping( calcCaseFolder, "ObsQZuMapping.gml", MAPPING_MEMBER );

      /* Prepare visitor */
      final CopyObservationFeatureVisitor visitor = prepareVisitor( calcCaseFolder, "Zufluss", "inObservationLink", sourceRange ); //$NON-NLS-1$ //$NON-NLS-2$

      /* Execute visitor */
      final int count = mappingFeatures.size();
      final MonitorFeatureVisitor wrappedVisitor = new MonitorFeatureVisitor( monitor, count, visitor );
      mappingFeatures.accept( wrappedVisitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to execute zufluss mapping", e );
      throw new CoreException( status );
    }
  }

  private void copyEvaporationTimeseries( final IFolder calcCaseFolder, final DateRange sourceRange, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      /* Read mapping */
      final FeatureList mappingFeatures = readMapping( calcCaseFolder, "ObsEMapping.gml", MAPPING_MEMBER );

      /* Prepare visitor */
      final CopyObservationFeatureVisitor visitor = prepareVisitor( calcCaseFolder, "Klima", "inObservationLink", sourceRange ); //$NON-NLS-1$ //$NON-NLS-2$

      /* Execute visitor */
      final int count = mappingFeatures.size();
      final MonitorFeatureVisitor wrappedVisitor = new MonitorFeatureVisitor( monitor, count, visitor );
      mappingFeatures.accept( wrappedVisitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to execute zufluss mapping", e );
      throw new CoreException( status );
    }
  }

  private FeatureList readMapping( final IFolder calcCaseFolder, final String filename, final QName memberProperty ) throws Exception
  {
    final IProject project = calcCaseFolder.getProject();
    final IFolder observationConfFolder = project.getFolder( INaProjectConstants.FOLDER_OBSERVATION_CONF );
    final IFile mappingFile = observationConfFolder.getFile( filename );
    final GMLWorkspace mappingWorkspace = GmlSerializer.createGMLWorkspace( mappingFile );
    final Feature rootFeature = mappingWorkspace.getRootFeature();
    return (FeatureList) rootFeature.getProperty( memberProperty );
  }

  private CopyObservationFeatureVisitor prepareVisitor( final IFolder calcCaseFolder, final String outputDir, final String sourceLinkName, final DateRange sourceRange )
  {
    final DateRange forecastRange = null;

    final Source source = new Source();
    source.setProperty( sourceLinkName ); //$NON-NLS-1$
    source.setFrom( Long.toString( sourceRange.getFrom().getTime() ) );
    source.setTo( Long.toString( sourceRange.getTo().getTime() ) );

    final GMLXPath targetPath = null;

    final File calcDir = calcCaseFolder.getLocation().toFile();
    final File targetObservationDir = new File( calcDir, outputDir ); //$NON-NLS-1$

    final URL context = ResourceUtilities.createQuietURL( calcCaseFolder );
    final String tokens = StringUtils.EMPTY;

    final ICopyObservationTarget obsTarget = CopyObservationTargetFactory.getLink( context, targetPath, targetObservationDir, sourceRange, forecastRange );
    final ICopyObservationSource obsSource = new FeatureCopyObservationSource( context, new Source[] { source }, tokens );

    final MetadataList metadata = new MetadataList();

    final SystemOutLogger logger = new SystemOutLogger();

    return new CopyObservationFeatureVisitor( obsSource, obsTarget, metadata, logger );
  }

  private void executeCatchmentModel( final NaModell model, final IRainfallGenerator generator, final QName targetLink, final DateRange range )
  {
    try
    {
      // FIXME: init target links!

      final IRainfallCatchmentModel rainfallModel = createRainfallModel( model, generator, targetLink, range );

      final IRainfallModelProvider modelProvider = new PlainRainfallModelProvider( rainfallModel );

      final RainfallGenerationOperation operation = new RainfallGenerationOperation( modelProvider, null );

      operation.execute( null );
    }
    catch( final InvocationTargetException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private IRainfallCatchmentModel createRainfallModel( final NaModell model, final IRainfallGenerator generator, final QName targetLink, final DateRange targetRange ) throws GMLSchemaException
  {
    final URL context = model.getWorkspace().getContext();
    final GMLWorkspace modelWorkspace = FeatureFactory.createGMLWorkspace( IRainfallCatchmentModel.FEATURE_FAINFALL_CATCHMENT_MODEL, context, null );

    // modelWorkspace.setNamespaceContext();

    final IRainfallCatchmentModel rainfallModel = (IRainfallCatchmentModel) modelWorkspace.getRootFeature();

    rainfallModel.getGenerators().add( generator );

    final ITarget target = rainfallModel.getTarget();

    /* Create link to catchment */
    target.setCatchmentPath( "CatchmentCollectionMember/CatchmentCollection/catchmentMember" );
    final IRelationType catchmentLinkRelation = (IRelationType) target.getFeatureType().getProperty( ITarget.PROPERTY_CATCHMENT_COLLECTION );
    final IFeatureType catchmentLinkType = GMLSchemaUtilities.getFeatureTypeQuiet( Feature.QNAME_FEATURE );
    final String catchmentLinkRef = "modell.gml#root"; //$NON-NLS-1$
    final XLinkedFeature_Impl catchmentXLink = new XLinkedFeature_Impl( target, catchmentLinkRelation, catchmentLinkType, catchmentLinkRef );
    target.setCatchmentFeature( catchmentXLink );

    /* Target range */
    target.setPeriod( targetRange );

    /* Observation path */
    target.setObservationPath( targetLink.getLocalPart() );

    return rainfallModel;
  }
}
