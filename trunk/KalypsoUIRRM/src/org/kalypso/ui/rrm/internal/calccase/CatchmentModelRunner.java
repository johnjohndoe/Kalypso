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
package org.kalypso.ui.rrm.internal.calccase;

import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.poi.ss.formula.eval.NotImplementedException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.IRainfallModelProvider;
import org.kalypso.model.rcm.RainfallGenerationOperation;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypso.model.rcm.util.PlainRainfallModelProvider;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import com.google.common.base.Charsets;

/**
 * This class execute a catchment model.
 * 
 * @author Holger Albert
 */
public class CatchmentModelRunner
{
  /**
   * The constructor.
   */
  public CatchmentModelRunner( )
  {
  }

  /**
   * This function executes the catchment model.
   * 
   * @param simulation
   *          The simulation.
   * @param control
   *          The na control.
   * @param model
   *          The na model.
   * @param generator
   *          The rainfall generator.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   * @param monitor
   *          A progress monitor.
   */
  public void executeCatchmentModel( final RrmSimulation simulation, final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType, final IProgressMonitor monitor ) throws CoreException
  {
    /* The timestep is only defined in linear sum generators for now. */
    if( !(generator instanceof ILinearSumGenerator) )
      throw new NotImplementedException( "Only ILinearSumGenerator's are supported at the moment..." ); //$NON-NLS-1$

    /* Cast. */
    final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) generator;

    try
    {
      monitor.beginTask( Messages.getString( "UpdateSimulationWorker_14" ), 100 ); //$NON-NLS-1$
      final String name = TimeseriesUtils.getName( parameterType );
      monitor.subTask( name );

      /* Calculate date range for filter. */
      final int timestepMinutes = getTimestepMinutes( linearGenerator, control );
      final LocalTime time = linearGenerator.getTimeStamp();
      final Period timestep = Period.minutes( timestepMinutes ).normalizedStandard();

      // TODO
      // Check: We use the timestep of each generator to adjust the range, this is not exactly what happened before.
      final DateRange range = CatchmentModelHelper.getRange( control, timestep, time );

      /* Create the rainfall generation operation. */
      final IRainfallCatchmentModel rainfallModel = createRainfallModel( simulation, model, linearGenerator, targetLink, range );

      /* Initialize the generator. */
      final ILinearSumGenerator clonedGenerator = (ILinearSumGenerator) rainfallModel.getGenerators().get( 0 );
      initGenerator( clonedGenerator, range, timestep, parameterType );

      /* Initialize the catchment target links. */
      initTargetLinks( simulation, clonedGenerator, targetLink, parameterType );

      /* Execute the rainfall generation operation. */
      final IRainfallModelProvider modelProvider = new PlainRainfallModelProvider( rainfallModel );
      final RainfallGenerationOperation operation = new RainfallGenerationOperation( modelProvider, null );

      /* Execute the operation. */
      operation.execute( new SubProgressMonitor( monitor, 100 ) );
    }
    catch( final Exception ex )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "UpdateSimulationWorker_15" ), ex ) ); // $NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();

      try
      {
        /* Refresh the simulation folder. */
        simulation.getSimulationFolder().refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
      }
      catch( final CoreException e )
      {
        /* REMARK: give priority to other exception, so we just system out it. */
        e.printStackTrace();
      }
    }
  }

  private int getTimestepMinutes( final ILinearSumGenerator generator, final NAControl control )
  {
    /* If a timestep in the generator is set, this one is used. */
    final Integer timestep = generator.getTimestep();
    if( timestep == null )
      return control.getMinutesOfTimestep();

    return timestep;
  }

  private void initGenerator( final ILinearSumGenerator generator, final DateRange range, final Period timestep, final String parameterType )
  {
    /* Set the period. */
    generator.setPeriod( range );

    /* Get the calendar field and the amount. */
    final String calendarField = PeriodUtils.findCalendarField( timestep ).name();
    final int amount = PeriodUtils.findCalendarAmount( timestep );

    /* Add a source filter. */
    if( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE.equals( parameterType ) )
      generator.addInterpolationFilter( calendarField, amount, true, "0.0", 0 ); //$NON-NLS-1$
    else
      generator.addIntervalFilter( calendarField, amount, 0.0, 0 );
  }

  private void initTargetLinks( final RrmSimulation simulation, final ILinearSumGenerator generator, final QName targetLink, final String parameterType ) throws Exception
  {
    /* Hash for the already created links. */
    final Map<String, String> linkHash = new HashMap<String, String>();

    /* The workspace to save. */
    GMLWorkspace workspaceToSave = null;

    /* Get the catchments. */
    final List<ICatchment> generatorCatchments = generator.getCatchments();
    for( final ICatchment generatorCatchment : generatorCatchments )
    {
      /* Get the area. */
      final IXLinkedFeature areaLink = (IXLinkedFeature) generatorCatchment.getAreaLink();
      final Catchment catchment = (Catchment) areaLink.getFeature();

      /* Find the workspace to save. */
      if( workspaceToSave == null )
        workspaceToSave = catchment.getWorkspace();

      /* Build the hash. */
      final String hash = RainfallGeneratorUtilities.buildHash( generatorCatchment );

      /* If the link hash contains this hash code, the corresponding link will be used. */
      if( linkHash.containsKey( hash ) )
      {
        /* Get the link. */
        final String link = linkHash.get( hash );

        /* Set the link. */
        setLink( catchment, targetLink, link );
      }
      else
      {
        /* Otherwise create a new link. */
        final String link = buildLink( parameterType, catchment );

        /* Set the link. */
        setLink( catchment, targetLink, link );

        /* Store the hash code. */
        linkHash.put( hash, link );
      }
    }

    /* Save the workspace, because it is reloaded in the rainfall operation. */
    /* HINT: This is the linked workspace of the modell.gml, not the loaded one here. */
    final IFile modelFile = simulation.getModelGml();
    GmlSerializer.saveWorkspace( workspaceToSave, modelFile );
  }

  private String buildLink( final String parameterType, final Catchment catchment ) throws UnsupportedEncodingException
  {
    final String folderName = getTargetLinkFolderName( parameterType );

    return String.format( "../%s/%s_%s.zml", folderName, parameterType, URLEncoder.encode( catchment.getName(), Charsets.UTF_8.name() ) ); //$NON-NLS-1$
  }

  private void setLink( final Catchment catchment, final QName targetLink, final String link )
  {
    /* Create the timeseries link type. */
    final TimeseriesLinkType tsLink = new TimeseriesLinkType();
    tsLink.setHref( link );

    /* Set the property. */
    catchment.setProperty( targetLink, tsLink );
  }

  // FIXME: use CalcCaseAccessor for that?
  private String getTargetLinkFolderName( final String parameterType )
  {
    switch( parameterType )
    {
      case ITimeseriesConstants.TYPE_RAINFALL:
        return Messages.getString( "UpdateSimulationWorker.3" ); // TODO i18n; en = 'Precipitation' //$NON-NLS-1$

      case ITimeseriesConstants.TYPE_MEAN_TEMPERATURE:
      case ITimeseriesConstants.TYPE_MEAN_EVAPORATION:
        return Messages.getString( "UpdateSimulationWorker.4" ); // TODO i18n; en = 'Climate' //$NON-NLS-1$
    }

    throw new IllegalArgumentException();
  }

  private IRainfallCatchmentModel createRainfallModel( final RrmSimulation simulation, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final DateRange targetRange ) throws Exception
  {
    /* Rainfall model. */
    final IFolder modelsFolder = simulation.getModelsFolder();
    final URL context = ResourceUtilities.createQuietURL( modelsFolder );
    final GMLWorkspace modelWorkspace = FeatureFactory.createGMLWorkspace( IRainfallCatchmentModel.FEATURE_FAINFALL_CATCHMENT_MODEL, context, GmlSerializer.DEFAULT_FACTORY );
    final IRainfallCatchmentModel rainfallModel = (IRainfallCatchmentModel) modelWorkspace.getRootFeature();

    /* Add a COPY of the generator into the model, because we are going to change it later */
    final IRelationType generatorsRelation = (IRelationType) rainfallModel.getFeatureType().getProperty( IRainfallCatchmentModel.MEMBER_GENERATOR );
    FeatureHelper.cloneFeature( rainfallModel, generatorsRelation, generator );

    /* Create a target. */
    final IRelationType parentRelation = (IRelationType) rainfallModel.getFeatureType().getProperty( IRainfallCatchmentModel.MEMBER_TARGET );
    final IFeatureType type = GMLSchemaUtilities.getFeatureTypeQuiet( ITarget.FEATURE_TARGET );
    final ITarget target = (ITarget) modelWorkspace.createFeature( rainfallModel, parentRelation, type );
    target.setCatchmentPath( "CatchmentCollectionMember/catchmentMember" ); //$NON-NLS-1$

    /* Set the target. */
    rainfallModel.setTarget( target );

    /* Create the link to the catchment. */
    final String catchmentLinkRef = "modell.gml#" + model.getId(); //$NON-NLS-1$
    target.setCatchmentFeature( catchmentLinkRef );

    /* Target range. */
    target.setPeriod( targetRange );

    /* Observation path. */
    target.setObservationPath( targetLink.getLocalPart() );

    return rainfallModel;
  }
}