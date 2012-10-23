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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.hydrology.util.cm.CatchmentHelper;
import org.kalypso.model.rcm.IRainfallModelProvider;
import org.kalypso.model.rcm.RainfallGenerationOperation;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypso.model.rcm.util.PlainRainfallModelProvider;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * This class executes a catchment model with a linear sum generator.
 *
 * @author Holger Albert
 */
public class LinearSumCatchmentModelRunner extends AbstractCatchmentModelRunner
{
  /**
   * This prefix is used when writing the timeseries.
   */
  private final String m_prefix;

  /**
   * The constructor.
   *
   * @param prefix
   *          This prefix is used when writing the timeseries.
   */
  public LinearSumCatchmentModelRunner( final String prefix )
  {
    m_prefix = prefix;
  }

  /**
   * @see org.kalypso.ui.rrm.internal.calccase.AbstractCatchmentModelRunner#executeCatchmentModel(org.kalypso.ui.rrm.internal.calccase.ICatchmentModelInfo,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void executeCatchmentModel( final ICatchmentModelInfo info, final IProgressMonitor monitor ) throws CoreException
  {
    /* Get the parameters. */
    final RrmSimulation simulation = info.getSimulation();
    final NaModell model = info.getModel();
    final IRainfallGenerator generator = info.getGenerator();
    final QName targetLink = info.getTargetLink();
    final String parameterType = info.getParameterType();
    final Period timestep = info.getTimestep();

    /* HINT: The range may be the adjusted simulation range, if executed as standalone generator. */
    /* HINT: The range may be the intersection of the adjusted simulation range and the generators validity range, */
    /* HINT: if executed as part of a multi generator. */
    /* HINT: But in the end, we do not care, because we simple use it. */
    final DateRange simulationRange = info.getSimulationRange();

    /* Get the unadjusted simulation range. */
    final DateRange unadjustedSimulationRange = info.getUnadjustedSimulationRange();

    /* Only ILinearSumGenerator's are supported. */
    if( !(generator instanceof ILinearSumGenerator) )
      throw new UnsupportedOperationException( "Only ILinearSumGenerator's are supported..." ); //$NON-NLS-1$

    /* Cast. */
    final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) generator;

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString( "LinearSumCatchmentModelRunner_0" ), 100 ); //$NON-NLS-1$
      monitor.subTask( TimeseriesUtils.getName( parameterType ) );

      /* Compare the catchments of the model and the catchments of the generator. */
      final IStatus status = CatchmentModelHelper.compareCatchments( model, linearGenerator );
      if( !status.isOK() )
        throw new CoreException( status );

      /* Create the rainfall generation operation. */
      final IRainfallCatchmentModel rainfallModel = createRainfallModel( model, linearGenerator, targetLink, simulationRange );

      /* Initialize the generator. */
      final ILinearSumGenerator clonedGenerator = (ILinearSumGenerator) rainfallModel.getGenerators().get( 0 );
      initGenerator( clonedGenerator, simulationRange, unadjustedSimulationRange, timestep, parameterType );

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
      throw new CoreException( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "LinearSumCatchmentModelRunner_1", ex.getLocalizedMessage() ), ex ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();

      /* Refresh the simulation folder. */
      refresh( simulation );
    }
  }

  private void initGenerator( final ILinearSumGenerator generator, final DateRange simulationRange, final DateRange unadjustedSimulationRange, final Period timestep, final String parameterType )
  {
    /* Set the period. */
    generator.setPeriod( simulationRange );
    generator.setValidityRange( unadjustedSimulationRange );

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
    final Map<String, String> linkHash = new HashMap<>();

    /* Get the catchments. */
    final List<ICatchment> generatorCatchments = generator.getCatchments();
    for( final ICatchment generatorCatchment : generatorCatchments )
    {
      /* Get the area. */
      final IXLinkedFeature areaLink = (IXLinkedFeature) generatorCatchment.getAreaLink();
      final Catchment catchment = (Catchment) areaLink.getFeature();

      /* Build the hash. */
      final String hash = CatchmentHelper.buildHash( generatorCatchment );

      /* If the link hash contains this hash code, the corresponding link will be used. */
      if( linkHash.containsKey( hash ) )
      {
        /* Get the link. */
        final String link = linkHash.get( hash );

        /* Set the link. */
        CatchmentModelHelper.setLink( catchment, targetLink, link );
      }
      else
      {
        /* Otherwise create a new link. */
        final String folderName = CatchmentModelHelper.getTargetLinkFolderName( simulation, parameterType );
        final String link = CatchmentModelHelper.buildLink( simulation, m_prefix, folderName, parameterType, catchment );

        /* Set the link. */
        CatchmentModelHelper.setLink( catchment, targetLink, link );

        /* Store the hash code. */
        linkHash.put( hash, link );
      }
    }
  }

  private IRainfallCatchmentModel createRainfallModel( final NaModell model, final ILinearSumGenerator generator, final QName targetLink, final DateRange targetRange ) throws Exception
  {
    final GMLWorkspace workspace = model.getWorkspace();
    final IFeatureProviderFactory featureProviderFactory = workspace.getFeatureProviderFactory();

    /* Rainfall model. */
    final GMLWorkspace modelWorkspace = FeatureFactory.createGMLWorkspace( IRainfallCatchmentModel.FEATURE_RAINFALL_CATCHMENT_MODEL, model.getWorkspace().getContext(), featureProviderFactory );
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