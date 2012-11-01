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
package org.kalypso.model.rcm.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.internal.i18n.Messages;
import org.kalypso.observation.util.ObservationHelper;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.timeseries.TuppleModelsLinearAdd;
import org.kalypso.ogc.sensor.timeseries.datasource.AddDataSourceObservationHandler;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.core.filter.ZmlFilterWorker;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Utilities for {@link org.kalypso.model.rcm.internal.binding.OmbrometerRainfallGenerator}'s and
 * {@link org.kalypso.model.rcm.internal.binding.InverseDistanceRainfallGenerator}'s.
 *
 * @author Holger Albert
 */
public final class RainfallGeneratorUtilities
{
  /**
   * The constructor.
   */
  private RainfallGeneratorUtilities( )
  {
  }

  /**
   * Res9olves the areas from the catchments AND transforms them into the kalypso srs.
   */
  public static GM_MultiSurface[] findCatchmentAreas( final Feature[] catchmentFeatures, final GMLXPath catchmentAreaXPath ) throws CoreException
  {
    try
    {
      final String kalypsoSrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      /* Memory for the results. */
      final GM_MultiSurface[] areas = new GM_MultiSurface[catchmentFeatures.length];

      for( int i = 0; i < catchmentFeatures.length; i++ )
      {
        final Feature catchmentFeature = catchmentFeatures[i];
        if( catchmentFeature == null )
          throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("RainfallGeneratorUtilities_0") ) ); //$NON-NLS-1$

        final Object object = GMLXPathUtilities.query( catchmentAreaXPath, catchmentFeature );
        final GM_MultiSurface multiSurface = toMultiSurface( catchmentAreaXPath, object );
        final GM_MultiSurface transformedMultiSurface = (GM_MultiSurface) multiSurface.transform( kalypsoSrs );
        areas[i] = transformedMultiSurface;
      }

      return areas;
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, "Failed to resolve catchment geometry path", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, "Failed to transform catchment geometry", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private static GM_MultiSurface toMultiSurface( final GMLXPath catchmentAreaXPath, final Object object ) throws CoreException
  {
    if( object instanceof GM_Polygon )
    {
      final GM_Polygon surface = (GM_Polygon) object;
      return GeometryFactory.createGM_MultiSurface( new GM_Polygon[] { surface }, surface.getCoordinateSystem() );
    }

    if( object instanceof GM_MultiSurface )
      return (GM_MultiSurface) object;

    if( object == null )
      return null; // does not make sense to process

    final String message = String.format( Messages.getString("RainfallGeneratorUtilities_1"), object, catchmentAreaXPath ); //$NON-NLS-1$
    throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, message ) );
  }

  public static IObservation[] readObservations( final Feature[] ombrometerFeatures, final GMLXPath linkXPath, final IZmlFilter[] filters, final DateRange dateRange ) throws SensorException
  {
    final IRequest request = new ObservationRequest( dateRange );

    final IObservation[] readObservations = new IObservation[ombrometerFeatures.length];
    for( int i = 0; i < ombrometerFeatures.length; i++ )
    {
      final ZmlLink link = new ZmlLink( ombrometerFeatures[i], linkXPath );
      if( link.isLinkSet() )
      {
        final IObservation source = link.loadObservation();
        final IObservation filteredObservation = ZmlFilterWorker.applyFilters( source, filters );
        final IObservation resolvedObservation = ObservationHelper.clone( filteredObservation, request );
        readObservations[i] = resolvedObservation;
      }
    }

    return readObservations;
  }

  /**
   * This function combines the observations using the specified weights.
   *
   * @param observations
   *          The observations to combine.
   * @param weights
   *          The weights to use.
   * @param dataSource
   *          The data source of the resulting observation
   * @return A new combined observation.
   */
  public static IObservation combineObses( final IObservation[] observations, final double[] weights, final String dataSource ) throws SensorException
  {
    /* There should be a weight for each observation. */
    Assert.isTrue( observations.length == weights.length );

    /* Here the array of the observations and the heights should have the same length. */
    /* So it is enough to check one of them. */
    if( observations.length == 0 )
      return null;

    /* Get some of the metadata from the first observation. */
    final IObservation firstObservation = observations[0];
    final MetadataList firstMetadataList = firstObservation.getMetadataList();
    final String timestep = findTimeStep( observations );
    final String firstStart = firstMetadataList.getProperty( TimeseriesUtils.MD_VORHERSAGE_START );
    final String firstEnde = firstMetadataList.getProperty( TimeseriesUtils.MD_VORHERSAGE_ENDE );

    /* Get some of the axis from the first observation. */
    final ITupleModel firstTuppleModel = firstObservation.getValues( null );
    final IAxis[] firstAxisList = firstTuppleModel.getAxes();
    final IAxis firstDateAxis = ObservationUtilities.findAxisByClass( firstAxisList, Date.class );
    final IAxis firstValueAxis = ObservationUtilities.findAxisByClass( firstAxisList, Double.class );
    IAxis firstStatusAxis = KalypsoStatusUtils.findStatusAxisForNoEx( firstAxisList, firstValueAxis );
    if( firstStatusAxis == null )
      firstStatusAxis = KalypsoStatusUtils.createStatusAxisFor( firstValueAxis, true );

    // FIXME 1: We still get values from observations with weight 0.0.
    // FIXME 1: -> We should first filter those out to improve performance.
    // FIXME 2: For still better performance, we could filter out everything with a weight smaller than some limit.
    // FIXME 2: To still get 100%, we could share the difference with the remaining obses to their weight.
    final List<ITupleModel> observationValues = new ArrayList<>();
    for( final IObservation observation : observations )
      observationValues.add( observation.getValues( null ) );

    /* ATTENTION: Make sure the axes of the observation are in the same order as the axes of the combined tuple model. */
    final TuppleModelsLinearAdd linearAdd = new TuppleModelsLinearAdd( firstValueAxis.getType(), firstDateAxis, firstValueAxis, firstStatusAxis );
    final ITupleModel combinedTuppleModel = linearAdd.addWeighted( observationValues.toArray( new ITupleModel[] {} ), weights );

    /* Copy the metadata. */
    final MetadataList metadata = new MetadataList();
    if( timestep != null )
      metadata.setProperty( MetadataHelper.MD_TIMESTEP, timestep );
    if( firstStart != null )
      metadata.setProperty( TimeseriesUtils.MD_VORHERSAGE_START, firstStart );
    if( firstEnde != null )
      metadata.setProperty( TimeseriesUtils.MD_VORHERSAGE_ENDE, firstEnde );

    /* Create a new observation. */
    final SimpleObservation combinedObservation = new SimpleObservation( "", "", metadata, combinedTuppleModel ); //$NON-NLS-1$ //$NON-NLS-2$
    combinedObservation.setName( Messages.getString("RainfallGeneratorUtilities_4") ); //$NON-NLS-1$

    /* Ignore original data sources because rainfall generator combines different data sources. */
    return new AddDataSourceObservationHandler( dataSource, dataSource, combinedObservation ).extend();
  }

  public static String findTimeStep( final IObservation[] observations )
  {
    /* A bit of a hack: Search all observations for a valid timestep. */
    for( final IObservation observation : observations )
    {
      final MetadataList metadataList = observation.getMetadataList();
      final String timestep = metadataList.getProperty( MetadataHelper.MD_TIMESTEP );
      if( timestep != null )
        return timestep;
    }

    return null;
  }

  /**
   * This function finds the name of the catchment. It will check the description and the name attribute of the
   * catchment.
   *
   * @param catchment
   *          The catchment.
   * @return The name or null.
   */
  public static String findName( final Feature catchment )
  {
    /* No catchment available. */
    if( catchment == null )
      return null;

    /* Get the description. */
    final String description = catchment.getDescription();
    if( description != null && description.length() > 0 )
      return description;

    /* Get the name. */
    final String name = catchment.getName();
    if( name != null && name.length() > 0 )
      return name;

    return null;
  }
}