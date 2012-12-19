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
package org.kalypso.model.rcm;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.NamespaceContext;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.rcm.binding.IMetadata;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.internal.i18n.Messages;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.request.RequestFactory;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.utils.log.GeoStatusLog;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * This task generates rainfall for catchment areas.
 * 
 * @author Gernot Belger
 */
public class RainfallGenerationOperation implements ICoreRunnableWithProgress
{
  private final IStringResolver m_variables;

  private IObservation[] m_result;

  private final IStatusCollector m_stati = new StatusCollector( KalypsoModelRcmActivator.PLUGIN_ID );

  private final IRainfallModelProvider m_modelProvider;

  public RainfallGenerationOperation( final IRainfallModelProvider modelProvider, final IStringResolver variables )
  {
    m_modelProvider = modelProvider;
    m_variables = variables;
  }

  public IObservation[] getResult( )
  {
    return m_result;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    IRainfallCatchmentModel rcm = null;

    // FIXME: does not belong here -> instead the whole process (i.e. result status of the outer operation) should be
    // save in one go into the log ->
    // the log should also show information about the analysis of the available entries etc.
    GeoStatusLog log = null;

    try
    {
      // Real work starts here: create the operation, convert and validate parameters
      final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("RainfallGenerationOperation_0"), 100 ); //$NON-NLS-1$

      rcm = m_modelProvider.getRainfallCatchmentModell( progress.newChild( 9 ) );
      log = initializeLog( rcm );

      final ITarget targetDefinition = rcm.getTarget();
      if( targetDefinition == null )
      {
        final String message = String.format( Messages.getString("RainfallGenerationOperation_1") ); //$NON-NLS-1$
        final Status status = new Status( IStatus.OK, KalypsoModelRcmActivator.PLUGIN_ID, message );
        log.log( status );
        return status;
      }

      final Feature[] catchments = targetDefinition.resolveCatchments();

      final IMetadata[] metadata = rcm.getMetadata().toArray( new IMetadata[0] );
      final IRainfallGenerator[] generators = rcm.getGenerators().toArray( new IRainfallGenerator[0] );

      final RainfallGenerationOp operation = new RainfallGenerationOp( catchments, generators, metadata, m_variables, log );
      final IStatus status = operation.execute( progress.newChild( 90, SubMonitor.SUPPRESS_NONE ) );

      /* Find target links right now, to avoid long waiting time if anything fails here. */
      final IObservation[] observations = operation.getResult();
      final TimeseriesLinkType[] targetLinks = findCatchmentLinks( targetDefinition, catchments );
      writeObservations( targetDefinition, observations, targetLinks, catchments );

      return status;
    }
    catch( final CoreException e )
    {
      if( log != null )
        log.log( e.getStatus() );
      // TODO: log other exceptions as well?
      throw e;
    }
    catch( final RuntimeException e )
    {
      if( RuntimeException.class == e.getClass() )
        throw new InvocationTargetException( e.getCause() );
      else
        throw new InvocationTargetException( e );
    }
    catch( final Throwable e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      /* Write the log, if one was created. */
      if( log != null )
        log.serialize();

      if( rcm != null )
        rcm.getWorkspace().dispose();
    }
  }

  /** Create a log, if a log file was provided. */
  private GeoStatusLog initializeLog( final IRainfallCatchmentModel rcm ) throws CoreException
  {
    try
    {
      final URL logLocation = rcm.getLogLocation();
      if( logLocation == null )
        return null;

      IFile logFile = ResourceUtilities.findFileFromURL( logLocation );
      if( logFile == null )
        logFile = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation( new Path( FileUtils.toFile( logLocation ).getAbsolutePath() ) );

      if( logFile == null )
        return null;

      return new GeoStatusLog( logFile );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString("RainfallGenerationOperation_2"), rcm.getLogPath() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, msg, e );
      throw new CoreException( status );
    }
  }

  private void writeObservations( final ITarget target, final IObservation[] observations, final TimeseriesLinkType[] links, final Feature[] catchmentFeatures ) throws CoreException
  {
    try
    {
      // TODO: use gml filter here?
      final String targetFilter = target.getFilter();

      final DateRange period = target.getPeriod( m_variables );

      for( int i = 0; i < links.length; i++ )
      {
        final TimeseriesLinkType link = links[i];
        if( link == null )
          continue;

        /* Get the observation. */
        /* If it is null, use the request defined in the filter to create a default one. */
        IObservation obs = observations[i];
        if( obs == null && targetFilter != null )
          obs = RequestFactory.createDefaultObservation( targetFilter );

        /* If it is still null, continue. */
        if( obs == null )
          continue;

        /* Get the catchment feature. */
        final Feature catchmentFeature = catchmentFeatures[i];

        /* Update the name. */
        final String name = RainfallGeneratorUtilities.findName( catchmentFeature );
        if( name != null && name.length() > 0 )
        {
          final MetadataList metadata = obs.getMetadataList();
          metadata.setProperty( ITimeseriesConstants.MD_NAME, name );
        }

        /* Decorate the observation. */
        final IObservation filteredObs = ZmlFactory.decorateObservation( obs, targetFilter, null );
        final IRequest request = new ObservationRequest( period );

        final URL context = catchmentFeature.getWorkspace().getContext();
        final URL location = UrlResolverSingleton.resolveUrl( context, link.getHref() );

        File file = ResourceUtilities.findJavaFileFromURL( location );
        if( file == null )
          file = FileUtils.toFile( location );

        file.getParentFile().mkdirs();
        ZmlFactory.writeToFile( filteredObs, file, request );
      }
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("RainfallGenerationOperation_3"), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private TimeseriesLinkType[] findCatchmentLinks( final ITarget targetDefinition, final Feature[] catchments ) throws CoreException
  {
    final String observationPath = targetDefinition.getObservationPath();

    final NamespaceContext namespaceResolver = targetDefinition.getWorkspace().getNamespaceContext();
    final GMLXPath observationXPath = new GMLXPath( observationPath, namespaceResolver );

    final TimeseriesLinkType[] links = new TimeseriesLinkType[catchments.length];

    for( int i = 0; i < catchments.length; i++ )
    {
      final Feature catchment = catchments[i];
      links[i] = findTargetLink( observationXPath, catchment );
    }

    return links;
  }

  private TimeseriesLinkType findTargetLink( final GMLXPath observationXPath, final Feature catchment ) throws CoreException
  {
    if( catchment == null )
      return null;

    Object object;
    try
    {
      object = GMLXPathUtilities.query( observationXPath, catchment );

      if( object == null )
      {
        m_stati.add( IStatus.WARNING, Messages.getString("RainfallGenerationOperation_4") + catchment ); //$NON-NLS-1$
        return null; // does not make sense to process
      }

      if( object instanceof TimeseriesLinkType )
        return (TimeseriesLinkType)object;
    }
    catch( final GMLXPathException e )
    {
      final String msg = String.format( "Ungültiger XPath für Ziel-Link: %s ", observationXPath ); //$NON-NLS-1$
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, msg ) );
    }

    final String msg = String.format( Messages.getString("RainfallGenerationOperation_6"), object, observationXPath ); //$NON-NLS-1$
    throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, msg ) );
  }
}