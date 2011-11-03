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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.rcm.binding.IMetadata;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.utils.log.GeoStatusLog;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * This task generates rainfall for catchment areas.
 * 
 * @author Gernot Belger
 */
public class RainfallGenerationOperation implements ICoreRunnableWithProgress
{
  private final IStringResolver m_variables;

  private IObservation[] m_result;

  private final URL m_rcmLocation;

  private final IStatusCollector m_stati = new StatusCollector( KalypsoModelRcmActivator.PLUGIN_ID );

  public RainfallGenerationOperation( final URL rcmLocation, final IStringResolver variables )
  {
    m_rcmLocation = rcmLocation;
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

    GeoStatusLog log = null;

    try
    {
      // Real work starts here: create the operation, convert and validate parameters
      final SubMonitor progress = SubMonitor.convert( monitor, "Gebietsniederschlagermittlung", 100 );
      progress.subTask( "Definition wird geladen..." );

      rcm = loadRainfallCatchmentModell( progress.newChild( 9 ) );
      log = initializeLog( rcm );

      final ITarget targetDefinition = rcm.getTarget();
      final Feature[] catchments = targetDefinition.getCatchments();

      final IMetadata[] metadata = rcm.getMetadata().toArray( new IMetadata[0] );
      final IRainfallGenerator[] generators = rcm.getGenerators().toArray( new IRainfallGenerator[0] );

      final RainfallGenerationOp operation = new RainfallGenerationOp( catchments, generators, metadata, m_variables, log );

      final SubMonitor subMon = progress.newChild( 90, SubMonitor.SUPPRESS_NONE );
      final IStatus status = operation.execute( subMon );

      final IObservation[] observations = operation.getResult();

      /* Find target links right now, to avoid long waiting time if anything fails here */
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

  private IRainfallCatchmentModel loadRainfallCatchmentModell( final SubMonitor progress ) throws Exception
  {
    final GMLWorkspace rcmWorkspace = GmlSerializer.createGMLWorkspace( m_rcmLocation, null );

    final Feature rootFeature = rcmWorkspace.getRootFeature();
    if( !(rootFeature instanceof IRainfallCatchmentModel) )
    {
      final String msg = String.format( "Root feature must be of type %s", IRainfallCatchmentModel.QNAME );
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, msg );
      throw new CoreException( status );
    }

    final IRainfallCatchmentModel rcm = (IRainfallCatchmentModel) rootFeature;

    // TODO: do we really need to do that? No geo-operations will be done
    final TransformVisitor transformVisitor = new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    rcmWorkspace.accept( transformVisitor, rootFeature, TransformVisitor.DEPTH_INFINITE );

    ProgressUtilities.done( progress );

    return rcm;
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
      final String msg = String.format( "Failed to initialize log file %s", rcm.getLogPath() );
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
        final IObservation obs = observations[i];
        final TimeseriesLinkType link = links[i];
        if( obs == null || link == null )
          continue;

        final URL context = catchmentFeatures[i].getWorkspace().getContext();

        final IObservation filteredObs = ZmlFactory.decorateObservation( obs, targetFilter, null );
        final IRequest request = new ObservationRequest( period );
        final URL location = UrlResolverSingleton.resolveUrl( context, link.getHref() );
        File file = ResourceUtilities.findJavaFileFromURL( location );
        if( file == null )
          file = FileUtils.toFile( location );

        ZmlFactory.writeToFile( filteredObs, file, request );
      }
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, "Fehler beim Schreiben der Zeitreihen", e );
      throw new CoreException( status );
    }
  }

  private TimeseriesLinkType[] findCatchmentLinks( final ITarget targetDefinition, final Feature[] catchments ) throws CoreException
  {
    final String observationPath = targetDefinition.getObservationPath();

    final NamespaceContext namespaceResolver = targetDefinition.getWorkspace().getNamespaceContext();
    final GMLXPath observationXPath = new GMLXPath( observationPath, namespaceResolver );

// final FeaturePath featurePath = new FeaturePath( m_catchmentObservationPath );

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
        m_stati.add( IStatus.WARNING, "Einzugsgebiet ohne Zielzeitreihe: " + catchment );
        return null; // does not make sense to process
      }

      if( object instanceof TimeseriesLinkType )
        return (TimeseriesLinkType) object;
    }
    catch( final GMLXPathException e )
    {
      final String msg = String.format( "Ung¸ltiger XPath f¸r Ziel-Link: %s ", observationXPath );
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, msg, null ) );
    }

    final String msg = String.format( "Ung¸ltiges Object in Zeitreihenlink: %s (Property: %s). Erwartet wird ein TimeseriesLinkType", object, observationXPath );
    throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, msg, null ) );
  }
}