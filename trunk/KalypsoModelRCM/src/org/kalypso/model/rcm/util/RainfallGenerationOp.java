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
package org.kalypso.model.rcm.util;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.model.rcm.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * This class does the real generation stuff.
 * 
 * @author Gernot Belger
 */
public class RainfallGenerationOp
{
  private final static class Generator
  {
    public final String m_gmlId;

    public final Date m_from;

    public final Date m_to;

    public Generator( final String gmlId, final Date from, final Date to )
    {
      m_gmlId = gmlId;
      m_from = from;
      m_to = to;
    }
  }

  private final List<Generator> m_generators = new ArrayList<Generator>();

  private final List<IStatus> m_generatorStati = new ArrayList<IStatus>();

  private final URL m_rcmGmlLocation;

  private final String m_catchmentFeaturePath;

  private final String m_catchmentObservationPath;

  private final Map<QName, String> m_catchmentMetadata;

  private final String m_targetFilter;

  private final Date m_targetFrom;

  private final Date m_targetTo;

  private final GMLWorkspace m_catchmentWorkspace;

  private final ILog m_log;

  private String m_sourceFilter;

  /**
   * The constructor.
   * 
   * @param rcmGmlLocation
   * @param catchmentWorkspace
   * @param catchmentFeaturePath
   * @param catchmentObservationPath
   * @param catchmentMetadata
   * @param targetFilter
   * @param targetFrom
   * @param targetTo
   * @param log
   *          If provided, the generators will write messages to this log.
   */
  public RainfallGenerationOp( final URL rcmGmlLocation, final GMLWorkspace catchmentWorkspace, final String catchmentFeaturePath, final String catchmentObservationPath, final Map<QName, String> catchmentMetadata, final String targetFilter, final Date targetFrom, final Date targetTo, final ILog log )
  {
    m_rcmGmlLocation = rcmGmlLocation;
    m_catchmentWorkspace = catchmentWorkspace;
    m_catchmentFeaturePath = catchmentFeaturePath;
    m_catchmentObservationPath = catchmentObservationPath;
    m_catchmentMetadata = catchmentMetadata;
    m_targetFilter = targetFilter;
    m_targetFrom = targetFrom;
    m_targetTo = targetTo;
    m_log = log;
  }
  
  
  /**
   * Sets a filter that will be applied to all read observations. If <code>null</code>, the observation is read as is.
   */
  public void setSourceFilter( String sourceFilter )
  {
    m_sourceFilter = sourceFilter;
  }

  public void addGenerator( final String gmlId, final Date from, final Date to )
  {
    m_generators.add( new Generator( gmlId, from, to ) );
  }

  public IObservation[] execute( final ILogger logger, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "", m_generators.size() * 10 + 2 * 3 + 10 );

    // 1. Load and verify catchments
    final URL targetContext = m_catchmentWorkspace.getContext();
    final Feature[] catchmentFeatureArray = findCatchmentFeatures( m_catchmentWorkspace, logger );
    final TimeseriesLinkType[] targetLinks = findCatchmentLinks( catchmentFeatureArray, logger );
    final List<IObservation>[] results = new List[catchmentFeatureArray.length];
    for( int i = 0; i < results.length; i++ )
      results[i] = new ArrayList<IObservation>();

    // 2. Load and verify generators
    final GMLWorkspace rcmWorkspace = loadGML( "Lade Gebietsniederschlagsmodelldefinition", m_rcmGmlLocation, logger, progress );

    for( final Generator generatorDesc : m_generators )
    {
      try
      {
        final Date from = generatorDesc.m_from;
        final Date to = generatorDesc.m_to;
        final IObservation[] obses = generate( generatorDesc, rcmWorkspace, catchmentFeatureArray, from, to, logger, m_log, progress.newChild( 10, SubMonitor.SUPPRESS_NONE ) );
        if( obses == null )
        {
          final String msg = String.format( "Niederschlagserzeugung f�r Generator '%s' liefert keine Ergebnisse und wird ingoriert", generatorDesc.m_gmlId );
          logger.log( Level.WARNING, -1, msg );
        }
        else if( obses.length != results.length )
        {
          final String msg = String.format( "Niederschlagserzeugung f�r Generator '%s': Anzahl Ergebnisszeitreihen ist falsch und wird ingoriert", generatorDesc.m_gmlId );
          logger.log( Level.WARNING, -1, msg );
        }
        else
        {
          for( int i = 0; i < obses.length; i++ )
          {
            final IObservation e = obses[i];
            if( e != null )
            {
              final IObservation resolvedObs = resolveObservation( e, from, to );
              results[i].add( resolvedObs );
            }
          }
        }
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelRcmActivator.getDefault().getLog().log( status );

        final String msg = String.format( "Niederschlagserzeugung f�r Generator '%s' schlug fehl mit Meldung: %s", generatorDesc.m_gmlId, status.getMessage() );
        logger.log( Level.WARNING, -1, msg );
        m_generatorStati.add( status );
      }
    }

    // Combine observations and write into target file while applying the targetFilter
    progress.subTask( "Schreibe Zeitreihen" );
    final IObservation[] combinedObservations = combineObservations( results );

    /* Add additional metadata, if wanted. */
    addAdditionalMetadata( catchmentFeatureArray, combinedObservations );

    if( targetLinks != null )
      writeObservations( combinedObservations, targetLinks, targetContext );

    ProgressUtilities.worked( progress, 10 );

    /* Dispose the rainfall catchment workspace. */
    if( rcmWorkspace != null )
      rcmWorkspace.dispose();

    return combinedObservations;
  }

  /**
   * Resolves all filters etc. and creates an new SimplObservationin memory, enforcing a given date range.<br>
   * TODO: move into ObservationUtilities
   */
  private IObservation resolveObservation( final IObservation o, final Date from, final Date to ) throws SensorException
  {
    final IRequest request = new ObservationRequest( from, to );

    final String href = o.getHref();
    final String identifier = o.getIdentifier();
    final String name = o.getName();
    final MetadataList metadataList = new MetadataList();
    metadataList.putAll( o.getMetadataList() );
    final IAxis[] axisList = o.getAxisList(); // clone?
    final ITuppleModel values = o.getValues( request );

    final SimpleTuppleModel clonedValues = new SimpleTuppleModel( values, new DateRange( from, to ) );

    final SimpleObservation simpleObservation = new SimpleObservation( href, identifier, name, false, metadataList, axisList, clonedValues );
    return simpleObservation;
  }

  private IObservation[] combineObservations( final List<IObservation>[] observationLists )
  {
    final IObservation[] result = new IObservation[observationLists.length];
    for( int i = 0; i < result.length; i++ )
      result[i] = combineObservations( observationLists[i] );

    return result;
  }

  private void addAdditionalMetadata( final Feature[] catchmentFeatureArray, final IObservation[] combinedObservations )
  {
    if( m_catchmentMetadata == null || m_catchmentMetadata.size() == 0 )
      return;

    for( int i = 0; i < combinedObservations.length; i++ )
    {
      /* All arrays must be in the same order and must have the same length. */
      IObservation observation = combinedObservations[i];
      Feature feature = catchmentFeatureArray[i];
      if( observation == null || feature == null )
        continue;

      /* Get the metadata list of this observation. */
      MetadataList metadataList = observation.getMetadataList();

      /* Get the qnames (keys) of the properties, which should be added as additional metadata. */
      QName[] qnames = m_catchmentMetadata.keySet().toArray( new QName[] {} );
      for( int j = 0; j < qnames.length; j++ )
      {
        /* Get the qname. */
        QName qname = qnames[j];

        /* Get the target string. */
        String target = m_catchmentMetadata.get( qname );

        /* Get the metadata property. */
        Object property = feature.getProperty( qname );

        /* Add the metadata property. */
        if( property != null )
          metadataList.setProperty( target, property.toString() );
        else
          metadataList.setProperty( target, "-" );
      }

      // TODO More fixed metadata ...
    }
  }

  /**
   * Combines a list of observations into a single one.
   */
  public static IObservation combineObservations( final List<IObservation> observationList )
  {
    try
    {
      final int size = observationList.size();
      if( size == 0 )
        return null;
      else if( size == 1 )
        return observationList.get( 0 );
      else
      {
        final IObservation[] obses = observationList.toArray( new IObservation[size] );
        final ForecastFilter fc = new ForecastFilter();
        fc.initFilter( obses, obses[0], null );
        return fc;
      }
    }
    catch( final SensorException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelRcmActivator.getDefault().getLog().log( status );
      return null;
    }
  }

  /**
   * @param generatorDesc
   * @param rcmWorkspace
   * @param catchmentFeatures
   * @param from
   * @param to
   * @param logger
   *          To this log, the function will write its messages.
   * @param log
   *          If provided, the generators will write messages to this log.
   * @param monitor
   */
  private IObservation[] generate( final Generator generatorDesc, final GMLWorkspace rcmWorkspace, final Feature[] catchmentFeatures, final Date from, final Date to, final ILogger logger, final ILog log, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    final String generatorId = generatorDesc.m_gmlId;
    if( generatorId == null )
    {
      logger.log( Level.SEVERE, -1, "Generator ohne g�ltige ID. Eintrag wird ignoriert." );
      return null;
    }

    /* Get the rainfall generator. */
    final IRainfallGenerator rainGen = (IRainfallGenerator) rcmWorkspace.getFeature( generatorId );
    if( rainGen == null )
    {
      final String msg = String.format( "Generator mit ID '%s' konnte in rcm-gml (%s) nicht gefunden werden. Eintrag wird ignoriert.", generatorId, rcmWorkspace.getContext() );
      logger.log( Level.SEVERE, -1, msg );
      return null;
    }

    /* Update the generator with the log, he should use. */
    rainGen.setLog( log );

    final String generatorName = rainGen.getName();
    progress.subTask( generatorName );

    try
    {
      final IObservation[] observations = rainGen.createRainfall( catchmentFeatures, from, to, m_sourceFilter, progress.newChild( 100, SubMonitor.SUPPRESS_NONE ) );
      final String msg = String.format( "Generator '%s' erfolgreich ausgef�hrt.", generatorName );
      logger.log( Level.INFO, -1, msg );
      return observations;
    }
    finally
    {
      ProgressUtilities.done( progress );
    }
  }

  private void writeObservations( final IObservation[] observations, final TimeseriesLinkType[] links, final URL context ) throws CoreException
  {
    try
    {
      for( int i = 0; i < links.length; i++ )
      {
        final IObservation obs = observations[i];
        final TimeseriesLinkType link = links[i];
        if( obs == null || link == null )
          continue;

        final IObservation filteredObs = ZmlFactory.decorateObservation( obs, m_targetFilter, null );
        final IRequest request;
        if( m_targetFrom == null || m_targetTo == null )
          request = null;
        else
          request = new ObservationRequest( m_targetFrom, m_targetTo );

        final URL location = UrlResolverSingleton.resolveUrl( context, link.getHref() );
        final File file = ResourceUtilities.findJavaFileFromURL( location );
        ZmlFactory.writeToFile( filteredObs, file, request );
      }
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e, "Fehler beim Schreiben der Zeitreihen" );
      throw new CoreException( status );
    }
  }

  private TimeseriesLinkType[] findCatchmentLinks( final Feature[] catchments, final ILogger logger ) throws CoreException
  {
    if( m_catchmentObservationPath == null )
      return null;

    final FeaturePath featurePath = new FeaturePath( m_catchmentObservationPath );

    final TimeseriesLinkType[] links = new TimeseriesLinkType[catchments.length];

    for( int i = 0; i < catchments.length; i++ )
    {
      final Feature catchment = catchments[i];
      if( catchment == null )
        continue;

      final Object object = featurePath.getFeatureForSegment( catchment.getWorkspace(), catchment, 0 );
      if( object instanceof TimeseriesLinkType )
        links[i] = (TimeseriesLinkType) object;
      else if( object == null )
      {
        logger.log( Level.WARNING, -1, "Einzugsgebiet ohne Zielzeitreihe: " + catchment );
        catchments[i] = null; // does not make sense to process
      }
      else
      {
        final String msg = String.format( "Ung�ltiges Object in Zeitreihenlink: %s (Property: %s). Erwartet wird ein TimeseriesLinkType", object, m_catchmentObservationPath );
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, msg, null ) );
      }
    }

    return links;
  }

  private Feature[] findCatchmentFeatures( final GMLWorkspace catchmentWorkspace, final ILogger logger ) throws CoreException
  {
    final FeaturePath catchmentFeaturePath = new FeaturePath( m_catchmentFeaturePath );
    final Object catchmentsObject = catchmentFeaturePath.getFeature( catchmentWorkspace );
    if( !(catchmentsObject instanceof FeatureList) )
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Einzugsgebiet-FeaturePfad (catchmentObservationPath) zeigt nicht auf eine FeatureListe: " + m_catchmentFeaturePath, null ) );

    final FeatureList catchmentFeatures = (FeatureList) catchmentsObject;

    final Feature[] array = FeatureHelper.toArray( catchmentFeatures );
    if( array.length < catchmentFeatures.size() )
      logger.log( Level.WARNING, -1, "Ung�ltige oder leere Feature-Links in Catchment-Workspace" );

    return array;
  }

  private GMLWorkspace loadGML( final String msg, final URL location, final ILogger logger, final SubMonitor progress ) throws CoreException
  {
    try
    {
      progress.subTask( msg );
      logger.log( Level.INFO, -1, msg + ": " + location );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( location, null );

      /* Transform. */
      final TransformVisitor transformVisitor = new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      workspace.accept( transformVisitor, workspace.getRootFeature(), TransformVisitor.DEPTH_INFINITE );

      ProgressUtilities.worked( progress, 3 );
      return workspace;
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Fehler beim GML-Laden: " + location, e );
      throw new CoreException( status );
    }
  }
}
