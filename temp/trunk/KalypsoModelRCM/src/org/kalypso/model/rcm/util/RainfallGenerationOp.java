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
package org.kalypso.model.rcm.util;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;

import org.eclipse.core.runtime.CoreException;
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
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

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

  private final URL m_catchmentGmlLocation;

  private final String m_catchmentFeaturePath;

  private final String m_catchmentObservationPath;

  private final String m_catchmentAreaPath;

  private final String m_targetFilter;

  private final Date m_targetFrom;

  private final Date m_targetTo;

  public RainfallGenerationOp( final URL rcmGmlLocation, final URL catchmentGmlLocation, final String catchmentFeaturePath, final String catchmentObservationPath, final String catchmentAreaPath, final String targetFilter, final Date targetFrom, final Date targetTo )
  {
    m_rcmGmlLocation = rcmGmlLocation;
    m_catchmentGmlLocation = catchmentGmlLocation;
    m_catchmentFeaturePath = catchmentFeaturePath;
    m_catchmentObservationPath = catchmentObservationPath;
    m_catchmentAreaPath = catchmentAreaPath;
    m_targetFilter = targetFilter;
    m_targetFrom = targetFrom;
    m_targetTo = targetTo;
  }

  public void addGenerator( final String gmlId, final Date from, final Date to )
  {
    m_generators.add( new Generator( gmlId, from, to ) );
  }

  public void execute( final ILogger logger, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "", m_generators.size() * 10 + 2 * 3 + 10 );

    // 1. Load and verify catchments
    final GMLWorkspace catchmentWorkspace = loadGml( "Lade Einzugsgebiete", m_catchmentGmlLocation, logger, progress );
    final URL targetContext = catchmentWorkspace.getContext();
    final Feature[] catchmentFeatureArray = findCatchmentFeatures( catchmentWorkspace, logger );
    final TimeseriesLinkType[] targetLinks = findCatchmentLinks( catchmentFeatureArray, logger );
    final GM_Surface<GM_SurfacePatch>[] catchmentAreas = findCatchmentAreas( catchmentFeatureArray, logger );
    final List<IObservation>[] results = new List[catchmentAreas.length];
    for( int i = 0; i < results.length; i++ )
      results[i] = new ArrayList<IObservation>();

    // 2. Load and verify generators
    final GMLWorkspace rcmWorkspace = loadGml( "Lade Gebietsniederschlagsmodelldefinition", m_rcmGmlLocation, logger, progress );

    for( final Generator generatorDesc : m_generators )
    {
      try
      {
        final IObservation[] obses = generate( generatorDesc, rcmWorkspace, catchmentAreas, generatorDesc.m_from, generatorDesc.m_to, logger, progress.newChild( 10, SubMonitor.SUPPRESS_NONE ) );
        if( obses == null )
        {
          final String msg = String.format( "Niederschlagserzeugung für Generator '%s' liefert keine Ergebnisse und wird ingoriert", generatorDesc.m_gmlId );
          logger.log( Level.WARNING, -1, msg );
        }
        else if( obses.length != results.length )
        {
          final String msg = String.format( "Niederschlagserzeugung für Generator '%s': Anzahl Ergebnisszeitreihen ist falsch und wird ingoriert", generatorDesc.m_gmlId );
          logger.log( Level.WARNING, -1, msg );
        }
        else
        {
          // sort by catchment
          for( int i = 0; i < obses.length; i++ )
          {
            if( obses[i] != null )
              results[i].add( obses[i] );
          }
        }

      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelRcmActivator.getDefault().getLog().log( status );

        final String msg = String.format( "Niederschlagserzeugung für Generator '%s' schlug fehl mit Meldung: %s", generatorDesc.m_gmlId, status.getMessage() );
        logger.log( Level.WARNING, -1, msg );
        m_generatorStati.add( status );
      }
    }

    // Combine observations and write into target file while applying the targetFilter
    progress.subTask( "Schreibe Zeitreihen" );
    final IObservation[] combinedObservations = combineObservations( results );
    writeObservations( combinedObservations, targetLinks, targetContext );

    ProgressUtilities.worked( progress, 10 );
  }

  private IObservation[] combineObservations( final List<IObservation>[] observationLists )
  {
    final IObservation[] result = new IObservation[observationLists.length];
    for( int i = 0; i < result.length; i++ )
      result[i] = combineObservations( observationLists[i] );

    return result;
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

  private IObservation[] generate( final Generator generatorDesc, final GMLWorkspace rcmWorkspace, final GM_Surface<GM_SurfacePatch>[] catchmentAreas, final Date from, final Date to, final ILogger logger, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    final String generatorId = generatorDesc.m_gmlId;
    if( generatorId == null )
    {
      logger.log( Level.SEVERE, -1, "Generator ohne gültige ID. Eintrag wird ignoriert." );
      return null;
    }

    final Feature feature = rcmWorkspace.getFeature( generatorId );
    if( feature == null )
    {
      final String msg = String.format( "Generator mit ID '%s' konnte in rcm-gml (%s) nicht gefunden werden. Eintrag wird ignoriert.", generatorId, rcmWorkspace.getContext() );
      logger.log( Level.SEVERE, -1, msg );
      return null;
    }

    final IRainfallGenerator rainGen = (IRainfallGenerator) feature;
    if( rainGen == null )
    {
      final String msg = String.format( "Generator mit ID '%s' konnte nicht instantiiert werden. Eintrag wird ignoriert.", generatorId );
      logger.log( Level.SEVERE, -1, msg );
      return null;
    }

    final String generatorName = rainGen.getName();
    progress.subTask( generatorName );

    try
    {
      final IObservation[] observations = rainGen.createRainfall( catchmentAreas, from, to, progress.newChild( 100, SubMonitor.SUPPRESS_NONE ) );
      final String msg = String.format( "Generator '%s' erfolgreich ausgeführt.", generatorName );
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
        final String msg = String.format( "Ungültiges Object in Zeitreihenlink: %s (Property: %s). Erwartet wird ein TimeseriesLinkType", object, m_catchmentObservationPath );
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, msg, null ) );
      }
    }

    return links;
  }

  @SuppressWarnings("unchecked")
  private GM_Surface<GM_SurfacePatch>[] findCatchmentAreas( final Feature[] catchments, final ILogger logger ) throws CoreException
  {
    final FeaturePath featurePath = new FeaturePath( m_catchmentAreaPath );

    final GM_Surface<GM_SurfacePatch>[] areas = new GM_Surface[catchments.length];

    for( int i = 0; i < catchments.length; i++ )
    {
      final Feature catchment = catchments[i];
      if( catchment == null )
        continue;

      final Object object = featurePath.getFeatureForSegment( catchment.getWorkspace(), catchment, 0 );
      if( object instanceof GM_Surface )
        areas[i] = (GM_Surface) object;
      else if( object == null )
      {
        logger.log( Level.WARNING, -1, "Einzugsgebiet ohne Polygonfläche: " + catchment );
        catchments[i] = null; // does not make sense to process
      }
      else
      {
        final String msg = String.format( "Ungültiges Object in Zeitreihenlink: %s (Property: %s). Erwartet wird ein GM_Surface", object, m_catchmentAreaPath );
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, msg, null ) );
      }
    }

    return areas;
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
      logger.log( Level.WARNING, -1, "Ungültige oder leere Feature-Links in Catchment-Workspace" );

    return array;
  }

  private GMLWorkspace loadGml( final String msg, final URL location, final ILogger logger, final SubMonitor progress ) throws CoreException
  {
    try
    {
      progress.subTask( msg );
      logger.log( Level.INFO, -1, msg + ": " + m_catchmentGmlLocation );
      final GMLWorkspace catchmentWorkspace = GmlSerializer.createGMLWorkspace( location, null );
      ProgressUtilities.worked( progress, 3 );
      return catchmentWorkspace;
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
