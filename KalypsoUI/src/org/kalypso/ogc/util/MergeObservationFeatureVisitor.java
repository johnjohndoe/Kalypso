/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.util;

import java.io.File;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * @author Gernot Belger
 */
public class MergeObservationFeatureVisitor implements FeatureVisitor
{
  private final IUrlResolver m_urlResolver;

  private final URL m_targetContext;

  private final URL m_sourceContext;

  private final String m_observationProperty;

  private final ILogger m_logger;

  /**
   * @param urlResolver
   *          resolver for urls
   * @param sourceContext
   *          context to resolve observation links of source observations
   * @param targetContext
   *          context to resolve observation links of target observations
   * @param observationProperty
   *          name of the property of the observation link
   */
  public MergeObservationFeatureVisitor( final IUrlResolver urlResolver, final URL sourceContext,
      final URL targetContext, final String observationProperty, final ILogger logger )
  {
    m_urlResolver = urlResolver;
    m_sourceContext = sourceContext;
    m_targetContext = targetContext;
    m_observationProperty = observationProperty;
    m_logger = logger;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final String featureId = f.getId();
    try
    {
      final TimeseriesLink obsLink = (TimeseriesLink)f.getProperty( m_observationProperty );
      if( obsLink == null )
      {
        m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX,
            "Keine Verknüpfung gefunden für Feature mit ID: " + featureId );
        return true;
      }

      final String href = obsLink.getHref();
      if( href == null || href.length() == 0 )
      {
        m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX, "Verknüpfung leer für Feature mit ID: "
            + featureId );
        return true;
      }

      // load source obs
      final URL sourceURL = m_urlResolver.resolveURL( m_sourceContext, href );
      final IObservation sourceObs = ZmlFactory.parseXML( sourceURL, featureId );
      if( sourceObs == null )
      {
        m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX,
            "Quell-Zeitreihe nicht vorhanden für Feature mit ID: " + featureId );
        return true;
      }

      // load target obs
      final URL targetURL = m_urlResolver.resolveURL( m_targetContext, href );
      final IObservation targetObs = ZmlFactory.parseXML( targetURL, featureId );
      if( targetObs == null )
      {
        m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX,
            "Ziel-Zeitreihe nicht vorhanden für Feature mit ID: " + featureId );
        return true;
      }

      // merge obses
      mergeObservation( sourceObs, targetObs );

      // Write target observation. A bit ugly, in order to find the file where to write it
      // remove query part if present, href is also used as file name here!
      final String targetHref = ZmlURL.getIdentifierPart( obsLink.getHref() );
      final IFile targetfile = ResourceUtilities.findFileFromURL( m_urlResolver
          .resolveURL( m_targetContext, targetHref ) );
      final IPath location = targetfile.getLocation();
      final File file = location.toFile();
      ZmlFactory.writeToFile( targetObs, file );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();

      // tricky: wrap the exception with timeserie-link as text to have a better error message
      m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_DETAILS,
          "Fehler beim Kopieren der Zeitreihen für Feature: " + featureId + "\t" + "Konnte Zeitreihe nicht laden" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_DETAILS,
          "Fehler beim Kopieren der Zeitreihen für Feature: " + featureId + "\t" + e.getLocalizedMessage() );
    }

    return true;
  }

  /**
   * Merges the two observations as described in the class comment.
   * 
   * @throws SensorException
   */
  private void mergeObservation( final IObservation sourceObs, final IObservation targetObs ) throws SensorException
  {
    final IAxis sourceKeyAxis = getKeyAxis( sourceObs );
    final IAxis targetKeyAxis = getKeyAxis( targetObs );

    final IAxis[] sourceAxes = sourceObs.getAxisList();
    final IAxis[] targetAxes = targetObs.getAxisList();

    final ITuppleModel sourceTuples = sourceObs.getValues( null );
    final ITuppleModel targetTuples = targetObs.getValues( null );

    final Map axisMap = mapAxes( sourceAxes, targetAxes );

    final Map sourceKeyHash = ObservationUtilities.hashValues( sourceTuples, sourceKeyAxis );

    for( int i = 0; i < targetTuples.getCount(); i++ )
    {
      final Object targetKey = targetTuples.getElement( i, targetKeyAxis );

      // does the source contains the key?
      final Integer sourceKeyIndex = (Integer)sourceKeyHash.get( targetKey );
      if( sourceKeyIndex == null )
        continue;
      final int sourceIndex = sourceKeyIndex.intValue();

      for( final Iterator axIt = axisMap.entrySet().iterator(); axIt.hasNext(); )
      {
        final Map.Entry entry = (Entry)axIt.next();
        final IAxis[] targets = (IAxis[])entry.getKey();
        final IAxis targetAxis = targets[0];
        final IAxis targetStatusAxis = targets[1];
        final IAxis[] sources = (IAxis[])entry.getValue();
        final IAxis sourceAxis = sources[0];
        final IAxis sourceStatusAxis = sources[1];

        // is the target a warned value?
        final Number targetStatus = (Number)targetTuples.getElement( i, targetStatusAxis );
        if( !KalypsoStatusUtils.checkMask( targetStatus.intValue(), KalypsoStati.BIT_CHECK ) )
          continue;

        // is the source value user edited?
        final Number sourceStatus = (Number)sourceTuples.getElement( sourceIndex, sourceStatusAxis );
        if( !KalypsoStatusUtils.checkMask( sourceStatus.intValue(), KalypsoStati.BIT_USER_MODIFIED ) )
          continue;

        // copy value
        final Object sourceValue = sourceTuples.getElement( sourceIndex, sourceAxis );

        targetTuples.setElement( i, sourceValue, targetAxis );
        targetTuples.setElement( i, sourceStatus, targetStatusAxis );
      }

    }

  }

  /**
   * Filters the 'good' target axises and maps them to their corresponding source axes.
   * 
   * @return Map <IAxis[], IAxis[]>; each array is of size 2; the axis and its status axis
   */
  private Map mapAxes( IAxis[] sourceAxes, IAxis[] targetAxes )
  {
    final Map result = new HashMap();

    for( int j = 0; j < targetAxes.length; j++ )
    {
      final IAxis targetAxis = targetAxes[j];
      // check, if it is a persistent value axis which has a status and a corresponding source axis;
      // everything else is ignored
      if( targetAxis.isKey() )
        continue;

      if( !targetAxis.isPersistable() )
        continue;

      if( KalypsoStatusUtils.isStatusAxis( targetAxis ) )
        continue;

      final IAxis targetStatusAxis = KalypsoStatusUtils.findStatusAxisFor( targetAxes, targetAxis );
      if( targetStatusAxis == null )
        continue;

      
      final IAxis sourceAxis = findSourceAxis( sourceAxes, targetAxis );
      final IAxis sourceStatusAxis = KalypsoStatusUtils.findStatusAxisFor( sourceAxes, sourceAxis );
      if( sourceStatusAxis == null )
        throw new IllegalArgumentException( "No status axis for sourceAxis with name: " + sourceAxis.getName() );

      final IAxis[] targets = new IAxis[]
      {
          targetAxis,
          targetStatusAxis };
      final IAxis[] sources = new IAxis[]
      {
          sourceAxis,
          sourceStatusAxis };

      result.put( targets, sources );
    }

    return result;
  }

  /**
   * Find a sourceAxis for a targetAxis. <br>
   * As names are not always identical (depends, if the server is availabale or not), we simply use the type. <br>
   * So this will not work if we have observations with more than one axis of the same type...
   */
  private IAxis findSourceAxis( final IAxis[] sourceAxes, final IAxis targetAxis )
  {
    // find corresponding source axis
    final String targetType = targetAxis.getType();
    final String targetName = targetAxis.getName();
    final IAxis sourceAxis = ObservationUtilities.findAxisByTypeNoEx( sourceAxes, targetType );
    if( sourceAxis == null )
      throw new IllegalArgumentException( "No source axis for target axis with name: " + targetName );

    return sourceAxis;
  }

  private IAxis getKeyAxis( final IObservation obs )
  {
    final IAxis[] axes = ObservationUtilities.findAxesByKey( obs.getAxisList() );
    if( axes.length == 0 || axes.length > 1 )
      throw new IllegalArgumentException( "MergeObservationFeatureVisitor: observation must have exactly one key-axis" );

    return axes[0];
  }
}
