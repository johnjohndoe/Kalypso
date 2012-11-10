/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;

/**
 * Holds the old mapping data for project conversion
 * 
 * @author Gernot Belger
 */
public class MappingData
{
  private final Map<Object, Map<String, TimeseriesIndexEntry>> m_mappings = new HashMap<>();

  private final File m_sourceProjectDir;

  private final TimeseriesIndex m_timeseriesIndex;

  public MappingData( final File sourceDir, final TimeseriesIndex timeseriesIndex )
  {
    m_sourceProjectDir = sourceDir;
    m_timeseriesIndex = timeseriesIndex;
  }

  public IStatus read( )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* for catchment models */
    log.add( readOmrbometer( ITimeseriesConstants.TYPE_RAINFALL, "ombrometer.gml" ) ); //$NON-NLS-1$
    log.add( readMapping( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE, "ObsTMapping.gml" ) ); //$NON-NLS-1$
    log.add( readMapping( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED, "ObsEMapping.gml" ) ); //$NON-NLS-1$

    /* for timeseries mappings */
    log.add( readMapping( TimeseriesMappingType.nodeInflow, "ObsQZuMapping.gml" ) ); //$NON-NLS-1$
    log.add( readMapping( TimeseriesMappingType.gaugeMeasurement, "ObsQMapping.gml" ) ); //$NON-NLS-1$ );
    log.add( readMapping( TimeseriesMappingType.storageEvaporation, "ObsEMapping.gml" ) ); //$NON-NLS-1$

    return log.asMultiStatus( Messages.getString("MappingData.0") ); //$NON-NLS-1$
  }

  private IStatus readMapping( final Object key, final String filename )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final Map<String, TimeseriesIndexEntry> mappings = new HashMap<>();
    m_mappings.put( key, mappings );

    try
    {
      final IPath observationConfPath = new Path( INaProjectConstants.PATH_OBSERVATION_CONF );

      final File mappingSourceFolder = new File( m_sourceProjectDir, observationConfPath.toOSString() );
      final File mappingSourceFile = new File( mappingSourceFolder, filename );

      final GMLWorkspace mappingWorkspace = GmlSerializer.createGMLWorkspace( mappingSourceFile, GmlSerializer.DEFAULT_FACTORY );
      final Feature rootFeature = mappingWorkspace.getRootFeature();
      final FeatureList mappingList = (FeatureList)rootFeature.getProperty( DeegreeUrlCatalog.RESULT_LIST_PROP );
      for( final Object object : mappingList )
      {
        final Feature feature = (Feature)object;
        final ZmlLink oldLink = new ZmlLink( feature, DeegreeUrlCatalog.RESULT_TS_IN_PROP );

        final String name = feature.getName();
        final String href = oldLink.getHref();
        final String projectRelativeHref = StringUtils.removeStart( href, UrlResolver.PROJECT_PROTOCOLL + "/" ); //$NON-NLS-1$

        if( !StringUtils.isBlank( href ) )
        {
          final TimeseriesIndexEntry entry = m_timeseriesIndex.findTimeseriesByOldHref( projectRelativeHref );
          mappings.put( name, entry );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      log.add( IStatus.WARNING, Messages.getString( "ObservationconfConverter_3" ), e, filename ); //$NON-NLS-1$
    }

    final String msg = String.format( Messages.getString( "ObservationconfConverter_4" ), filename ); //$NON-NLS-1$
    return log.asMultiStatus( msg );
  }

  @SuppressWarnings( "deprecation" )
  private IStatus readOmrbometer( final Object key, final String filename )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final Map<String, TimeseriesIndexEntry> mappings = new HashMap<>();
    m_mappings.put( key, mappings );

    final QName collectionMemberName = new QName( NaModelConstants.NS_OMBROMETER, "ombrometerMember" ); //$NON-NLS-1$
    final QName timeseriesPropertyName = new QName( NaModelConstants.NS_OMBROMETER, "NRepository" ); //$NON-NLS-1$

    try
    {
      final IPath observationConfPath = new Path( INaProjectConstants.PATH_OBSERVATION_CONF );

      final File mappingSourceFolder = new File( m_sourceProjectDir, observationConfPath.toOSString() );
      final File mappingSourceFile = new File( mappingSourceFolder, filename );

      final GMLWorkspace mappingWorkspace = GmlSerializer.createGMLWorkspace( mappingSourceFile, GmlSerializer.DEFAULT_FACTORY );
      final Feature rootFeature = mappingWorkspace.getRootFeature();

      final FeatureList mappingList = (FeatureList)rootFeature.getProperty( collectionMemberName );
      for( final Object object : mappingList )
      {
        final Feature feature = (Feature)object;
        final ZmlLink oldLink = new ZmlLink( feature, timeseriesPropertyName );

        final String name = feature.getName();
        final String href = oldLink.getHref();
        final String projectRelativeHref = StringUtils.removeStart( href, UrlResolver.PROJECT_PROTOCOLL + "/" ); //$NON-NLS-1$

        if( !StringUtils.isBlank( href ) )
        {
          final TimeseriesIndexEntry entry = m_timeseriesIndex.findTimeseriesByOldHref( projectRelativeHref );
          mappings.put( name, entry );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      log.add( IStatus.WARNING, Messages.getString( "ObservationconfConverter_3" ), e, filename ); //$NON-NLS-1$
    }

    final String msg = String.format( Messages.getString( "ObservationconfConverter_4" ), filename ); //$NON-NLS-1$
    return log.asMultiStatus( msg );
  }

  public Map<String, TimeseriesIndexEntry> getMapping( final Object key )
  {
    return m_mappings.get( key );
  }
}