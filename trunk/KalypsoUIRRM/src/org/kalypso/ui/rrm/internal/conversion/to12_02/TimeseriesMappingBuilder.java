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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;

/**
 * Helper that guesses timeseries mappings for each existing calculation case.
 * 
 * @author Gernot Belger
 */
public class TimeseriesMappingBuilder
{
  private final Map<TimeseriesMappingType, String> m_mappingIndex = new HashMap<>();

  private final Map<String, TimeseriesIndexEntry> m_oldMappings = new HashMap<>();

  private final NaModell m_naModel;

  private final ITimeseriesMappingCollection m_mappings;

  private final File m_simulationDir;

  private final TimeseriesIndex m_timeseriesIndex;

  private final File m_sourceProjectDir;

  private final Map<String, Set<TimeseriesIndexEntry>> m_convertsionMap;

  public TimeseriesMappingBuilder( final File sourceProjectDir, final NaModell naModel, final ITimeseriesMappingCollection mappings, final File simulationDir, final TimeseriesIndex timeseriesIndex, final Map<String, Set<TimeseriesIndexEntry>> convertIndex )
  {
    m_sourceProjectDir = sourceProjectDir;
    m_naModel = naModel;
    m_mappings = mappings;
    m_simulationDir = simulationDir;
    m_timeseriesIndex = timeseriesIndex;
    m_convertsionMap = convertIndex;
  }

  public String getMappingPath( final TimeseriesMappingType mappingType )
  {
    final String id = m_mappingIndex.get( mappingType );
    if( id == null )
      return null;

    return String.format( "%s#%s", INaProjectConstants.GML_CATCHMENT_MODEL_FILE, id ); //$NON-NLS-1$
  }

  public IStatus execute( final TimeseriesMappingType mappingType )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Read old mapping */
    final IStatus oldMappingStatus = readOldMapping( mappingType );
    log.add( oldMappingStatus );

    /* Always create a new mapping, even if it is empty */
    final IFeatureBindingCollection<ITimeseriesMapping> mappings = m_mappings.getTimeseriesMappings();
    final ITimeseriesMapping newMapping = mappings.addNew( ITimeseriesMapping.FEATURE_TIMESERIES_MAPPING );

    /* set metadata of mapping */
    newMapping.setDescription( m_simulationDir.getName() );
    newMapping.setComment( Messages.getString( "CatchmentModelBuilder_0" ) ); //$NON-NLS-1$;
    newMapping.setType( mappingType );
    newMapping.setLastModified( new Date() );

    /* Fetch model elements */
    final Feature[] modelElements = mappingType.getModelElements( m_naModel );

    final QName linkProperty = mappingType.getModelLinkProperty();
    final IFeatureBindingCollection<IMappingElement> mappingElements = newMapping.getMappings();

    final URL timeseriesContext = getTimeseriesContext( m_naModel );

    for( final Feature modelElement : modelElements )
    {
      final ZmlLink link = new ZmlLink( modelElement, linkProperty, timeseriesContext );
      if( link.isLinkSet() )
      {
        try
        {
          /* Guess timeseries link */
          final TimeseriesMappingGuesser timeseriesGuesser = new TimeseriesMappingGuesser( link, mappingType, m_timeseriesIndex, m_oldMappings, m_convertsionMap );

          final IStatus guessStatus = timeseriesGuesser.execute();
          log.add( guessStatus );

          final String timeseriesPath = timeseriesGuesser.getResult();
          final String modelElementRef = String.format( "%s#%s", INaProjectConstants.GML_MODELL_FILE, modelElement.getId() ); //$NON-NLS-1$

          /* always add a mapping if link exists */
          if( !StringUtils.isBlank( timeseriesPath ) )
          {
            final IMappingElement newElement = mappingElements.addNew( IMappingElement.FEATURE_MAPPING_ELEMENT );
            newElement.setName( modelElement.getName() );
            newElement.setDescription( modelElement.getDescription() );
            newElement.setLinkedModelElement( modelElementRef );
            newElement.setLinkedTimeseries( timeseriesPath );
          }

        }
        catch( final Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }

    /* Delete mapping if it is empty */
    if( newMapping.getMappings().size() == 0 )
    {
      mappings.remove( newMapping );
      log.add( IStatus.OK, Messages.getString( "TimeseriesMappingBuilder.1" ) ); //$NON-NLS-1$
    }

    final String typeLabel = mappingType.getLabel();
    final String message = String.format( Messages.getString( "TimeseriesMappingBuilder.2" ), typeLabel ); //$NON-NLS-1$
    return log.asMultiStatus( message );
  }

  private URL getTimeseriesContext( final NaModell naModel )
  {
    try
    {
      // IMPORTANT: we use the simulation folder as context, because this is the right relative location
      // for the existing timeseries links. Like this, the links do not need to be fixed before this operation.
      // The links will be removed in any way after this operation.
      return m_simulationDir.toURI().toURL();
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return naModel.getWorkspace().getContext();
    }
  }

  private IStatus readOldMapping( final TimeseriesMappingType mappingType )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final String filename = getObervationConfFilename( mappingType );

    try
    {
      final IPath observationConfPath = RrmProject.getObservationConfPath();

      final File mappingSourceFolder = new File( m_sourceProjectDir, observationConfPath.toOSString() );
      final File mappingSourceFile = new File( mappingSourceFolder, filename );

      final GMLWorkspace mappingWorkspace = GmlSerializer.createGMLWorkspace( mappingSourceFile, GmlSerializer.DEFAULT_FACTORY );
      final Feature rootFeature = mappingWorkspace.getRootFeature();
      final FeatureList mappingList = (FeatureList) rootFeature.getProperty( UrlCatalogUpdateObservationMapping.RESULT_LIST_PROP );
      for( final Object object : mappingList )
      {
        final Feature feature = (Feature) object;
        final ZmlLink oldLink = new ZmlLink( feature, UrlCatalogUpdateObservationMapping.RESULT_TS_IN_PROP );

        final String name = feature.getName();
        final String href = oldLink.getHref();
        final String projectRelativeHref = StringUtils.removeStart( href, UrlResolver.PROJECT_PROTOCOLL + "/" ); //$NON-NLS-1$

        if( !StringUtils.isBlank( href ) )
        {
          final TimeseriesIndexEntry entry = m_timeseriesIndex.findTimeseriesByOldHref( projectRelativeHref );
          m_oldMappings.put( name, entry );
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

  private String getObervationConfFilename( final TimeseriesMappingType mappingType )
  {
    switch( mappingType )
    {
      case gaugeMeasurement:
        return "ObsQZuMapping.gml"; //$NON-NLS-1$

      case nodeInflow:
        return "ObsQMapping.gml"; //$NON-NLS-1$

      case storageEvaporation:
        return "ObsEMapping.gml"; //$NON-NLS-1$
    }

    throw new IllegalArgumentException();
  }
}