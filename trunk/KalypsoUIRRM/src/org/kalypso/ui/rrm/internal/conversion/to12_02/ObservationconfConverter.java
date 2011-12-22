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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;

import com.google.common.base.Charsets;

/**
 * @author Gernot Belger
 */
public class ObservationconfConverter
{
  private final TimeseriesIndex m_timeseriesIndex;

  private final File m_sourceDir;

  private final File m_targetDir;

  public ObservationconfConverter( final TimeseriesIndex timeseriesIndex, final File sourceDir, final File targetDir )
  {
    m_timeseriesIndex = timeseriesIndex;
    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  public IStatus execute( final String filename )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    try
    {
      final File mappingSourceFolder = new File( m_sourceDir, INaProjectConstants.FOLDER_OBSERVATION_CONF );
      final File mappingSourceFile = new File( mappingSourceFolder, filename );

      final GMLWorkspace mappingWorkspace = GmlSerializer.createGMLWorkspace( mappingSourceFile, GmlSerializer.DEFAULT_FACTORY );
      final Feature rootFeature = mappingWorkspace.getRootFeature();
      final FeatureList mappingList = (FeatureList) rootFeature.getProperty( UrlCatalogUpdateObservationMapping.RESULT_LIST_PROP );
      for( final Object object : mappingList )
      {
        final Feature feature = (Feature) object;
        final ZmlLink oldLink = new ZmlLink( feature, UrlCatalogUpdateObservationMapping.RESULT_TS_IN_PROP );

        final String href = oldLink.getHref();
        final String projectRelativeHref = StringUtils.removeStart( href, UrlResolver.PROJECT_PROTOCOLL + "/" );

        if( StringUtils.isBlank( href ) )
        {
          // skip entry
          log.add( IStatus.WARNING, "Skit empty link: %s", null, feature.getName() );
        }
        else
        {
          final TimeseriesIndexEntry entry = m_timeseriesIndex.findTimeseriesByOldHref( projectRelativeHref );
          if( entry == null )
          {
            log.add( IStatus.WARNING, "Unable to convert old timeseries link: %s", null, projectRelativeHref );
          }
          else
          {
            final String fixedHref = entry.getHref();

            final TimeseriesLinkType fixedLink = new TimeseriesLinkType();
            fixedLink.setHref( fixedHref );
            feature.setProperty( UrlCatalogUpdateObservationMapping.RESULT_TS_IN_PROP, fixedLink );
          }
        }
      }

      final File targetMappingDir = new File( m_targetDir, INaProjectConstants.FOLDER_OBSERVATION_CONF );
      final File targetMappingFile = new File( targetMappingDir, filename );
      GmlSerializer.serializeWorkspace( targetMappingFile, mappingWorkspace, Charsets.UTF_8.name() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      log.add( IStatus.WARNING, "Failed to convert mapping file: %s", e, filename );
    }

    final String msg = String.format( "Convert timeseries mapping: %s", filename );
    return log.asMultiStatusOrOK( msg, msg );
  }

}
