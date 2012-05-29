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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Helper that guesses timeseries mappings for each existing calculation case.
 *
 * @author Gernot Belger
 */
public class TimeseriesMappingBuilder
{
  private final Map<TimeseriesMappingType, String> m_mappingIndex = new HashMap<>();

  private final NaModell m_naModel;

  private final ITimeseriesMappingCollection m_mappings;

  private final File m_targetDir;

  private final TimeseriesIndex m_timeseriesIndex;

  public TimeseriesMappingBuilder( final NaModell naModel, final ITimeseriesMappingCollection mappings, final File targetDir, final TimeseriesIndex timeseriesIndex )
  {
    m_naModel = naModel;
    m_mappings = mappings;
    m_targetDir = targetDir;
    m_timeseriesIndex = timeseriesIndex;
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

    /* Always create a new mapping, even if it is empty */
    final IFeatureBindingCollection<ITimeseriesMapping> mappings = m_mappings.getTimeseriesMappings();
    final ITimeseriesMapping newMapping = mappings.addNew( ITimeseriesMapping.FEATURE_TIMESERIES_MAPPING );

    /* set metadata of mapping */
    newMapping.setDescription( m_targetDir.getName() );
    newMapping.setComment( Messages.getString( "CatchmentModelBuilder_0" ) ); //$NON-NLS-1$;
    newMapping.setType( mappingType );
    newMapping.setLastModified( new Date() );

    /* Fetch model elements */
    final Feature[] modelElements = mappingType.getModelElements( m_naModel );

    final QName linkProperty = mappingType.getModelLinkProperty();
    final IFeatureBindingCollection<IMappingElement> mappingElements = newMapping.getMappings();

    for( final Feature modelElement : modelElements )
    {
      final ZmlLink link = new ZmlLink( modelElement, linkProperty );
      if( link.isLinkSet() )
      {
        final String modelElementRef = String.format( "%s#%s", INaProjectConstants.GML_MODELL_FILE, modelElement.getId() );

        /* always add a mapping if link exists */
        final IMappingElement newElement = mappingElements.addNew( IMappingElement.FEATURE_MAPPING_ELEMENT );
        newElement.setName( modelElement.getName() );
        newElement.setDescription( modelElement.getDescription() );
        newElement.setLinkedFeature( modelElementRef );

        /* Guess timeseries link */
        final TimeseriesMappingGuesser timeseriesGuesser = new TimeseriesMappingGuesser( link, mappingType, m_timeseriesIndex );
        final IStatus guessStatus = timeseriesGuesser.execute();
        final String timeseriesPath = timeseriesGuesser.getResult();

        newElement.setLinkedTimeseries( timeseriesPath );

        log.add( guessStatus );
      }
    }

    final String typeLabel = mappingType.getLabel();
    final String message = String.format( "Convert timeseries mapping of type '%s' from existing timeseries references", typeLabel );
    return log.asMultiStatusOrOK( message, message );
  }
}