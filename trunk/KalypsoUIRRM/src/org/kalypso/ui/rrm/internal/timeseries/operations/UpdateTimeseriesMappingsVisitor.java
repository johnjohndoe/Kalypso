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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import org.eclipse.core.resources.IFile;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

/**
 * updates updated timeseries_mappings.gml times series links.
 * 
 * @author Dirk Kuch
 */
public class UpdateTimeseriesMappingsVisitor implements IFeatureBindingCollectionVisitor<ITimeseriesMapping>
{

  private final IFile m_oldFile;

  private final String m_href;

  public UpdateTimeseriesMappingsVisitor( final IFile oldFile, final String href )
  {
    m_oldFile = oldFile;
    m_href = href;
  }

  @Override
  public void visit( final ITimeseriesMapping timeseriesMapping )
  {
    final IFeatureBindingCollection<IMappingElement> mappings = timeseriesMapping.getMappings();
    for( final IMappingElement mapping : mappings )
    {
      final ZmlLink timeseries = mapping.getLinkedTimeseries();
      final IFile lnk = timeseries.getFile();

      if( Objects.equal( m_oldFile, lnk ) )
      {
        final TimeseriesLinkType linkType = timeseries.getTimeseriesLink();
        linkType.setHref( m_href );
        mapping.setLinkedTimeseries( m_href );
      }

    }

  }

}
