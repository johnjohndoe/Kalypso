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
package org.kalypso.ui.rrm.internal.cm.view;

import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Represents a mapping element ( {@link org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement} for
 * editing in the dialog.
 *
 * @author Gernot Belger
 */
public class MappingElementBean
{
  private IMappingElement m_mappingElement;

  private final Feature m_modelElement;

  private String m_href;

  private ITimeseries m_timeseries;

  public MappingElementBean( final IMappingElement mappingElement, final Feature modelElement, final String href )
  {
    m_mappingElement = mappingElement;
    m_modelElement = modelElement;
    m_href = href;
  }

  public void setMappingElement( final IMappingElement mappingElement )
  {
    m_mappingElement = mappingElement;
  }

  public IMappingElement getMappingElement( )
  {
    return m_mappingElement;
  }

  public String getHref( )
  {
    return m_href;
  }

  public Feature getModelElement( )
  {
    return m_modelElement;
  }

  public void setTimeseries( final ITimeseries timeseries )
  {
    m_timeseries = timeseries;
  }

  public ITimeseries getTimeseries( )
  {
    return m_timeseries;
  }

  public void setHref( final String href )
  {
    m_href = href;
  }
}