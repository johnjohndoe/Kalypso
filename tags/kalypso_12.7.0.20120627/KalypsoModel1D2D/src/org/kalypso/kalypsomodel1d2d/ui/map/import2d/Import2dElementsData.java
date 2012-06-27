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
package org.kalypso.kalypsomodel1d2d.ui.map.import2d;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.kalypsomodel1d2d.internal.import2dm.IPolygonWithName;

/**
 * Data modle for {@link Import2dElementsWidget}
 *
 * @author Gernot Belger
 */
public class Import2dElementsData extends AbstractModelObject
{
  public static final String PROPERTY_ANALYSIS_ENABLED = "analysisEnabled"; //$NON-NLS-1$

  private IPolygonWithName[] m_elements;

  private final Import2dDataset m_statistics = new Import2dDataset();

  public Import2dDataset getStatistics( )
  {
    return m_statistics;
  }

  public void setElements( final IPolygonWithName[] elements, final IStatus readStatus )
  {
    final Object oldAnalysisEnabled = getAnalysisEnabled();

    m_elements = elements;

    if( m_elements == null )
      m_statistics.setElementCount( 0 );
    else
      m_statistics.setElementCount( m_elements.length );

    m_statistics.setLastReadStatus( readStatus );

    firePropertyChange( PROPERTY_ANALYSIS_ENABLED, oldAnalysisEnabled, getAnalysisEnabled() );
  }

  public boolean getAnalysisEnabled( )
  {
    return m_statistics.getElementCount() > 0;
  }
}