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
package org.kalypso.kalypso1d2d.internal.importNet;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Data modle for {@link Import2dElementsWidget}
 * 
 * @author Gernot Belger
 */
public class Import2dElementsData extends AbstractModelObject
{
  public static final String PROPERTY_ELEMENTS = "elements"; //$NON-NLS-1$

  public static final String PROPERTY_ELEMENT_COUNT = "elementCount"; //$NON-NLS-1$

  public static final String PROPERTY_SELECTED_ELEMENT = "selectedElement"; //$NON-NLS-1$

  public static final String PROPERTY_LAST_READ_STATUS = "lastReadStatus"; //$NON-NLS-1$

  public static final String PROPERTY_ANALYSIS_ENABLED = "analysisEnabled"; //$NON-NLS-1$

  private static final IStatus NO_STATUS = new Status( IStatus.INFO, Kalypso1d2dProjectPlugin.PLUGIN_ID, "No imported elements" );

  private IPolygonWithName[] m_elements = new IPolygonWithName[0];

  private IPolygonWithName m_selectedElement;

  private int m_elementCount;

  private IStatus m_readStatus = NO_STATUS;

  public void setElements( final IPolygonWithName[] elements )
  {
    final Object oldElements = m_elements;
    final Object oldAnalysisEnabled = getAnalysisEnabled();

    m_elements = elements;

    setElementCount( m_elements.length );

    firePropertyChange( PROPERTY_ELEMENTS, oldElements, elements );
    firePropertyChange( PROPERTY_ANALYSIS_ENABLED, oldAnalysisEnabled, getAnalysisEnabled() );

    setSelectedElement( m_elements.length == 0 ? null : m_elements[0] );
  }

  public boolean getAnalysisEnabled( )
  {
    return getElementCount() > 0;
  }

  public IPolygonWithName[] getElements( )
  {
    return m_elements;
  }

  public void setElementCount( final int length )
  {
    final Object oldValue = m_elementCount;

    m_elementCount = length;

    firePropertyChange( PROPERTY_ELEMENT_COUNT, oldValue, length );
  }

  public int getElementCount( )
  {
    return m_elementCount;
  }

  public IStatus getLastReadStatus( )
  {
    return m_readStatus;
  }

  public void setLastReadStatus( final IStatus readStatus )
  {
    final Object oldValue = m_readStatus;

    if( readStatus == null )
      m_readStatus = NO_STATUS;
    else
      m_readStatus = readStatus;

    firePropertyChange( PROPERTY_LAST_READ_STATUS, oldValue, m_readStatus );
  }

  public GM_Envelope getBoundingBox( )
  {
    GM_Envelope envelope = null;
    if( m_elements == null )
      return envelope;

    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    for( final IPolygonWithName element : m_elements )
    {
      final Envelope env = element.getEnvelope();
      final GM_Envelope box = JTSAdapter.wrap( env, kalypsoSRS );

      if( envelope == null )
        envelope = box;
      else
        envelope = envelope.getMerged( box );
    }

    return envelope;
  }

  public IPolygonWithName getSelectedElement( )
  {
    return m_selectedElement;
  }

  public void setSelectedElement( final IPolygonWithName selectedElement )
  {
    final IPolygonWithName oldValue = m_selectedElement;

    m_selectedElement = selectedElement;

    firePropertyChange( PROPERTY_SELECTED_ELEMENT, oldValue, selectedElement );
  }

  public void clearElements( )
  {
    setElements( new IPolygonWithName[0] );
  }
}