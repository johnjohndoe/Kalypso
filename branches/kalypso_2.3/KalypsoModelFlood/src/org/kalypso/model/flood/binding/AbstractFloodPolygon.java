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
package org.kalypso.model.flood.binding;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 * 
 */
public abstract class AbstractFloodPolygon extends AbstractFeatureBinder implements IFloodPolygon
{
  private final FeatureWrapperCollection<IRunoffEvent> m_runoffEvents;

  public AbstractFloodPolygon( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );

    m_runoffEvents = new FeatureWrapperCollection<IRunoffEvent>( featureToBind, IRunoffEvent.class, QNAME_PROP_EVENT );
  }

  /**
   * @see org.kalypso.model.flood.binding.IFloodPolygon#appliesToEvent(java.lang.String)
   */
  @Override
  public boolean appliesToEvent( final String eventId )
  {
    for( final IRunoffEvent event : m_runoffEvents )
    {
      if( eventId.equals( event.getFeature().getId() ) )
        return true;
    }

    return false;
  }

  @Override
  public boolean contains( final GM_Position crd )
  {
    return getArea().contains( crd );
  }

  /**
   * @see org.kalypso.model.flood.binding.IFloodPolygon#getArea()
   */
  @Override
  @SuppressWarnings("unchecked")
  public GM_Surface<GM_SurfacePatch> getArea( )
  {
    return getProperty( QNAME_PROP_AREA, GM_Surface.class );
  }

  /**
   * @see org.kalypso.model.flood.binding.IFloodPolygon#getEvents()
   */
  @Override
  public IFeatureWrapperCollection<IRunoffEvent> getEvents( )
  {
    return m_runoffEvents;
  }

}
