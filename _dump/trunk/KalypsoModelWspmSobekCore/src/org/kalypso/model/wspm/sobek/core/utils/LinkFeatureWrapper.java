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
package org.kalypso.model.wspm.sobek.core.utils;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author kuch
 */
public class LinkFeatureWrapper
{
  private final ILinkFeatureWrapperDelegate m_wrapper;

  public LinkFeatureWrapper( final ILinkFeatureWrapperDelegate wrapper )
  {
    m_wrapper = wrapper;
  }

  public Feature getFeature( )
  {
    final Object property = m_wrapper.getProperty();

    if( property instanceof String )
      return m_wrapper.getLinkedFeature( (String) property );
    else if( property instanceof XLinkedFeature_Impl )
    {
      final XLinkedFeature_Impl xLnk = (XLinkedFeature_Impl) property;
      return xLnk.getFeature();
    }
    else if( property instanceof Feature )
      return (Feature) property;
    else if( property == null )
      return null;

    throw new IllegalStateException( "Type not supported: " + property.toString() );
  }

}
