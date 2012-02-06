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
package org.kalypso.model.wspm.tuhh.core.gml;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class TuhhMarker extends AbstractFeatureBinder implements IWspmTuhhConstants
{
  public static final QName QNAME_MARKER = new QName( NS_WSPM_TUHH, "ProfileMarker" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_TYPE = new QName( NS_WSPM_TUHH, "type" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_LOCATION = new QName( NS_WSPM_TUHH, "location" ); //$NON-NLS-1$

  public TuhhMarker( final Feature featureToBind )
  {
    super( featureToBind, QNAME_MARKER );
  }

  public void setType( final String type )
  {
    getFeature().setProperty( QNAME_PROP_TYPE, type );
  }

  public void setLocation( final GM_Point location )
  {
    getFeature().setProperty( QNAME_PROP_LOCATION, location );
  }
}
