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
package org.kalypsodeegree.model.feature;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * Intermediate Class for Feature-API refactorings. Used to declare some old, now obsolete methods of Deegree1 Feature
 * deprecated. Reason: Get nearer to the Deegree2 Feature API
 * 
 * @author Dirk Kuch
 */
public interface KalypsoBaseFeature extends BaseFeature
{
  // TODO: put deprecated comments into javadoc (@deprecated)

  @Deprecated
  // use {FeatureDeegreeTwo}.getDefaultGeometryPropertyValue() instead
  GM_Object getDefaultGeometryProperty( );

  @Deprecated
  // use {FeatureDeegreeTwo}.getBoundedBy() instead
  GM_Envelope getEnvelope( );

  @Deprecated
  // use {FeatureDeegreeTwo}.getGeometryPropertyValues instead
  GM_Object[] getGeometryProperties( );

  @Deprecated
  // use @link{FeatureDeegreeTwo}.getOwner() instead
  public Feature getParent( );

  @Deprecated
  // use FeatureDeegreeTwo.setEnvelopesUpdated() instead
  public void invalidEnvelope( );
}
