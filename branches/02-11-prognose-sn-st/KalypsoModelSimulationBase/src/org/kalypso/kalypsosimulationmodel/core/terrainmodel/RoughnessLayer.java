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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RoughnessLayer extends AbstractFeatureBinder implements IRoughnessLayer
{
  public RoughnessLayer( Feature featureToBind )
  {
    super( featureToBind, IRoughnessLayer.QNAME );
  }

  public RoughnessLayer( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer#isEditable()
   */
  @Override
  public boolean isEditable( )
  {
    final Boolean isEditable = (Boolean) getFeature().getProperty( IRoughnessLayer.PROP_EDITABLE );
    if( isEditable == null )
      return false;
    return isEditable.booleanValue();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer#setEditable(boolean)
   */
  @Override
  public void setEditable( boolean status )
  {
    getFeature().setProperty( IRoughnessLayer.PROP_EDITABLE, status );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer#isBasicLayer()
   */
  @Override
  public boolean isBasicLayer( )
  {
    final Boolean isBasicLayer = (Boolean) getFeature().getProperty( IRoughnessLayer.PROP_LAYER_TYPE );
    if( isBasicLayer == null )
      return false;
    return isBasicLayer.booleanValue();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer#isCorrectionLayer()
   */
  @Override
  public boolean isCorrectionLayer( )
  {
    return !isBasicLayer();
  }

}
