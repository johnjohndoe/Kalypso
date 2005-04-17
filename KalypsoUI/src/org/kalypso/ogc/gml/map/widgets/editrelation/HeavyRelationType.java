package org.kalypso.ogc.gml.map.widgets.editrelation;

import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;

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

public class HeavyRelationType extends RelationType
{

  private final FeatureType m_bodyFT;

  private final FeatureAssociationTypeProperty m_destLinkFTP;

  /*
   * 
   * @author doemming
   */
  public HeavyRelationType( FeatureType ft1, FeatureAssociationTypeProperty linkFTP1,
      FeatureType ft2, FeatureAssociationTypeProperty linkFTP2, FeatureType ft3 )
  {
    super( ft1, linkFTP1, ft3 );
    m_bodyFT = ft2;
    m_destLinkFTP = linkFTP2;
  }

  public FeatureType getBodyFT()
  {
    return m_bodyFT;
  }

  public FeatureType getDestFT()
  {
    return m_destFT;
  }

  public FeatureAssociationTypeProperty getDestLinkFTP()
  {
    return m_destLinkFTP;
  }

  public FeatureType getSrcFT()
  {
    return m_srcFT;
  }

  public boolean equals( Object obj )
  {
    if( obj == null || !( obj instanceof HeavyRelationType ) )
      return false;
    final HeavyRelationType other = (HeavyRelationType)obj;
    return super.equals( obj ) && other.getBodyFT().equals( m_bodyFT )
        && other.getDestLinkFTP().equals( m_destLinkFTP );
  }

  public int hashCode()
  {
    return ( getSrcFT().getName() + getLink().getName() + m_bodyFT.getName()
        + m_destLinkFTP.getName() + getDestFT().getName() ).hashCode();
  }
}