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
package org.kalypso.kalypsosimulationmodel.core.operationalmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class SimpleOperationalModel implements ISimpleOperationalModel
{
  private final Feature m_feature;

  /**
   * Create a SimpleOperationalModel object based on an existing feature
   * 
   * @param feature
   * @throws IllegalArgumentException
   *           if feature is null or not of the appopriate type
   */
  public SimpleOperationalModel( Feature feature )
  {
    Assert.throwIAEOnNull( feature, "Param feature cannot be null." );
    Assert.throwIAEOnNotDirectInstanceOf( feature, KalypsoModelSimulationBaseConsts.SIM_BASE_F_SIMPLE_OPERATIONAL_MODEL );
    this.m_feature = feature;
  }

  public SimpleOperationalModel( Feature parentFeature, QName linkPropQName )
  {
    Assert.throwIAEOnNull( parentFeature, "Param feature cannot be null." );
    Assert.throwIAEOnNull( linkPropQName, "Parameter linkPropQName cannot be null." );
    this.m_feature = (Feature) parentFeature.getProperty( linkPropQName );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.operationalmodel.ISimpleOperationalModel#getOperationalConcept()
   */
  public Feature getOperationalConcept( )
  {
    Object object = m_feature.getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_P_OPERATIONAL_CONCEPT );
    if(object instanceof Feature)
      return (Feature) object;
    else if(object == null)
      return null;
    else
      throw new RuntimeException( "Feature expected but got:" + "\n\ttype=" + object.getClass() + "\n\tvalue=" + object );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.operationalmodel.ISimpleOperationalModel#getPointCoverage()
   */
  public Feature getPointCoverage( )
  {
    Object object = m_feature.getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_P_POINT_COVERAGE );
    if(object instanceof Feature)
      return (Feature) object;
    else if(object == null)
      return null;
    else
      throw new RuntimeException( "Feature expected but got:" + "\n\ttype=" + object.getClass() + "\n\tvalue=" + object );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.operationalmodel.ISimpleOperationalModel#setOperationalConcept(org.kalypsodeegree.model.feature.Feature)
   */
  public void setOperationalConcept( Feature feature )
  {
    Assert.throwIAEOnNull( feature, "Paramerter feature cannot be null" );
    m_feature.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_P_OPERATIONAL_CONCEPT, feature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.operationalmodel.ISimpleOperationalModel#setPointCoverage(org.kalypsodeegree.model.feature.Feature)
   */
  public void setPointCoverage( Feature feature )
  {
    Assert.throwIAEOnNull( feature, "Paramerter feature cannot be null" );
    m_feature.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_P_POINT_COVERAGE, feature );
  }

}
