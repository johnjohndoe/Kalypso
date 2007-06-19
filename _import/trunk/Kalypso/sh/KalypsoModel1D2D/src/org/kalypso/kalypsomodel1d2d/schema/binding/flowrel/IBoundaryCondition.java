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
package org.kalypso.kalypsomodel1d2d.schema.binding.flowrel;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public interface IBoundaryCondition extends IFlowRelationship
{
  public static final QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "BoundaryCondition" );

  public static final QName QNAME_DIRECTED_OBSERVATION = new QName( UrlCatalog1D2D.MODEL_1D2DObservation_NS, "DirectedObservationWithSource" );

  public static final QName QNAME_P_OBSERVATION = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "observation" );

  public static final QName QNAME_P_DIRECTION = new QName( UrlCatalog1D2D.MODEL_1D2DObservation_NS, "direction" );

  public IObservation<TupleResult> initializeObservation( final String domainComponentUrn, final String valueComponentUrn );

  public void setObservation( final IObservation<TupleResult> obs );

  public IObservation<TupleResult> getObservation( );
  
  /**
   * To get the scope mark of this boundary condition.
   * A scope mark marks a boundary line element different of the line where
   * the boundary is applied. 
   * It thereafter allows the identification of a boundary line the target calculation 
   * unit must contains.
   * @param scopeMark the scope mark of this 
   */
  public void addScopeMark( GM_Point scopeMark );
  
  /**
   * Removes the scope parks within the specified circle.
   * @param scopeMark the target scope and center of the search circle
   * @param searchRadius the mark distance within which a point is considered
   *            equal to the scopeMark         
   * 
   */
  public void removeScopeMark( GM_Point scopeMark, double searchRadius );
  
  /**
   * Removes all scope marks on of this boundary condition
   */
  public void clearScopeMarks();
  
  /**
   * Returns all scope marks of this boundary condition
   */
  public List<GM_Point> getScopeMark();
  
}
