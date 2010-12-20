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
package org.kalypso.kalypsosimulationmodel.core.flowrel;

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.IModel;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * Interface for classes representing a simBase:FlowRelationshipModel
 *
 * @author Gernot Belger
 */
public interface IFlowRelationshipModel extends IFeatureWrapperCollection<IFlowRelationship>, IModel
{
  public static final QName QNAME_PROP_FLOW_REL_MEMBER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "flowRelationshipMember" ); //$NON-NLS-1$

  public IFlowRelationship findFlowrelationship( final GM_Position position, final double searchRectWidth );

  public IFlowRelationship[] findFlowrelationships( final GM_Position position, final double searchRectWidth );

  /**
   * narrows the possible search results. Only {@link IFlowRelationship} specified in an array (as QNames) are
   * considered.
   */
  public IFlowRelationship findFlowrelationship( final GM_Position position, final double searchDistance, final Class< ? extends IFlowRelationshipModel>[] flowRelationTypes );
}
