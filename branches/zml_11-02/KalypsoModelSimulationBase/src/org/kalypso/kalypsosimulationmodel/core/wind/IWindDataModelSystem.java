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
package org.kalypso.kalypsosimulationmodel.core.wind;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * Interface for classes representing the simBase:WindDataModelSystem.
 * They are typically composed by {@link IWindDataModel}s.
 * 
 * 
 * @author ig
 */
public interface IWindDataModelSystem extends IFeatureWrapper2
{

  public final static QName QNAME_PROP_ORIGIN_X = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "originX" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_ORIGIN_Y = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "originY" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_CRS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "CRS" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_CELL_X_LEN = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "cellXLen" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_CELL_Y_LEN = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "cellYLen" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_COLUMNS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "columns" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_ROWS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "rows" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_ORDER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "order" ); //$NON-NLS-1$


  /**
   * To get all wind data models in that system
   * @return all the wind data models in this system
   */
  public IFeatureWrapperCollection<IWindDataModel> getWindDataModels();

  public RectifiedGridDomain getGridDescriptor( );
  
  /**
   * returns the order(position in the list of all wind data model systems)
   */
  public int getOrder();
  
  /**
   * set the order(position in the list of all wind data model systems)
   */
  public void setOrder( final int pOrder );
}
