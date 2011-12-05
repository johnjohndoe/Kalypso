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

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Dejan Antanaskovic
 *
 */
public interface IRoughnessLayer extends IFeatureWrapper2
{
  public final static QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessLayer" ); //$NON-NLS-1$
  
  public final static QName PROP_EDITABLE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "editable" ); //$NON-NLS-1$
  
  // PROP_LAYER_TYPE: true = it is a basic layer, false = it is a correction layer
  public final static QName PROP_LAYER_TYPE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "basic" ); //$NON-NLS-1$
  
  public boolean isEditable();
  
  public void setEditable(boolean status);
  
  public boolean isBasicLayer();
  
  public boolean isCorrectionLayer();
}
