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
 *  Lesser General License for more details.
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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;

/**
 * Interface for classes that represents a feature of the type wb1d2d:Element
 * 
 * @author Patrice Congo
 */
public interface IFE1D2DElement extends IFENetItem
{
  QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element" ); //$NON-NLS-1$

  QName WB1D2D_PROP_ELEMENT_CONTAINERS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "elementContainer" ); //$NON-NLS-1$

  QName WB1D2D_PROP_DIRECTEDEDGE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "directedEdge" ); //$NON-NLS-1$

  QName PROP_ROUGHNESS_CLS_ID = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "roughnessClsID" ); //$NON-NLS-1$

  QName PROP_ROUGHNESS_STYLE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "roughnessStyle" ); //$NON-NLS-1$

  QName PROP_ROUGHNESS_CORRECTION_KS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "correction_ks" ); //$NON-NLS-1$

  QName PROP_ROUGHNESS_CORRECTION_AXAY = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "correction_axay" ); //$NON-NLS-1$

  QName PROP_ROUGHNESS_CORRECTION_DP = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "correction_dp" ); //$NON-NLS-1$

  QName PROP_GEOMETRY = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" ); //$NON-NLS-1$

  void setRoughnessClsID( String value );

  void setRoughnessStyle( String value );

  void setRoughnessCorrectionKS( Double value );

  void setRoughnessCorrectionAxAy( Double value );

  void setRoughnessCorrectionDP( Double value );

  String getRoughnessClsID( );

  String getRoughnessStyle( );

  Double getRoughnessCorrectionKS( );

  Double getRoughnessCorrectionAxAy( );

  Double getRoughnessCorrectionDP( );
  
  IFE1D2DEdge[] getEdges( );

  IFE1D2DNode[] getNodes( );
  
}