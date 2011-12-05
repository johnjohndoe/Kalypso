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

import java.math.BigDecimal;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;

/**
 * @author Gernot Belger
 */
public interface IKingFlowRelation extends IFlowRelation1D
{
  public static final QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "KingFlowRelation" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_WIDTH = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "width" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_SS1 = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "ss1" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_SS2 = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "ss2" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_WIDS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "wids" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_WIDBS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "widbs" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_WSS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "wss" ); //$NON-NLS-1$

  public BigDecimal getWidth( );

  public BigDecimal getBankSlopeLeft( );

  public BigDecimal getBankSlopeRight( );

  public BigDecimal getWidthStorage( );

  public BigDecimal getHeightStorage( );

  public BigDecimal getSlopeStorage( );
}
