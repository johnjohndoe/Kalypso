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
package org.kalypso.kalypsomodel1d2d.schema.binding.result;

import java.math.BigDecimal;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public interface IDocumentResultMeta extends IResultMeta
{
  enum DOCUMENTTYPE
  {
    nodes,
    tinTerrain,
    tinWsp,
    tinVelo,
    tinDepth,
    tinShearStress,
    hydrograph,
    lengthSection,
    log,
    coreDataZip
  }

  public static final QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "DocumentResultMeta" );

  public static final QName QNAME_PROP_DOCUMENT_TYPE = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "type" );

  public static final QName QNAME_PROP_DOCUMENT_MIN_VALUE = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "minValue" );

  public static final QName QNAME_PROP_DOCUMENT_MAX_VALUE = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "maxValue" );

  public DOCUMENTTYPE getDocumentType( );

  public void setDocumentType( DOCUMENTTYPE documentType );

  public BigDecimal getMinValue( );

  public BigDecimal getMaxValue( );

  public void setMinValue( BigDecimal minValue );

  public void setMaxValue( BigDecimal maxValue );

}