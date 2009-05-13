/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.Date;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public interface ICalcUnitResultMeta extends IResultMeta
{
  public static final QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "CalculationUnitResultMeta" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_CALC_START_TIME = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "simulationStartTime" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_CALC_END_TIME = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "simulationEndTime" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_CALC_UNIT_ID = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "calcUnitID" ); //$NON-NLS-1$

  public void setCalcStartTime( final Date startTime );

  public Date getCalcStartTime( );

  public void setCalcEndTime( final Date endTime );

  public Date getCalcEndTime( );

  public String getCalcUnit( );

  public void setCalcUnit( String calcUnitID );

  public IStepResultMeta addStepResult( );

  public boolean containsChildType( final DOCUMENTTYPE doctype );

  public IDocumentResultMeta getDocument( DOCUMENTTYPE doctype );

  public IDocumentResultMeta[] getDocuments( DOCUMENTTYPE doctype );

}