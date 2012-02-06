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
package org.kalypso.kalypsomodel1d2d.schema.binding.results;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public interface IHydrograph extends IFeatureWrapper2
{
  public final static QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "Hydrograph" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_LOCATION = new QName( NS.OM, "location" ); //$NON-NLS-1$

  @Override
  public GM_Object getLocation( );

  public void setLocation( GM_Point point );

  public IObservation<TupleResult> initializeObservation( final String domainComponentUrn, final String valueComponentUrn );

  public IObservation<TupleResult> getObservation( );

  public void setObservation( IObservation<TupleResult> obs );
}
