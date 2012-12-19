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
package org.kalypso.kalypso1d2d.internal.importNet.twodm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.bce.gis.io.zweidm.ISmsModel;
import com.bce.gis.io.zweidm.SmsElement;

/**
 * @author Gernot Belger
 */
public class SmsConverter
{
  private final Collection<ISmsConversionTarget> m_targets = new ArrayList<>();

  private final ISmsModel m_model;

  public SmsConverter( final ISmsModel model )
  {
    m_model = model;
  }

  public void addTarget( final ISmsConversionTarget target )
  {
    m_targets.add( target );
  }

  public void execute( )
  {
    final List<SmsElement> elements = m_model.getElementList();
    for( final SmsElement element : elements )
    {
      final IPolygonWithName surface = element.toSurface();
      for( final ISmsConversionTarget importer : m_targets )
        importer.addElement( surface );
    }

    for( final ISmsConversionTarget target : m_targets )
      target.finish();
  }
}