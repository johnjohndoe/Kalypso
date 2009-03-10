/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.observation.test;

import java.util.Calendar;

import junit.framework.TestCase;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author schlienger
 */
public class ObservationTest extends TestCase
{
  public static IObservation<TupleResult> createTestObservation( )
  {
    final Component dc = new Component( "DATE", "Date", "", null, null, XmlTypes.XS_DATE, null, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    final Component vc = new Component( "VALUE", "Value", "", null, null, XmlTypes.XS_DOUBLE, "m", null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    final IComponent[] comps = { dc, vc };
    final TupleResult tupleResult = new TupleResult( comps );

    final Calendar cal = Calendar.getInstance();

    for( int i = 0; i < 10; i++ )
    {
      final IRecord record = tupleResult.createRecord();
      record.setValue( dc, cal.getTime() );
      record.setValue( vc, i );

      tupleResult.add( record );

      cal.add( Calendar.HOUR_OF_DAY, 1 );
    }

    return new Observation<TupleResult>( "Test-Observation", "", tupleResult ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void testDummy( )
  {
    // does not test anything, but junit complains if we have a test class without test
  }
}
