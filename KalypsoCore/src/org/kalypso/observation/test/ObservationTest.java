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
package org.kalypso.observation.test;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.DateComponent;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.ValueComponent;

/**
 * @author schlienger
 */
public class ObservationTest extends TestCase
{
  public static IObservation<TupleResult> createTestObservation( )
  {
    DateComponent dc = new DateComponent( "Date", "", XmlTypes.XS_DATE );
    ValueComponent vc = new ValueComponent( "Value", "", XmlTypes.XS_DOUBLE, "m" );
    IComponent[] comps = { dc, vc };
    TupleResult tupleResult = new TupleResult( comps );

    Calendar cal = Calendar.getInstance();
    
    for( int i = 0; i < 10; i++ )
    {
      IRecord record = tupleResult.createRecord();
      record.setValue( dc, cal.getTime() );
      record.setValue( vc, i );
      
      tupleResult.add( record );
      
      cal.add( Calendar.HOUR_OF_DAY, 1 );
    }

    List<MetadataObject> mdList = new ArrayList<MetadataObject>();
    mdList.add( new MetadataObject( "Test", "Nothing") );
    
    return new Observation<TupleResult>( "Test-Observation", "", tupleResult, mdList );
  }
}
