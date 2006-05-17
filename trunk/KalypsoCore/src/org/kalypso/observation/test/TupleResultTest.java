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
package org.kalypso.observation.test;

import junit.framework.TestCase;

import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.ValueComponent;

/**
 * @author schlienger
 */
public class TupleResultTest extends TestCase
{
  public void testGetComponents( )
  {
    TupleResult result = new TupleResult();
    assertEquals( 0, result.getComponents().length );
    assertEquals( 0, result.size() );

    final IComponent comp = new ValueComponent( "Abfluss", "", Double.class, "m�/s" );
    result.addComponent( comp );
    assertEquals( 1, result.getComponents().length );

//    ListIterator<IRecord> it = result.listIterator();

    IRecord r1 = result.createRecord();
    result.add( r1 );
    result.setValue( r1, comp, 0 );
    assertEquals( 0, r1.getValue( comp ) );

    try
    {
      result.setValue( r1, new Component( "Non-Existent", "", Integer.class ), 0 );

      fail( "there should be an exception if we set a value of a nonexistent component" );
    }
    catch( Exception ignored )
    {
    }

    try
    {
      r1.setValue( new Component( "Non-Existent", "", Integer.class ), 0 );

      fail( "there should be an exception if we set a value of a nonexistent component" );
    }
    catch( Exception ignored )
    {
    }

    IRecord r2 = result.createRecord();
    r2.setValue( comp, 1 );
    result.add( r2 );

    IRecord r3 = result.createRecord();
    result.add( r3 );
    r3.setValue( comp, 2 );

    assertEquals( 3, result.size() );

    assertEquals( 0, result.getValue( r1, comp ) );
    result.setValue( r1, comp, 4 );
    assertEquals( 4, result.getValue( r1, comp ) );

    result.removeComponent( comp );
    assertEquals( 0, result.getComponents().length );
  }

  public void testComponent( )
  {
    ValueComponent comp = new ValueComponent( "Abfluss", null, Double.class, "m�/s" );
    assertEquals( "Abfluss", comp.getName() );
    assertNull( comp.getDescription() );
    assertEquals( "m�/s", comp.getUnit() );
    assertEquals( Double.class, comp.getValueClass() );
  }
}
