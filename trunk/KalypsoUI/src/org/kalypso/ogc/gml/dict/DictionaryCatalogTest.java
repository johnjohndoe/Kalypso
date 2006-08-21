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
package org.kalypso.ogc.gml.dict;

import junit.framework.TestCase;

import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A plugin test; can only be run in an eclipse runtime workspace.
 * <p>
 * Use the globally available 'KalypsoCoreTest.launch' (located in etc/test) to run this test.
 * </p>
 * 
 * @author Gernot Belger
 */
public class DictionaryCatalogTest extends TestCase
{
  private DictionaryCatalog m_catalog;

  /**
   * @see TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    // get the dictionary
    m_catalog = KalypsoGisPlugin.getDictionaryCatalog();
  }

  /**
   * @see TestCase#tearDown()
   */
  @Override
  protected void tearDown( ) throws Exception
  {
    // release the dictionary
    m_catalog = null;
  }

  public void testDictionaryIsReady( )
  {
    assertNotNull( "The KalypsoCore Plugin must provide a catalog.", m_catalog );
  }

  /**
   * Retrieves one entry from a dictionary and test if it is non null.
   */
  public void testGetDictionaryEntry( )
  {
    final String urn = "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#niederschlag";

    final Feature entry = m_catalog.getEntry( urn );
    assertNotNull( entry );

    m_catalog.releaseEntry( entry );
  }

  /**
   * Tests the behaviour of the pool in concert with getting a dictionary entry.
   * <p>
   * After release of the last entry, also the pool object (here the GMLWorkspace) should be set free.
   * </p>
   */
  public void testDictionaryIsPooled( )
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final KeyInfo[] infosBeforeStart = pool.getInfos();
    assertEquals( "Pool must be empty before start", 0, infosBeforeStart.length );

    final Feature entry = m_catalog.getEntry( "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#niederschlag" );

    final KeyInfo[] infos = pool.getInfos();
    assertEquals( "Pool should have one object pooled at this point", 1, infos.length );

    // pool is working on the workspace of the feature
    final CommandableWorkspace cw = (CommandableWorkspace) infos[0].getObject();
    assertEquals( cw.getWorkspace(), entry.getWorkspace() );

    m_catalog.releaseEntry( entry );

    final KeyInfo[] infosAtEnd = pool.getInfos();
    assertEquals( "Pool must be empty after we have finished", 0, infosAtEnd.length );
  }

  /**
   * Tests the behaviour of the pool in concert with getting a dictionary entry.
   * <p>
   * This time we get several entries
   * </p>
   * <p>
   * After release of the last entry, also the pool object (here the GMLWorkspace) should be set free.
   * </p>
   */
  public void testDictionaryIsPooledComplex( )
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final KeyInfo[] infosBeforeStart = pool.getInfos();
    assertEquals( "Pool must be empty before start", 0, infosBeforeStart.length );

    // one and release it
    final Feature entry1 = m_catalog.getEntry( "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#niederschlag" );
    m_catalog.releaseEntry( entry1 );

    assertEquals( "Pool should be empty now", 0, pool.getInfos().length );

    // two different features
    final Feature entry2 = m_catalog.getEntry( "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#niederschlag" );
    final Feature entry3 = m_catalog.getEntry( "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#wasserstand" );
    m_catalog.releaseEntry( entry2 );
    m_catalog.releaseEntry( entry3 );

    // one two times and release them, objet must be the same
    final Feature entry4 = m_catalog.getEntry( "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#niederschlag" );
    final Feature entry5 = m_catalog.getEntry( "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#niederschlag" );
    m_catalog.releaseEntry( entry4 );
    m_catalog.releaseEntry( entry5 );

    assertEquals( "Pool must be empty after we have finished", 0, pool.getInfos().length );
  }

  /**
   * Tests the behaviour of the pool in concert with getting a dictionary entry.
   * <p>
   * After release of the last entry, also the pool object (here the GMLWorkspace) should be set free.
   * </p>
   */
  public void testNotDirty( )
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final Feature entry = m_catalog.getEntry( "urn:ogc:gml:kalypso:dict:phenomenon:dont:know#niederschlag" );

    final KeyInfo[] infos = pool.getInfos();
    assertEquals( "Pool should have one object pooled at this point", 1, infos.length );
    assertFalse( infos[0].isDirty() );

    m_catalog.releaseEntry( entry );
  }
}
