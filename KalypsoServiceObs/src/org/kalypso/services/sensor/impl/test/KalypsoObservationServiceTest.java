/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.services.sensor.impl.test;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Map;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.beans.DateRangeBean;
import org.kalypso.ogc.sensor.beans.OCSDataBean;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.repository.beans.ItemBean;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.sensor.impl.KalypsoObservationService;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * @author schlienger
 */
public class KalypsoObservationServiceTest extends TestCase
{
  private final static String KALYPSO_SERVER_BASE = "\\\\pc242\\kalypsotemp";

  private KalypsoObservationService m_srv;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp( ) throws Exception
  {
    super.setUp();

    // because we are not in the server context
    System.setProperty( ServiceConfig.TEMP_DIR, System
        .getProperty( "java.io.tmpdir" ) );
    System.setProperty( ServiceConfig.CONF_DIR, getClass().getResource( "." )
        .getFile() );

    m_srv = new KalypsoObservationService();
  }

  public void testGetServiceVersion( )
  {
    final int ver = m_srv.getServiceVersion();

    System.out.println( "Service version: " + ver );

    assertTrue( ver >= 0 );
  }

  public void testGetChildren( ) throws RemoteException
  {
    System.out.println( "Start Tree Listing:" );

    final ItemBean[] beans = m_srv.getChildren( null );

    for( int i = 0; i < beans.length; i++ )
      outputBean( beans[i], "#" );

    System.out.println( ":Stop Tree Listing" );
  }

  private void outputBean( final ItemBean bean, final String space )
      throws RemoteException
  {
    assertNotNull( bean );

    System.out.println( space + bean.getId() + " - " + bean.getName() );

    if( m_srv.hasChildren( bean ) )
    {
      final ItemBean[] children = m_srv.getChildren( bean );

      for( int j = 0; j < children.length; j++ )
        outputBean( children[j], space + " " );
    }
  }

  public void testReadData( ) throws RemoteException, MalformedURLException
  {
    System.out.println( "Start Test Read Data:" );

    final ItemBean[] beans = m_srv.getChildren( null );

    for( int i = 0; i < /*beans.length*/ 1; i++ )
      readData( beans[i], "#" );

    System.out.println( ":Stop Test Read Data" );
  }

  private void readData( final ItemBean bean, final String space )
      throws RemoteException, MalformedURLException
  {
    final ObservationBean ob = m_srv.adaptItem( bean );

    if( ob != null )
    {
      final Map map = ob.getMetadataList();

      System.out.println( space + "Bean is observation: " + ob.getId() );
      System.out.println( space + "Metadata for " + ob.getName() + " are:"
          + map );

      final DateRangeArgument dra = DateRangeArgument.createFromPastDays( 30 );
      final DateRangeBean drb = new DateRangeBean( dra.getFrom(), dra.getTo() );

      final OCSDataBean oddb = m_srv.readData( ob, drb );
      final URL url = new URL( oddb.getLocation() );

      System.out.println( space + "Data location: " + url );
      final File f = new File( url.getFile() );
      assertTrue( f.exists() );

      m_srv.clearTempData( oddb );

      System.out.println( space + "Data cleared" );
      assertFalse( f.exists() );
    }
    else
    {
      System.out.println( space + "Bean not adaptable: " + bean.getName()
          + " ID=" + bean.getId() );
    }

    if( m_srv.hasChildren( bean ) )
    {
      final ItemBean[] children = m_srv.getChildren( bean );

      for( int j = 0; j < children.length; j++ )
        readData( children[j], space + " " );
    }
  }

  public void testFindItem( ) throws RemoteException
  {
    final ItemBean b1 = m_srv.findItem( "Spree://2004/PA_GROEDI.zml" );
    assertNotNull( b1 );
    assertFalse( m_srv.hasChildren( b1 ) );

    final ItemBean b2 = m_srv
        .findItem( "psicompact://HN.1_ES.02PG...501010.P1_MW" );
    assertNotNull( b2 );
    assertFalse( m_srv.hasChildren( b2 ) );

    final ItemBean b3 = m_srv.findItem( "Spree://2004" );
    assertNotNull( b3 );
    assertTrue( m_srv.hasChildren( b3 ) );

    final ItemBean b4 = m_srv.findItem( "inexistent-id" );
    assertNull( b4 );

    final ItemBean b5 = m_srv.findItem( "foo://XYZ" );
    assertNull( b5 );
  }
}