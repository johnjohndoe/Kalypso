/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.wiskiadapter.test;

import java.rmi.RemoteException;
import java.util.Date;

import junit.framework.TestCase;

import org.kalypso.repository.RepositoryException;
import org.kalypso.wiskiadapter.TsInfoItem;
import org.kalypso.wiskiadapter.WiskiRepository;
import org.kalypso.wiskiadapter.wiskicall.GetRatingTables;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;

/**
 * 
 * Some main() code for getting rating tables from wiski.
 * 
 * @author Gernot Belger
 */
public class TestGetRatingTables extends TestCase
{
  //  private static KiWWDataProviderRMIf m_wiski;
  //  private static HashMap m_userAuthorisation;
  private WiskiRepository m_repository;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    super.setUp();

    final String config = "rmi://ibpm.bjoernsen.de:10991/KiWWDataProvider#-#wdpuser#geheim#de";
    m_repository = new WiskiRepository( "testRepository", "factory", config, false );
  }

  /**
   * @see junit.framework.TestCase#tearDown()
   */
  protected void tearDown() throws Exception
  {
    m_repository.dispose();
  }

  public void testGetFromStation() throws RemoteException, KiWWException, RepositoryException
  {
//    final TsInfoItem item = (TsInfoItem)m_repository.findItem( "wiski://HVZ_Modellierung_Saale.Durchfluss.579620" ); // Hadmersleben.Q.15.Tafel
//    final TsInfoItem item = (TsInfoItem)m_repository.findItem( "wiski://HVZ_Modellierung_Saale.Wasserstand.579070" ); // Hadmersleben.W.15

    final TsInfoItem item = (TsInfoItem)m_repository.findItem( "wiski://HVZ_Modellierung_Saale.Inhalt.579430" ); // TS-Rappbode
    
    final Date validity = new Date();

    /* By station */
    final Long stationId = item.getWiskiStationId();
    GetRatingTables getTablesFromStation = new GetRatingTables( stationId, validity,
        KiWWDataProviderInterface.OBJECT_STATION, validity );
    m_repository.executeWiskiCall( getTablesFromStation );

    /* By parameter */
    final Long paramId = item.getWiskiParameterId();
    GetRatingTables getTablesFromParam = new GetRatingTables( paramId, validity,
        KiWWDataProviderInterface.OBJECT_PARAMETER, validity );
    m_repository.executeWiskiCall( getTablesFromParam );

    /* By timeserie */
    final Long tsId = item.getWiskiId();
    GetRatingTables getTablesFromTs = new GetRatingTables( tsId, validity, KiWWDataProviderInterface.OBJECT_TIMESERIES,
        validity );
    m_repository.executeWiskiCall( getTablesFromTs );

    System.out.println();
  }

}
