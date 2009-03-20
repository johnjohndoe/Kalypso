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

package org.kalypso.wiskiadapter.debug;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;

import org.apache.commons.io.IOUtils;

import com.thoughtworks.xstream.XStream;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.tsmsystem.common.data.SimpleRequestSortTerm;
import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * @author schlienger
 */
public class WDPDebuger implements KiWWDataProviderRMIf
{
  private final File m_basedir;
  private final boolean m_simulate;
  private final KiWWDataProviderRMIf m_wiski;
  private final XStream m_xstream;

  public WDPDebuger( final File basedir, final boolean simulate, final KiWWDataProviderRMIf wiski )
  {
    m_basedir = basedir;
    m_simulate = simulate;
    m_wiski = wiski;

    m_xstream = new XStream();
  }

  private synchronized Object readFromFile( final String filename, final Object defaultValue )
  {
    FileInputStream stream = null;
    try
    {
      stream = new FileInputStream( new File( m_basedir, filename ) );
      final Object wiskiObj = m_xstream.fromXML( stream );
      stream.close();
      return wiskiObj;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return defaultValue;
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  private synchronized void writeToFile( final String filename, final Object wiskiObj )
  {
    FileOutputStream stream = null;
    try
    {
      stream = new FileOutputStream( new File( m_basedir, filename ) );
      m_xstream.toXML( wiskiObj, stream );
      stream.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#about()
   */
  public HashMap about() throws KiWWException, RemoteException
  {
    final String filename = "about";

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.about();
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getUserPrefs(java.lang.String, java.lang.String,
   *      java.util.HashMap)
   */
  public HashMap getUserPrefs( String arg0, String arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getUserAuthorisation(java.lang.String,
   *      java.lang.String, java.lang.String, java.lang.String, java.util.HashMap)
   */
  public HashMap getUserAuthorisation( String arg0, String arg1, String arg2, String arg3, HashMap arg4 )
      throws KiWWException, RemoteException
  {
    final String filename = "userAuthorisation";

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.getUserAuthorisation( arg0, arg1, arg2, arg3, arg4 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#readAuthorisationMapTable(java.lang.String,
   *      java.lang.String)
   */
  public Hashtable readAuthorisationMapTable( String arg0, String arg1 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#writeAuthorisationMapTable(java.lang.String,
   *      java.util.Hashtable, java.lang.String)
   */
  public void writeAuthorisationMapTable( String arg0, Hashtable arg1, String arg2 ) throws KiWWException,
      RemoteException
  {}

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getUserPermission(java.lang.String,
   *      java.lang.String, java.util.HashMap)
   */
  public HashMap getUserPermission( String arg0, String arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#setUserPrefs(java.lang.String, java.lang.String,
   *      java.util.HashMap, java.util.HashMap)
   */
  public void setUserPrefs( String arg0, String arg1, HashMap arg2, HashMap arg3 ) throws KiWWException,
      RemoteException
  {}

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#logout(java.util.HashMap, java.util.HashMap)
   */
  public void logout( HashMap arg0, HashMap arg1 ) throws KiWWException, RemoteException
  {
    if( !m_simulate )
      m_wiski.logout( arg0, arg1 );
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getColumnNames(java.lang.String,
   *      java.lang.String)
   */
  public LinkedList getColumnNames( String arg0, String arg1 ) throws KiWWException, RemoteException
  {
    final String filename = "columnNames_" + arg0 + "_" + arg1;

    if( m_simulate )
      return (LinkedList)readFromFile( filename, new LinkedList() );

    final LinkedList list = m_wiski.getColumnNames( arg0, arg1 );
    writeToFile( filename, list );
    return list;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getStationList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getStationList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2, SimpleRequestFilterTerm arg3,
      int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getGroupList(java.util.HashMap,
   *      java.lang.String[], java.lang.String, de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getGroupList( HashMap arg0, String[] arg1, String arg2, SimpleRequestSortTerm arg3,
      SimpleRequestFilterTerm arg4, int arg5, int arg6, boolean arg7, HashMap arg8 ) throws KiWWException,
      RemoteException
  {
    final String filename = "groupList_" + arg4.getFilterTerm().hashCode();

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.getGroupList( arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getStationGroupList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getStationGroupList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2,
      SimpleRequestFilterTerm arg3, int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getGroupEntryList(java.util.HashMap,
   *      java.lang.String, long, java.util.HashMap)
   */
  public LinkedList getGroupEntryList( HashMap arg0, String arg1, long arg2, HashMap arg3 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getStationParameterList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getStationParameterList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2,
      SimpleRequestFilterTerm arg3, int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getParameterTypeList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getParameterTypeList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2,
      SimpleRequestFilterTerm arg3, int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getTsInfoList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getTsInfoList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2, SimpleRequestFilterTerm arg3,
      int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException, RemoteException
  {
    final String filename = "tsInfoList_" + arg3.getFilterTerm().hashCode();

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.getTsInfoList( arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getTestMiscList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getTestMiscList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2,
      SimpleRequestFilterTerm arg3, int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getRiverList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getRiverList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2, SimpleRequestFilterTerm arg3,
      int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getRegionList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getRegionList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2, SimpleRequestFilterTerm arg3,
      int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getManagerList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getManagerList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2, SimpleRequestFilterTerm arg3,
      int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getSiteList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getSiteList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2, SimpleRequestFilterTerm arg3,
      int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getAreaList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getAreaList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2, SimpleRequestFilterTerm arg3,
      int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getTsCoverageList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getTsCoverageList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2,
      SimpleRequestFilterTerm arg3, int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getTerritoryNameCatalog(java.util.HashMap,
   *      java.util.HashMap)
   */
  public LinkedList getTerritoryNameCatalog( HashMap arg0, HashMap arg1 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getStationStatusCatalog(java.util.HashMap,
   *      java.util.HashMap)
   */
  public LinkedList getStationStatusCatalog( HashMap arg0, HashMap arg1 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getStationTypeCatalog(java.util.HashMap,
   *      java.util.HashMap)
   */
  public LinkedList getStationTypeCatalog( HashMap arg0, HashMap arg1 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getTsData(java.util.HashMap, long[],
   *      java.sql.Timestamp, java.sql.Timestamp, java.util.HashMap)
   */
  public HashMap getTsData( HashMap arg0, long[] arg1, Timestamp arg2, Timestamp arg3, HashMap arg4 )
      throws KiWWException, RemoteException
  {
    final String filename = "tsData_" + arg1[0];

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.getTsData( arg0, arg1, arg2, arg3, arg4 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getCoverageRange(java.util.HashMap,
   *      java.lang.String, java.lang.Long[], java.util.HashMap)
   */
  public LinkedList getCoverageRange( HashMap arg0, String arg1, Long[] arg2, HashMap arg3 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getStationDetailList(java.util.HashMap,
   *      java.lang.String[], java.lang.Long[], java.util.HashMap)
   */
  public HashMap getStationDetailList( HashMap arg0, String[] arg1, Long[] arg2, HashMap arg3 ) throws KiWWException,
      RemoteException
  {
    final String filename = "stationDetailList_" + arg2[0];

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.getStationDetailList( arg0, arg1, arg2, arg3 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#setTsData(java.util.HashMap, java.util.HashMap,
   *      java.util.HashMap)
   */
  public boolean setTsData( HashMap arg0, HashMap arg1, HashMap arg2 ) throws KiWWException, InterruptedException,
      RemoteException
  {
    final HashMap series = (HashMap)arg1.get( KiWWDataProviderInterface.KEY_TIMESERIES );

    final String filename = "tsData_" + series.keySet().toArray()[0];

    writeToFile( filename, arg1 );

    if( !m_simulate )
      m_wiski.setTsData( arg0, arg1, arg2 );

    return true;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getAlarmLevelList(java.util.HashMap,
   *      java.lang.Long[], java.util.HashMap)
   */
  public HashMap getAlarmLevelList( HashMap arg0, Long[] arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {
    final String filename = "alarmLevelList_" + arg1[0];

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.getAlarmLevelList( arg0, arg1, arg2 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getWebParameter(java.lang.String,
   *      java.util.HashMap)
   */
  public HashMap getWebParameter( String arg0, HashMap arg1 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#setWebParameter(java.lang.String,
   *      java.util.HashMap, java.util.HashMap)
   */
  public void setWebParameter( String arg0, HashMap arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {}

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getStationGeoMapImage(java.util.HashMap,
   *      java.util.HashMap, java.util.HashMap, java.util.HashMap)
   */
  public HashMap getStationGeoMapImage( HashMap arg0, HashMap arg1, HashMap arg2, HashMap arg3 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getRatingTables(java.util.HashMap,
   *      java.lang.String, java.lang.Long[], java.sql.Timestamp)
   */
  public HashMap getRatingTables( HashMap arg0, String arg1, Long[] arg2, Timestamp arg3 ) throws KiWWException,
      RemoteException
  {
    final String filename = "ratingTables_" + arg1 + "_" + arg2[0];

    if( m_simulate )
      return (HashMap)readFromFile( filename, new HashMap() );

    final HashMap map = m_wiski.getRatingTables( arg0, arg1, arg2, arg3 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getWebjSessionList(java.util.HashMap,
   *      java.lang.String[], de.kisters.tsmsystem.common.data.SimpleRequestSortTerm,
   *      de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm, int, int, boolean, java.util.HashMap)
   */
  public HashMap getWebjSessionList( HashMap arg0, String[] arg1, SimpleRequestSortTerm arg2,
      SimpleRequestFilterTerm arg3, int arg4, int arg5, boolean arg6, HashMap arg7 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getPolicyList(java.util.HashMap,
   *      java.util.HashMap)
   */
  public LinkedList getPolicyList( HashMap arg0, HashMap arg1 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getUserPolicy(java.util.HashMap,
   *      java.lang.String[], java.util.HashMap)
   */
  public HashMap getUserPolicy( HashMap arg0, String[] arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#setUserPolicy(java.util.HashMap,
   *      java.util.HashMap, java.util.HashMap)
   */
  public boolean setUserPolicy( HashMap arg0, HashMap arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {
    return false;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getObjectComments(java.util.HashMap,
   *      java.util.HashMap, java.sql.Timestamp, java.sql.Timestamp, java.util.HashMap)
   */
  public HashMap getObjectComments( HashMap arg0, HashMap arg1, Timestamp arg2, Timestamp arg3, HashMap arg4 )
      throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#setObjectComments(java.util.HashMap,
   *      java.util.HashMap, java.util.HashMap)
   */
  public boolean setObjectComments( HashMap arg0, HashMap arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {
    return false;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#isTsWritable(java.util.HashMap, java.lang.Long[],
   *      java.util.HashMap)
   */
  public HashMap isTsWritable( HashMap arg0, Long[] arg1, HashMap arg2 ) throws KiWWException, RemoteException
  {
    final String filename = "tsWritable_" + arg1[0];

    if( m_simulate )
    {
      final HashMap map = new HashMap();
      map.put( arg1[0], new Boolean( false ) );
      return (HashMap)readFromFile( filename, map );
    }

    final HashMap map = m_wiski.isTsWritable( arg0, arg1, arg2 );
    writeToFile( filename, map );
    return map;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getObjectAuxAttributes(java.util.HashMap,
   *      java.util.HashMap, java.util.HashMap)
   */
  public HashMap getObjectAuxAttributes( HashMap arg0, HashMap arg1, HashMap arg2 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getParameterCalibrationTable(java.util.HashMap,
   *      java.lang.Long[], java.util.HashMap)
   */
  public HashMap getParameterCalibrationTable( HashMap arg0, Long[] arg1, HashMap arg2 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getObjectDataCollectorInfo(java.util.HashMap,
   *      java.util.HashMap, java.util.HashMap)
   */
  public HashMap getObjectDataCollectorInfo( HashMap arg0, HashMap arg1, HashMap arg2 ) throws KiWWException,
      RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#getWelcomeMessages(java.lang.String)
   */
  public String[] getWelcomeMessages( String arg0 ) throws KiWWException, RemoteException
  {
    return null;
  }

  /**
   * @see de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf#setWelcomeMessages(java.lang.String,
   *      java.lang.String[])
   */
  public void setWelcomeMessages( String arg0, String[] arg1 ) throws KiWWException, RemoteException
  {}
}
