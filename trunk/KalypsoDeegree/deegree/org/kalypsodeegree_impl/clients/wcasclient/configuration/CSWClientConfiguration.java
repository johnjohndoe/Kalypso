/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.clients.wcasclient.configuration;

import java.io.InputStreamReader;
import java.net.URL;
import java.util.HashMap;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;

/**
 * @author Administrator
 *  
 */

public class CSWClientConfiguration
{
  private static CSWClientConfiguration conf = null;

  private CMapping mapping = null;

  private HashMap thesauri = null;

  private Download download = null;

  private TextComponent textComponent = null;

  private CCatalog[] ccatalog = null;

  private WMSClientConfiguration wmsCConfig = null;

  private int maxInactiveInterval = 0;

  private String[] filterIDs = null;

  private int maxRecords = 100;

  private int queryScope = 0;

  private GM_Envelope rootBoundingBox = null;

  /**
   * creates an instance of the Configuration
   * 
   * @param ccatalog
   * @param mapping
   * @param maxInactiveInterval
   * @param wmsCConfig
   * @param thesauri
   * @param download
   * @param textComponent
   * @param maxRecords
   * @param filterIDs
   * @throws Exception
   */
  CSWClientConfiguration( CCatalog[] ccatalog, CMapping mapping, int maxInactiveInterval,
      WMSClientConfiguration wmsCConfig, HashMap thesauri, Download download,
      TextComponent textComponent, int maxRecords, String[] filterIDs ) throws Exception
  {
    this.ccatalog = ccatalog;
    this.mapping = mapping;
    this.maxInactiveInterval = maxInactiveInterval;
    this.wmsCConfig = wmsCConfig;
    this.thesauri = thesauri;
    this.download = download;
    this.textComponent = textComponent;
    this.filterIDs = filterIDs;
    this.maxRecords = maxRecords;

    WMSGetMapRequest gmr = wmsCConfig.getInitialGetMapRequest();
    rootBoundingBox = gmr.getBoundingBox();
  }

  /**
   * returns an instance of the Configuration class and initializes a new one if
   * nessecary (singleton pattern)
   */
  public synchronized static CSWClientConfiguration getInstance( URL confURL ) throws Exception
  {

    InputStreamReader isr = new InputStreamReader( confURL.openStream() );
    conf = ConfigurationFactory.createConfiguration( isr );
    isr.close();

    return conf;
  }

  /**
   * returns an instance of the Configuration class. If no instance have been
   * initializes before an exception will be thrown
   */
  public synchronized static CSWClientConfiguration getInstance()
  {
    return conf;
  }

  /**
   * returns the addresses of all connected catalogs
   */
  public URL[] getCatalogServerAddresses()
  {
    URL[] add = new URL[ccatalog.length];

    for( int i = 0; i < add.length; i++ )
    {
      add[i] = ccatalog[i].getAddress();
    }

    return add;
  }

  /**
   * returns the address of the submitted catalog
   */
  public URL getCatalogServerAddress( String catalogName )
  {
    URL add = null;

    for( int i = 0; i < ccatalog.length; i++ )
    {
      if( ccatalog[i].getName().equals( catalogName ) )
      {
        add = ccatalog[i].getAddress();
        break;
      }
    }

    return add;
  }

  /**
   * returns the names of all servered catalogs
   */
  public String[] getCatalogNames()
  {

    String[] names = new String[ccatalog.length];

    for( int i = 0; i < names.length; i++ )
    {
      names[i] = ccatalog[i].getName();
    }

    return names;
  }

  /**
   * returns the catalog (iso-) elements that shall be targeted by a html form
   * element
   */
  public String[] getCatalogElements( String htmlField )
  {
    return mapping.getCatalogElements( htmlField );
  }

  /**
   * returns the catalog (iso-) elements that shall be targeted by a html form
   * element
   */
  public String[] getFormElements( String catalogField )
  {
    return mapping.getFormFields( catalogField );
  }

  /**
   * returns the maximun time a session will be alive after the last change in
   * seconds
   */
  public int getMaxSessionLifeTime()
  {
    return maxInactiveInterval;
  }

  /**
   * returns the address of the submitted thesaurus
   */
  public URL getThesaurusAddress( String thesaurus )
  {
    return (URL)thesauri.get( thesaurus );
  }

  /**
   * returns the names of the thesauri known by the client
   */
  public String[] getThesaurusNames()
  {
    String[] tn = new String[thesauri.size()];
    return (String[])thesauri.keySet().toArray( tn );
  }

  /**
   * returns the address of the host for sending the mails that inform the users
   * about the place where to download the requested data
   */
  public Download getDownload()
  {
    return download;
  }

  /**
   * returns an object that enables access to all text componentes that are
   * needed within the application for dynamic message generation
   */
  public TextComponent getTextComponent()
  {
    return textComponent;
  }

  /**
   * returns an array of IDs that marks UDK objects that are valid for the
   * catalog
   */
  public String[] getFilterIDs()
  {
    return filterIDs;
  }

  /**
   * returns the maximum number records requested in a GetRecord request.
   */
  public int getMaxRecords()
  {
    return maxRecords;
  }

  /**
   * returns the maximum number (iterations) to cascaded catalogs that shall be
   * performed.
   */
  public int getQueryScope()
  {
    return queryScope;
  }

  /**
   * returns the bounding box of the maximum area that shall be searched for
   * (meta)data. This parameter will be extracted from the searchmap parameter
   */
  public GM_Envelope getRootBoundingBox()
  {
    return rootBoundingBox;
  }

  /**
   * returns the configuration of the map client used by the catalogclient for
   * searching and displaying the results.
   */
  public WMSClientConfiguration getWMSClientConfiguration()
  {
    return wmsCConfig;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = mapping.toString() + "\n";

    for( int i = 0; i < ccatalog.length; i++ )
    {
      ret += ( ccatalog[i].getName() + "\n" );
    }

    return ret;
  }
}