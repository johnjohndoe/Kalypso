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
package org.deegree_impl.clients.wmsclient.configuration;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class WMSClientConfiguration implements Marshallable
{
  private ArrayList offeredInfoFormats = null;

  private ArrayList offeredMapFormats = null;

  private ArrayList offeredMapOperations = null;

  private ArrayList offeredMapSizes = null;

  private ArrayList offeredPanFactors = null;

  private ArrayList offeredZoomFactors = null;

  private ArrayList offeredProjects = null;

  private HashMap wmsCapabilities = null;

  private WMSClientConfiguration defaultConfig = null;

  private WMSGetMapRequest gmr = null;

  private double maxScale = 0;

  private double minScale = 0;

  /**
   * Creates a new WMSClientConfiguration object. The first submitted parameter
   * is an instance of the default map client configuration. if one of the other
   * parameters submitted to the constructor isn't set, the value(s) of the
   * default configuration will be used. this inculdes that if the default
   * configuration is created all parameters must be set! Therefore an instance
   * of a <tt>WMSClientConfiguration</tt> shall be created using a factory.
   * 
   * @param defaultConfig
   *          default map client configuration
   * @param wmsCapabilities
   *          map of the capabilities of the WMS known by the client
   * @param offeredMapSizes
   *          list of map sizes offered by the client
   * @param offeredMapFormats
   *          list of map formats offered by the client
   * @param offeredInfoFormats
   *          list of info formats offered by the client
   * @param offeredMapOperations
   *          list of map operations offered by the client
   * @param offeredZoomFactors
   *          list of zoom factors offered by the client
   * @param offeredPanFactors
   *          list of pan factors offered by the client
   * @param minScale
   *          min map scale handled by the client
   * @param maxScale
   *          maximum map scale handled by the client
   * @param gmr
   *          initial GetMap request (first map the user will see)
   */
  WMSClientConfiguration( WMSClientConfiguration defaultConfig, HashMap wmsCapabilities,
      MapSize[] offeredMapSizes, Format[] offeredMapFormats, Format[] offeredInfoFormats,
      MapOperation[] offeredMapOperations, MapOperationFactor[] offeredZoomFactors,
      MapOperationFactor[] offeredPanFactors, double minScale, double maxScale,
      WMSGetMapRequest gmr, Project[] offeredProjects )
  {
    this.offeredMapSizes = new ArrayList();
    this.offeredMapFormats = new ArrayList();
    this.offeredInfoFormats = new ArrayList();
    this.offeredMapOperations = new ArrayList();
    this.offeredZoomFactors = new ArrayList();
    this.offeredPanFactors = new ArrayList();
    this.offeredProjects = new ArrayList();

    this.defaultConfig = defaultConfig;

    setOfferedMapSizes( offeredMapSizes );
    setOfferedMapFormats( offeredMapFormats );
    setOfferedInfoFormats( offeredInfoFormats );
    setOfferedMapOperations( offeredMapOperations );
    setOfferedZoomFactors( offeredZoomFactors );
    setOfferedPanFactors( offeredPanFactors );
    setOfferedProjects( offeredProjects );

    this.minScale = minScale;
    this.maxScale = maxScale;
    this.gmr = gmr;

    this.wmsCapabilities = wmsCapabilities;
  }

  /**
   * @see java.lang.Object#clone()
   */
  public Object clone()
  {

    MapSize[] ms = new MapSize[offeredMapSizes.size()];
    for( int i = 0; i < ms.length; i++ )
    {
      MapSize m = (MapSize)offeredMapSizes.get( i );
      ms[i] = new MapSize( m.getWidth(), m.getHeight(), m.isSelected(), m.isFree() );
    }

    Format[] mapFormat = new Format[offeredMapFormats.size()];
    for( int i = 0; i < mapFormat.length; i++ )
    {
      Format f = (Format)offeredMapFormats.get( i );
      mapFormat[i] = new Format( f.getName(), f.isSelected() );
    }

    Format[] infoFormat = new Format[offeredInfoFormats.size()];
    for( int i = 0; i < infoFormat.length; i++ )
    {
      Format f = (Format)offeredInfoFormats.get( i );
      infoFormat[i] = new Format( f.getName(), f.isSelected() );
    }

    MapOperation[] mapOp = new MapOperation[offeredMapOperations.size()];
    for( int i = 0; i < mapOp.length; i++ )
    {
      MapOperation mo = (MapOperation)offeredMapOperations.get( i );
      mapOp[i] = new MapOperation( mo.getName(), mo.isSelected() );
    }

    MapOperationFactor[] zoom = new MapOperationFactor[offeredZoomFactors.size()];
    for( int i = 0; i < zoom.length; i++ )
    {
      MapOperationFactor mo = (MapOperationFactor)offeredZoomFactors.get( i );
      zoom[i] = new MapOperationFactor( mo.getFactor(), mo.isSelected(), mo.isFree() );
    }

    MapOperationFactor[] pan = new MapOperationFactor[offeredPanFactors.size()];
    for( int i = 0; i < pan.length; i++ )
    {
      MapOperationFactor mo = (MapOperationFactor)offeredPanFactors.get( i );
      pan[i] = new MapOperationFactor( mo.getFactor(), mo.isSelected(), mo.isFree() );
    }

    Project[] projects = new Project[offeredProjects.size()];
    for( int i = 0; i < projects.length; i++ )
    {
      Project mo = (Project)offeredProjects.get( i );
      projects[i] = new Project( mo.getName(), mo.getInitialWMSGetMapRequest() );
    }

    return new WMSClientConfiguration( defaultConfig, wmsCapabilities, ms, mapFormat, infoFormat,
        mapOp, zoom, pan, minScale, maxScale, gmr, projects );
  }

  /**
   * returns the encapsulated capabilites of all WMS registered to the client
   * 
   * @return <tt>WMSCapabilities</tt> obejcts for all registered WMS
   */
  public WMSCapabilities[] getWMSCapabilities()
  {
    if( wmsCapabilities.size() > 0 )
    {
      WMSCapabilities[] capa = new WMSCapabilities[wmsCapabilities.size()];
      return (WMSCapabilities[])wmsCapabilities.values().toArray( capa );
    }
    return defaultConfig.getWMSCapabilities();
  }

  /**
   * returns the <tt>WMSCapabilities</tt> object for the WMS identified by the
   * submitted id. If no WMS with the submitted id is known by the client
   * <tt>null</tt> will be returned
   * 
   * @param id
   *          ID of the WMS which capabilities shall be returned
   * 
   * @return capabilities of the desired WMS or <tt>null</tt>
   */
  public WMSCapabilities getWMSCapabilities( String id )
  {
    WMSCapabilities capa = (WMSCapabilities)wmsCapabilities.get( id );

    if( capa == null )
    {
      capa = defaultConfig.getWMSCapabilities( id );
    }

    return capa;
  }

  /**
   * adds a new WMS to the client. The submitted URL is a reference to the
   * capabilities of the WMS (shall be version 1.1.0 or higher)
   * 
   * @param id
   *          ID of the WMS
   * @param url
   *          reference to the capabilities of the WMS
   */
  public void addWMS( String id, URL url ) throws XMLParsingException
  {
    OGCWMSCapabilitiesFactory cF = new OGCWMSCapabilitiesFactory();
    WMSCapabilities capa = cF.createCapabilities( url );
    addWMS( id, capa );
  }

  /**
   * adds a new WMS the the client.
   * 
   * @param id
   *          ID of the WMS
   * @param capabilities
   *          capabilities of the WMS
   */
  public void addWMS( String id, WMSCapabilities capabilities )
  {
    wmsCapabilities.put( id, capabilities );
  }

  /**
   * 
   * 
   * @param mapSizes
   */
  public void setOfferedMapSizes( MapSize[] mapSizes )
  {
    offeredMapSizes.clear();

    if( mapSizes != null )
    {
      for( int i = 0; i < mapSizes.length; i++ )
      {
        addMapSize( mapSizes[i] );
      }
    }
  }

  /**
   * 
   * 
   * @param mapSize
   */
  public void addMapSize( MapSize mapSize )
  {
    offeredMapSizes.add( mapSize );
  }

  /**
   * returns the list of map sizes offered by the client
   */
  public MapSize[] getOfferedMapSizes()
  {
    MapSize[] ms = new MapSize[0];
    if( offeredMapSizes.size() == 0 && defaultConfig != null )
    {
      ms = defaultConfig.getOfferedMapSizes();
    }
    else
    {
      ms = new MapSize[offeredMapSizes.size()];
      ms = (MapSize[])offeredMapSizes.toArray( ms );
    }

    return ms;
  }

  /**
   * returns the <tt>MapSize</tt> that is marked as selected. If no
   * <tt>MapSize</tt> is marked, the first <tt>MapSize</tt> will be
   * returned.
   */
  public MapSize getSelectedMapSize()
  {
    MapSize[] mss = getOfferedMapSizes();
    MapSize ms = mss[0];
    for( int i = 0; i < mss.length; i++ )
    {
      if( mss[i].isSelected() )
      {
        ms = mss[i];
        break;
      }
    }
    return ms;
  }

  /**
   * 
   * 
   * @param panFactors
   */
  public void setOfferedPanFactors( MapOperationFactor[] panFactors )
  {
    offeredPanFactors.clear();

    if( panFactors != null )
    {
      for( int i = 0; i < panFactors.length; i++ )
      {
        addPanFactor( panFactors[i] );
      }
    }
  }

  /**
   * 
   * 
   * @param panFactor
   */
  public void addPanFactor( MapOperationFactor panFactor )
  {
    offeredPanFactors.add( panFactor );
  }

  /**
   * returns the list of pan factors offered by the client
   */
  public MapOperationFactor[] getOfferedPanFactors()
  {
    MapOperationFactor[] ms = new MapOperationFactor[0];

    if( offeredPanFactors.size() == 0 && defaultConfig != null )
    {
      ms = defaultConfig.getOfferedPanFactors();
    }
    else
    {
      ms = new MapOperationFactor[offeredPanFactors.size()];
      ms = (MapOperationFactor[])offeredPanFactors.toArray( ms );
    }

    return ms;
  }

  /**
   * returns the pan factor that is marked as selected. If no pan factor is
   * marked, the first pan factor will be returned.
   */
  public MapOperationFactor getSelectedPanFactor()
  {
    MapOperationFactor[] mss = getOfferedPanFactors();
    MapOperationFactor ms = mss[0];
    for( int i = 0; i < mss.length; i++ )
    {
      if( mss[i].isSelected() )
      {
        ms = mss[i];
        break;
      }
    }
    return ms;
  }

  /**
   * 
   * 
   * @param zoomFactors
   */
  public void setOfferedZoomFactors( MapOperationFactor[] zoomFactors )
  {
    offeredZoomFactors.clear();

    if( zoomFactors != null )
    {
      for( int i = 0; i < zoomFactors.length; i++ )
      {
        addZoomFactor( zoomFactors[i] );
      }
    }
  }

  /**
   * 
   * 
   * @param zoomFactor
   */
  public void addZoomFactor( MapOperationFactor zoomFactor )
  {
    offeredZoomFactors.add( zoomFactor );
  }

  /**
   * returns the list of zoom factors offered by the client
   */
  public MapOperationFactor[] getOfferedZoomFactors()
  {
    MapOperationFactor[] ms = new MapOperationFactor[0];

    if( offeredZoomFactors.size() == 0 && defaultConfig != null )
    {
      ms = defaultConfig.getOfferedZoomFactors();
    }
    else
    {
      ms = new MapOperationFactor[offeredZoomFactors.size()];
      ms = (MapOperationFactor[])offeredZoomFactors.toArray( ms );
    }

    return ms;
  }

  /**
   * returns the zoom factor that is marked as selected. If no zoom factor is
   * marked, the first zoom factor will be returned.
   */
  public MapOperationFactor getSelectedZoomFactor()
  {
    MapOperationFactor[] mss = getOfferedZoomFactors();
    MapOperationFactor ms = mss[0];
    for( int i = 0; i < mss.length; i++ )
    {
      if( mss[i].isSelected() )
      {
        ms = mss[i];
        break;
      }
    }
    return ms;
  }

  /**
   * 
   * 
   * @param mapOperations
   */
  public void setOfferedMapOperations( MapOperation[] mapOperations )
  {
    offeredMapOperations.clear();

    if( mapOperations != null )
    {
      for( int i = 0; i < mapOperations.length; i++ )
      {
        addMapOperation( mapOperations[i] );
      }
    }
  }

  /**
   * 
   * 
   * @param mapOperation
   */
  public void addMapOperation( MapOperation mapOperation )
  {
    offeredMapOperations.add( mapOperation );
  }

  /**
   * returns the list of map operations offered by the client
   */
  public MapOperation[] getOfferedMapOperations()
  {
    MapOperation[] ms = new MapOperation[0];

    if( offeredMapOperations.size() == 0 && defaultConfig != null )
    {
      ms = defaultConfig.getOfferedMapOperations();
    }
    else
    {
      ms = new MapOperation[offeredMapOperations.size()];
      ms = (MapOperation[])offeredMapOperations.toArray( ms );
    }

    return ms;
  }

  /**
   * returns the map operation that is marked as selected. If no map operation
   * is marked, the first map operation will be returned.
   */
  public MapOperation getSelectedMapOperation()
  {
    MapOperation[] mss = getOfferedMapOperations();
    MapOperation ms = mss[0];
    for( int i = 0; i < mss.length; i++ )
    {
      if( mss[i].isSelected() )
      {
        ms = mss[i];
        break;
      }
    }
    return ms;
  }

  /**
   * sets the submitted operation as selected
   */
  public void setSelectedMapOperation( String name )
  {
    MapOperation[] mss = getOfferedMapOperations();
    for( int i = 0; i < mss.length; i++ )
    {
      if( name.equals( mss[i].getName() ) )
      {
        mss[i].setSelected( true );
      }
      else
      {
        mss[i].setSelected( false );
      }
    }
  }

  /**
   * 
   * 
   * @param mapFormats
   */
  public void setOfferedMapFormats( Format[] mapFormats )
  {
    offeredMapFormats.clear();

    if( mapFormats != null )
    {
      for( int i = 0; i < mapFormats.length; i++ )
      {
        addMapFormat( mapFormats[i] );
      }
    }
  }

  /**
   * 
   * 
   * @param mapFormat
   */
  public void addMapFormat( Format mapFormat )
  {
    offeredMapFormats.add( mapFormat );
  }

  /**
   * returns the list of map formats offered by the client
   */
  public Format[] getOfferedMapFormats()
  {
    Format[] ms = new Format[0];

    if( offeredMapFormats.size() == 0 && defaultConfig != null )
    {
      ms = defaultConfig.getOfferedMapFormats();
    }
    else
    {
      ms = new Format[offeredMapFormats.size()];
      ms = (Format[])offeredMapFormats.toArray( ms );
    }

    return ms;
  }

  /**
   * returns the map format that is marked as selected. If no map format is
   * marked, the first map format will be returned.
   */
  public Format getSelectedMapFormat()
  {
    Format[] mss = getOfferedMapFormats();
    Format ms = mss[0];
    for( int i = 0; i < mss.length; i++ )
    {
      if( mss[i].isSelected() )
      {
        ms = mss[i];
        break;
      }
    }
    return ms;
  }

  /**
   * 
   * 
   * @param infoFormats
   */
  public void setOfferedInfoFormats( Format[] infoFormats )
  {
    offeredInfoFormats.clear();

    if( infoFormats != null )
    {
      for( int i = 0; i < infoFormats.length; i++ )
      {
        addInfoFormat( infoFormats[i] );
      }
    }
  }

  /**
   * 
   * 
   * @param infoFormat
   */
  public void addInfoFormat( Format infoFormat )
  {
    offeredInfoFormats.add( infoFormat );
  }

  /**
   * returns the list of map formats offered by the client
   */
  public Format[] getOfferedInfoFormats()
  {
    Format[] ms = new Format[0];

    if( offeredInfoFormats.size() == 0 && defaultConfig != null )
    {
      ms = defaultConfig.getOfferedInfoFormats();
    }
    else
    {
      ms = new Format[offeredInfoFormats.size()];
      ms = (Format[])offeredInfoFormats.toArray( ms );
    }

    return ms;
  }

  /**
   * returns the info format that is marked as selected. If no info format is
   * marked, the first info format will be returned.
   */
  public Format getSelectedInfoFormat()
  {
    Format[] mss = getOfferedInfoFormats();
    Format ms = mss[0];
    for( int i = 0; i < mss.length; i++ )
    {
      if( mss[i].isSelected() )
      {
        ms = mss[i];
        break;
      }
    }
    return ms;
  }

  /**
   * 
   * 
   * @param projects
   */
  public void setOfferedProjects( Project[] projects )
  {
    offeredProjects.clear();

    if( projects != null )
    {
      for( int i = 0; i < projects.length; i++ )
      {
        addProject( projects[i] );
      }
    }
  }

  /**
   * 
   * 
   * @param project
   */
  public void addProject( Project project )
  {
    offeredProjects.add( project );
  }

  /**
   * returns the list of projects offered by the client
   */
  public Project[] getOfferedProjects()
  {
    Project[] ms = new Project[0];

    if( offeredProjects.size() == 0 && defaultConfig != null )
    {
      ms = defaultConfig.getOfferedProjects();
    }
    else
    {
      ms = new Project[offeredProjects.size()];
      ms = (Project[])offeredProjects.toArray( ms );
    }

    return ms;
  }

  /**
   * returns the project identified by its name. If no project with the
   * submitted name is registered <tt>null</tt> will be returned
   */
  public Project getProject( String name )
  {
    Project project_ = null;
    for( int i = 0; i < offeredProjects.size(); i++ )
    {
      Project project = (Project)offeredProjects.get( i );
      if( project.getName().equals( name ) )
      {
        project_ = project;
        break;
      }
    }
    return project_;
  }

  /**
   * returns the minimum map scale as defined at the OGC WMS specs that is
   * offered by the client
   * 
   * @return
   */
  public double getMinScale()
  {
    return minScale;
  }

  /**
   * returns the maximum map scale as defined at the OGC WMS specs that is
   * offered by the client
   * 
   * @return
   */
  public double getMaxScale()
  {
    return maxScale;
  }

  /**
   * returns the initial get map request
   * 
   * @return
   */
  public WMSGetMapRequest getInitialGetMapRequest()
  {
    return gmr;
  }

  /**
   * Produces an XML-representation of this object.
   * <p>
   * 
   * @return XML-representation of this object
   *  
   */
  public String exportAsXML()
  {
    return null;
  }
}