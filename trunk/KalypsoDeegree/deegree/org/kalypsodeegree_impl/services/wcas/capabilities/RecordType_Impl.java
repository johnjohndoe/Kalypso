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

package org.deegree_impl.services.wcas.capabilities;

import java.net.URL;
import java.util.ArrayList;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.capabilities.MetadataURL;
import org.deegree.services.wcas.capabilities.Operation;
import org.deegree.services.wcas.capabilities.RecordType;
import org.deegree.services.wcas.capabilities.RecordTypeList;

/**
 * The main purpose of the &lt;RecordTypeList&gt; section is to define the list
 * of record types that a WFS can service and define the operations that are
 * supported on each record type. For possible operations see the Operations
 * interface.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public class RecordType_Impl implements RecordType
{

  private String name = null;

  private String title = null;

  private String abstract_ = null;

  private String srs = null;

  private GM_Envelope latLonBoundingBox = null;

  private RecordTypeList parentList = null;

  private String responsibleClassName = null;

  private URL configURL = null;

  private ArrayList keyword = null;

  private ArrayList operation = null;

  private ArrayList metadataURL = null;

  /**
   * constructor initializing the class with the <RecordType>
   */
  RecordType_Impl( String name, String title, String abstract_, String srs,
      GM_Envelope latLonBoundingBox, RecordTypeList parentList, String[] keyword,
      Operation[] operation, MetadataURL[] metadataURL, String responsibleClassName, URL configURL )
  {
    this.keyword = new ArrayList();
    this.metadataURL = new ArrayList();
    this.operation = new ArrayList();
    setName( name );
    setTitle( title );
    setAbstract( abstract_ );
    setSrs( srs );
    setLatLonBoundingBox( latLonBoundingBox );
    setParentList( parentList );
    setResponsibleClassName( responsibleClassName );
    setConfigURL( configURL );
    setKeyword( keyword );
    setOperations( operation );
    setMetadataURL( metadataURL );
  }

  /**
   * The name of the record type. This element is mandatory.
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the name of the record type.
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * The &lt;Title&gt; is a human-readable title to briefly identify this record
   * type in menus.
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * sets the title
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * The &lt;Abstract&gt; is a descriptive narrative for more information about
   * the record type.
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * sets the abstract
   */
  public void setAbstract( String abstract_ )
  {
    this.abstract_ = abstract_;
  }

  /**
   * The &lt;Keyword&gt; element delimits short words to aid catalog searching.
   */
  public String[] getKeywords()
  {
    return (String[])keyword.toArray( new String[keyword.size()] );
  }

  /**
   * adds the keyword
   */
  public void addKeyword( String keyword )
  {
    this.keyword.add( keyword );
  }

  /**
   * sets the keyword
   */
  public void setKeyword( String[] keyword )
  {
    this.keyword.clear();
    if( keyword != null )
    {
      for( int i = 0; i < keyword.length; i++ )
      {
        this.keyword.add( keyword[i] );
      }
    }
  }

  /**
   * The &lt;SRS&gt; element is used to indicate which spatial reference system
   * should be used to express the state of a record.
   */
  public String getSrs()
  {
    return srs;
  }

  /**
   * sets the srs
   */
  public void setSrs( String srs )
  {
    this.srs = srs;
  }

  /**
   * The &lt;Operations&gt; element defines which are operations are supported
   * on a record type. Any locally defined operations take precedence over any
   * globally defined operations.
   */
  public Operation[] getOperations()
  {
    return (Operation[])operation.toArray( new Operation[operation.size()] );
  }

  /**
   * adds the operation
   */
  public void addOperation( Operation operation )
  {
    this.operation.add( operation );
  }

  /**
   * sets the operation
   */
  public void setOperations( Operation[] operation )
  {
    this.operation.clear();
    if( operation != null )
    {
      for( int i = 0; i < operation.length; i++ )
      {
        addOperation( operation[i] );
      }
    }
  }

  /**
   * The LatLonBoundingBox attributes indicate the edges of the enclosing
   * rectangle in latitude/longitude decimal degrees (as in SRS EPSG:4326
   * [WGS1984 lat/lon]). Its purpose is to facilitate geographic searches
   * without requiring coordinate transformations by the search engine.
   */
  public GM_Envelope getLatLonBoundingBox()
  {
    return latLonBoundingBox;
  }

  /**
   * sets the LatLonBoundingBox
   */
  public void setLatLonBoundingBox( GM_Envelope latLonBoundingBox )
  {
    this.latLonBoundingBox = latLonBoundingBox;
  }

  /**
   * A WFS may use zero or more &lt;MetadataURL&gt; elements to offer detailed,
   * standardized metadata about the data inaparticularrecord type.The type
   * attribute indicates the standard to which the metadata complies; the format
   * attribute indicates how the metadata is structured. Two types are defined
   * at present: �TC211� = ISO TC211 19115; FGDC = FGDC CSDGM.
   */
  public MetadataURL[] getMetadataURL()
  {
    return (MetadataURL[])metadataURL.toArray( new MetadataURL[metadataURL.size()] );
  }

  /**
   * adds the metadataURL
   */
  public void addMetadataURL( MetadataURL metadataURL )
  {
    this.metadataURL.add( metadataURL );
  }

  /**
   * sets the metadataURL
   */
  public void setMetadataURL( MetadataURL[] metadataURL )
  {
    this.metadataURL.clear();
    if( metadataURL != null )
    {
      for( int i = 0; i < metadataURL.length; i++ )
      {
        this.metadataURL.add( metadataURL[i] );
      }
    }
  }

  /**
   * returns an instance of the record type list the record belongs to.
   */
  public RecordTypeList getParentList()
  {
    return parentList;
  }

  /**
   * sets an instance of the record type list the record belongs to.
   */
  public void setParentList( RecordTypeList parentList )
  {
    this.parentList = parentList;
  }

  /**
   * returns the name of the class that's responsible for performing the
   * operation.
   */
  public String getResponsibleClassName()
  {
    return responsibleClassName;
  }

  /**
   * sets the name of the responsible class for handling this operation.
   */
  public void setResponsibleClassName( String responsibleClassName )
  {
    this.responsibleClassName = responsibleClassName;
  }

  /**
   * sets the name of the configuration file of the responsible class
   */
  public void setConfigURL( URL configURL )
  {
    this.configURL = configURL;
  }

  /**
   * returns the name of the configuration file of the responsible class
   */
  public URL getConfigURL()
  {
    return configURL;
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:04  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:01  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:53:32 doemming ***
 * empty log message *** Revision 1.1.1.1 2002/09/25 16:01:30 poth no message
 * 
 * Revision 1.1 2002/08/19 15:58:38 ap no message
 *  
 */
