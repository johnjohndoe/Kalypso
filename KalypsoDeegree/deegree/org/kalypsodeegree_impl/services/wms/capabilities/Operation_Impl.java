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
package org.deegree_impl.services.wms.capabilities;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.deegree.services.capabilities.DCPType;
import org.deegree.services.wms.capabilities.Format;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;

/**
 * The interface <Operation>defines the differt types of operations that may be
 * performed by a map server, their access addresses and formats.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-03-08
 */
class Operation_Impl implements Operation, Marshallable
{
  private ArrayList dCPType = null;

  private HashMap formats = null;

  private String operationName = null;

  private int operationType = 0;

  private String responsibleClass = null;

  /**
   * default constructor
   */
  Operation_Impl()
  {
    formats = new HashMap();
    dCPType = new ArrayList();
  }

  /**
   * constructor initializing the class with the <Operation>and uses default
   * responsible classes
   */
  Operation_Impl( String operationName, Format[] formats, DCPType[] types )
  {
    this();
    setOperationName( operationName );
    setDCPType( types );
    setFormats( formats );
  }

  /**
   * constructor initializing the class with the <Operation>
   */
  Operation_Impl( String operationName, Format[] formats, DCPType[] types, String responsibleClass )
  {
    this();
    setOperationName( operationName );
    setDCPType( types );
    setFormats( formats );
    this.responsibleClass = responsibleClass;
  }

  /**
   * Returns the specified <tt>Format</tt> (currently as a <tt>String</tt>),
   * if the <tt>Operation</tt> supports it, else null.
   * <p>
   * 
   * @param format
   *          the name of the <tt>Format</tt> to look up
   * @return the name of the <tt>Format</tt>, null if it is not supported
   */
  public Format getFormat( String format )
  {
    return (Format)formats.get( format );
  }

  /**
   * Adds a format to the <tt>Operation</tt>'s formats (if it is not defined
   * yet).
   * <p>
   * 
   * @param format
   *          the name of the format to add
   */
  public void addFormat( Format format )
  {
    if( formats.get( format.getName() ) == null )
    {
      formats.put( format.getName(), format );
    }
  }

  /**
   * returns the formats a operation is able to return its results
   */
  public Format[] getFormats()
  {
    return (Format[])formats.values().toArray( new String[formats.size()] );
  }

  /**
   * adds the formats a operation is able to return its results
   */
  public void setFormats( Format[] formats )
  {
    this.formats.clear();

    for( int i = 0; i < formats.length; i++ )
    {
      this.formats.put( formats[i].getName(), formats[i] );
    }
  }

  /**
   * returns the available Distributed Computing Platforms (DCPs) for a
   * operation. At present, only HTTP (GET & POST) is defined.
   */
  public DCPType[] getDCPTypes()
  {
    return (DCPType[])dCPType.toArray( new DCPType[dCPType.size()] );
  }

  /**
   * adds the available Distributed Computing Platforms (DCPs) for a operation.
   * At present, only HTTP (GET & POST) is defined.
   */
  public void addDCPType( DCPType dCPType )
  {
    this.dCPType.add( dCPType );
  }

  /**
   * adds the available Distributed Computing Platforms (DCPs) for a operation.
   * At present, only HTTP (GET & POST) is defined.
   */
  public void setDCPType( DCPType[] dCPTypes )
  {
    this.dCPType.clear();

    for( int i = 0; i < dCPTypes.length; i++ )
    {
      this.dCPType.add( dCPTypes[i] );
    }
  }

  /**
   * returns the operation type defnied above. If the operation isn't known
   * <tt>Operation.UNKNOWN</tt> (-1) will be returned.
   */
  public int getOperationType()
  {
    return operationType;
  }

  /**
   * sets the operation type defnied above. If the operation isn't known
   * <tt>Operation.UNKNOWN</tt> (-1) will be returned.
   */
  public void setOperationType( int operationType )
  {
    this.operationType = operationType;

    switch( operationType )
    {
    case GETCAPABILITIES:
      operationName = GETCAPABILITIES_NAME;
      break;
    case CAPABILITIES:
      operationName = CAPABILITIES_NAME;
      break;
    case GETMAP:
      operationName = GETMAP_NAME;
      responsibleClass = "org.deegree_impl.services.wms.GetMapHandler";
      break;
    case MAP:
      operationName = MAP_NAME;
      responsibleClass = "org.deegree_impl.services.wms.GetMapHandler";
      break;
    case GETFEATUREINFO:
      operationName = GETFEATUREINFO_NAME;
      responsibleClass = "org.deegree_impl.services.wms.GetFeatureInfoHandler";
      break;
    case FEATUREINFO:
      operationName = FEATUREINFO_NAME;
      responsibleClass = "org.deegree_impl.services.wms.GetFeatureInfoHandler";
      break;
    case DESCRIBELAYER:
      operationName = DESCRIBELAYER_NAME;
      break;
    case GETLEGENDGRAPHIC:
      operationName = GETLEGENDGRAPHIC_NAME;
      break;
    case GETSTYLES:
      operationName = GETSTYLES_NAME;
      break;
    case PUTSTYLES:
      operationName = PUTSTYLES_NAME;
      break;
    default:
      operationName = UNKNOWN_NAME;
      break;
    }
  }

  /**
   * returns the name of the operation defined above.
   */
  public String getOperationName()
  {
    return operationName;
  }

  /**
   * sets the name of the operation defined above.
   */
  public void setOperationName( String operationName )
  {
    this.operationName = operationName;

    if( operationName.equals( GETCAPABILITIES_NAME ) )
    {
      operationType = GETCAPABILITIES;
    }
    else if( operationName.equals( CAPABILITIES_NAME ) )
    {
      operationType = CAPABILITIES;
    }
    else if( operationName.equals( GETMAP_NAME ) )
    {
      operationType = GETMAP;
      responsibleClass = "org.deegree_impl.services.wms.GetMapHandler";
    }
    else if( operationName.equals( MAP_NAME ) )
    {
      operationType = MAP;
      responsibleClass = "org.deegree_impl.services.wms.GetMapHandler";
    }
    else if( operationName.equals( GETFEATUREINFO_NAME ) )
    {
      operationType = GETFEATUREINFO;
      responsibleClass = "org.deegree_impl.services.wms.GetFeatureInfoHandler";
    }
    else if( operationName.equals( FEATUREINFO_NAME ) )
    {
      operationType = FEATUREINFO;
      responsibleClass = "org.deegree_impl.services.wms.GetFeatureInfoHandler";
    }
    else if( operationName.equals( DESCRIBELAYER_NAME ) )
    {
      operationType = DESCRIBELAYER;
    }
    else if( operationName.equals( GETLEGENDGRAPHIC_NAME ) )
    {
      operationType = GETLEGENDGRAPHIC;
    }
    else if( operationName.equals( GETSTYLES_NAME ) )
    {
      operationType = GETSTYLES;
    }
    else if( operationName.equals( PUTSTYLES_NAME ) )
    {
      operationType = PUTSTYLES;
    }
    else
    {
      operationType = UNKNOWN;
    }
  }

  /**
   * 
   * 
   * @param operationType
   * 
   * @return
   */
  public static String getName( int operationType )
  {
    String s = null;

    switch( operationType )
    {
    case GETCAPABILITIES:
      s = GETCAPABILITIES_NAME;
      break;
    case CAPABILITIES:
      s = CAPABILITIES_NAME;
      break;
    case GETMAP:
      s = GETMAP_NAME;
      break;
    case MAP:
      s = MAP_NAME;
      break;
    case GETFEATUREINFO:
      s = GETFEATUREINFO_NAME;
      break;
    case FEATUREINFO:
      s = FEATUREINFO_NAME;
      break;
    case DESCRIBELAYER:
      s = DESCRIBELAYER_NAME;
      break;
    case GETLEGENDGRAPHIC:
      s = GETLEGENDGRAPHIC_NAME;
      break;
    case GETSTYLES:
      s = GETSTYLES_NAME;
      break;
    case PUTSTYLES:
      s = PUTSTYLES_NAME;
      break;
    default:
      s = UNKNOWN_NAME;
    }

    return s;
  }

  /**
   * returns the name of the class that's responsible for handling the operation
   * (request). Defaults are:
   * <ul>
   * <li>GetMap -> org.deegree_impl.services.wms.GetMapHandler
   * <li>GetFeatureInfo -> org.deegree_impl.services.wms.GetFeatureInfoHandler
   * </ul>
   * For GetCapabilities operation no handler is required.
   */
  public String getResponsibleClass()
  {
    return responsibleClass;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "operationType = " + operationType + "\n";
    ret += ( "operationName = " + operationName + "\n" );
    ret += ( "formats = " + formats + "\n" );
    ret += ( "dCPType = " + dCPType + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<" ).append( operationName ).append( ">" );

    Iterator it = formats.values().iterator();

    while( it.hasNext() )
    {
      Format fo = (Format)it.next();
      sb.append( "<Format>" ).append( XMLTools.validateCDATA( fo.getName() ) ).append( "</Format>" );
    }

    it = dCPType.iterator();

    while( it.hasNext() )
    {
      sb.append( ( (Marshallable)it.next() ).exportAsXML() );
    }

    sb.append( "</" ).append( operationName ).append( ">" );

    return sb.toString();
  }

}