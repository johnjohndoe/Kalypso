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

import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.services.capabilities.DCPType;
import org.deegree.services.wcas.capabilities.GetRecord;
import org.deegree.services.wfs.GetFeatureResponseHandler;

/**
 * The &lt;GetFeature&gt; tag isused todefine the formats available for
 * expressing the results of a query. The RESULTFORMATS entity defines the
 * mandatory output format of GML but can be redefined to include additional
 * vendor specific formats.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public class GetRecord_Impl extends RequestType_Impl implements GetRecord
{

  private ArrayList resultFormat = null;

  private HashMap classes = null;

  /**
   * constructor initializing the class with the <FeatureType>
   */
  GetRecord_Impl( DCPType[] dcpTypes, String[] resultFormat, String[] classNames )
  {
    super( dcpTypes );
    this.resultFormat = new ArrayList();
    this.classes = new HashMap();

    setResultFormat( resultFormat );
    setClasses( resultFormat, classNames );
  }

  /**
   * gets the ResultFormat
   */
  public String[] getResultFormat()
  {
    return (String[])resultFormat.toArray( new String[resultFormat.size()] );
  }

  /**
   * adds a result format to the GetFeature operation
   */
  public void addResultFormat( String resultFormat )
  {
    this.resultFormat.add( resultFormat );
  }

  /**
   * sets the ResultFormat
   */
  public void setResultFormat( String[] resultFormat )
  {
    this.resultFormat.clear();
    if( resultFormat != null )
    {
      for( int i = 0; i < resultFormat.length; i++ )
      {
        addResultFormat( resultFormat[i] );
      }
    }
  }

  /**
   * the method returns the class that is responsible for handling/creating the
   * submitted format. This enables an implementation of the deegree CAS to add
   * the handling for new formats dynamicly by editing the its capabilities
   * document.
   */
  public GetFeatureResponseHandler getClassForFormat( String format )
  {
    return (GetFeatureResponseHandler)classes.get( format );
  }

  /**
   * sets the classes that are responsible for handling/creating the submitted
   * format.
   */
  public void setClasses( String formats[], String[] classNames )
  {
    classes.clear();
    if( classNames != null )
    {
      for( int i = 0; i < classNames.length; i++ )
      {
        addClassByName( formats[i], classNames[i] );
      }
    }
  }

  /**
   * adds a class that is responsible for handling/creating the submitted
   * format.
   */
  public void addClassByName( String format, String className )
  {
    try
    {
      Class class_ = Class.forName( className );
      classes.put( format, class_.newInstance() );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret = "resultFormat = " + resultFormat + "\n";
    ret += "classes = " + classes + "\n";
    return ret;
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
 * Revision 1.3 2004/08/31 12:53:31 doemming ***
 * empty log message *** Revision 1.2 2004/01/08 09:50:23 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:30 poth no message
 * 
 * Revision 1.1 2002/08/19 15:58:38 ap no message
 * 
 *  
 */
