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
package org.deegree_impl.services.wcts.capabilities;

import java.util.ArrayList;

import org.deegree.services.capabilities.DCPType;
import org.deegree.services.wcts.capabilities.ActionType;

/**
 * <p>
 * &lt;GetCapabilities&gt;, &lt;Transform&gt;, &lt;IsTransformable&gt; and the
 * optional &lt;DescribeTransformation&gt; consist the same two elements, which
 * are included in this class: the &lt;Format&gt;-element and the
 * &lt;DCPType&gt;-element.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-07-10
 */
public class ActionType_Impl implements ActionType
{
  private ArrayList dCPTypes = null;

  private ArrayList format = null;

  /**
   * constructor initializing the class with the &lt;ActionType&gt;
   */
  ActionType_Impl( String[] format, DCPType[] dCPTypes )
  {
    this.format = new ArrayList();
    this.dCPTypes = new ArrayList();

    setFormat( format );
    setDCPType( dCPTypes );
  }

  /**
   * returns the &lt;Format&gt; element
   */
  public String[] getFormat()
  {
    return (String[])format.toArray( new String[format.size()] );
  }

  /**
   * adds ;
   */
  public void addFormat( String format )
  {
    this.format.add( format );
  }

  /**
   * sets the &lt;knownTransformationTypes&gt;
   */
  public void setFormat( String[] format )
  {
    this.format.clear();

    if( format != null )
    {
      for( int i = 0; i < format.length; i++ )
      {
        this.format.add( format[i] );
      }
    }
  }

  /**
   * returns the &lt;DCPType&gt; element
   */
  public DCPType[] getDCPTypes()
  {
    return (DCPType[])dCPTypes.toArray( new DCPType[dCPTypes.size()] );
  }

  /**
   * @see ActionType_Impl#getDCPTypes()
   */
  public void addDCPType( DCPType dcpType )
  {
    this.dCPTypes.add( dcpType );
  }

  /**
   * @see ActionType_Impl#getDCPTypes()
   */
  public void setDCPType( DCPType[] dCPTypes )
  {
    this.dCPTypes.clear();

    if( dCPTypes != null )
    {
      for( int i = 0; i < dCPTypes.length; i++ )
      {
        this.dCPTypes.add( dCPTypes[i] );
      }
    }
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "format = " + format + "\n";
    ret += ( "dCPTypes = " + dCPTypes + "\n" );
    return ret;
  }
}