// $Header:
// /cvsroot/deegree/deegree/org/deegree/services/gazetteer/GazetteerException.java,v
// 1.4 2004/03/15 16:55:28 poth Exp $
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
package org.deegree.services.gazetteer;

import org.deegree_impl.tools.StringExtend;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class GazetteerException extends Exception
{

  String st = null;

  /**
   * Creates a new instance of <code>GazetteerException</code> without detail
   * message.
   */
  public GazetteerException()
  {
    st = "GazetteerException";
  }

  /**
   * Constructs an instance of <code>GazetteerException</code> with the
   * specified detail message.
   * 
   * @param msg
   *          the detail message.
   */
  public GazetteerException( String msg )
  {
    super( msg );
    st = "GazetteerException";
  }

  /**
   * Constructs an instance of <code>GazetteerException</code> with the
   * specified detail message.
   * 
   * @param msg
   *          the detail message.
   */
  public GazetteerException( String msg, Exception e )
  {
    this( msg );
    st = StringExtend.stackTraceToString( e.getStackTrace() );
  }

}