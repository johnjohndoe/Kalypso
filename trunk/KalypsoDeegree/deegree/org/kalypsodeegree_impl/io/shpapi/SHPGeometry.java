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

package org.deegree_impl.io.shpapi;

/**
 * Class representing basic Geometry for geometries read from a shape-file.
 * <p>
 * 
 * <B>Last changes <B>: <BR>
 * 12.01.2000 ap: constructor re-declared <BR>
 * 13.01.2000 ap: all methods removed <BR>
 * 13.01.2000 ap: all variables except reBuffer removed <BR>
 * 16.08.2000 ap: field enevlope added <BR>
 * 16.08.2000 ap: method getEnvelope() added <BR>
 * 
 * <!---------------------------------------------------------------------------->
 * 
 * @author Andreas Poth
 * @version 16.08.2000
 */

public class SHPGeometry
{

  protected byte[] recBuffer = null;

  protected SHPEnvelope envelope = null;

  public SHPGeometry()
  {}

  public SHPGeometry( byte[] recBuf )
  {

    /* private copy of record Buffer */
    recBuffer = recBuf;

  } // end of constructor

  /**
   * returns the minimum boundary rectangle of the geomertry <BR>
   */
  public SHPEnvelope getEnvelope()
  {
    return envelope;
  }

} // end of class SHPGeometry
