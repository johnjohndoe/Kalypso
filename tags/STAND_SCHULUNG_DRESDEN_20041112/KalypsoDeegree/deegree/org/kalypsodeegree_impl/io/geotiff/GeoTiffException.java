// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/io/geotiff/GeoTiffException.java,v
// 1.2 2004/07/15 09:57:23 axel_schaefer Exp $
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
package org.deegree_impl.io.geotiff;

/**
 * The GeoTIFF exception. Thrown in the context of this package of the GeoTIFF
 * Writer and Reader.
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </A>
 * @author last edited by: $Author$
 * @version 2.0. $Revision$, $Date$
 * @see link to internal or external resource
 * @since
 */
public class GeoTiffException extends Exception
{

  private String message = "GeoTiffException";

  /**
   * 
   * @param message
   */
  public GeoTiffException( String message )
  {
    this.message = message;
  }

  /**
   *  
   */
  public String getMessage()
  {
    return this.message;
  }

  /**
   *  
   */
  public String toString()
  {
    return message + "\n" + getLocalizedMessage();
  }

}

/*
 * ****************************************************************************
 * Changes to this class. What the people have been up to:
 * 
 * $Log$
 * Revision 1.1  2004/10/07 14:09:20  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:51  doemming
 * *** empty log message ***
 * Revision 1.1 2004/08/31 12:41:08 doemming ***
 * empty log message *** Revision 1.2 2004/07/15 09:57:23 axel_schaefer no
 * message
 * 
 * ****************************************************************************
 */