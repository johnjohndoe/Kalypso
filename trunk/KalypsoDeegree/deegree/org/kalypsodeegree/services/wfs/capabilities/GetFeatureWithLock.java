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
package org.deegree.services.wfs.capabilities;

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
public interface GetFeatureWithLock extends GetFeature
{
  /* #Request lnkWFS_Service; */
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:07  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:12  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01 doemming
 * *** empty log message *** Revision 1.3 2004/01/26 08:15:37 poth no message
 * 
 * Revision 1.2 2003/06/10 07:52:04 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:52 poth no message
 * 
 * Revision 1.2 2002/08/15 10:02:41 ap no message
 * 
 * Revision 1.1 2002/04/26 09:02:34 ap no message
 * 
 * 
 *  
 */
