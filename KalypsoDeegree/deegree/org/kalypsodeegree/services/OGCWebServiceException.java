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

package org.deegree.services;

/**
 * <p>
 * A web feature server has to handle two classes of errors; exceptions that are
 * generated during the processing of a WFS request (such as parsing the
 * request) and exceptions that are generated during the execution of a WFS
 * request (such as a transaction failure). This section deals with exceptions
 * generated during the processing of a WFS request.
 * <p>
 * Exceptions encountered during request execution are dealt with in subsequent
 * sections. In the event that a web feature server encounters an error while
 * processing a request, it shall generate an XML document indicating that an
 * error has occurred.
 * <p>
 * The &lt;WFS_Exception&gt; tag can delimit one or more exception.details.
 * Individual exceptions are delimited using the &lt;Exception&gt; tag.
 * <p>
 * The optional &lt;Locator%gt; element is used to indicate where an exception
 * was encountered. A number of elements defined in this document include a
 * handle attribute that can be used to uniquely identify an element. If such a
 * handle exists, its value would be reported using the <Locator>element. If a
 * handle attribute does not exist or is not defined, then the web feature
 * server can attempt to locate the error using other means such as line
 * numbers, etc...
 * <p>
 * The &lt;Message&gt; element is used to delimit any messages associated with
 * an exception
 */

public interface OGCWebServiceException
{

  /**
   * returns the class/service that has caused the exception
   */
  public String getLocator();

  /**
   * returns the error message
   */
  public String getMessage();

}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:07  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:11  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01
 * doemming *** empty log message *** Revision 1.4 2004/01/26 08:15:37 poth no
 * message
 * 
 * Revision 1.3 2003/06/10 07:52:02 poth no message
 * 
 * Revision 1.2 2003/05/26 07:16:06 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:50 poth no message
 * 
 * Revision 1.2 2002/08/19 15:57:23 ap no message
 * 
 * Revision 1.1 2002/05/17 15:58:45 ap no message
 * 
 * Revision 1.4 2002/05/14 14:40:42 ap no message
 * 
 * Revision 1.3 2002/04/26 09:02:51 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
