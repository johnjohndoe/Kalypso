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
package org.deegree.xml;

/**
 * This exception is thrown when a syntactic or semantic error has been
 * encountered during the parsing process of a XML document.
 * <p>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$
 */
public class XMLParsingException extends Exception {

     private String message = "org.deegree.xml.XMLParsingException";
    private String st = "";
    
    /**
     * Creates a new instance of <code>XMLParsingException</code> without detail message.
     */
    public XMLParsingException() {
    }
    
     
    /**
     * Constructs an instance of <code>XMLParsingException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public XMLParsingException(String msg) {
        message = msg;
    }
       
    /**
     * Constructs an instance of <code>XMLParsingException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public XMLParsingException(String msg, Exception e) {
        this( msg );
        StackTraceElement[] se = e.getStackTrace();
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < se.length; i++) {
            sb.append( se[i].getClassName() + " " );
            sb.append( se[i].getFileName() + " " );
            sb.append( se[i].getMethodName() + "(" );
            sb.append( se[i].getLineNumber() + ")\n" );
        }
        st = e.getMessage() + sb.toString();
    }
    
    public String toString() {
        return message + "\n" + st;
    }
    
    public String getMessage() {
        return message;
    }
}
