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
package org.deegree.services.wfs.protocol;

/**
* In response to a transaction request, the web feature server shall 
* generate an object document indicating the termination status of the 
* transaction. In addition, if the transaction request includes <Insert> 
* operations, then the web feature server must report the feature 
* identifiers of all newly created features. In the event that the 
* transaction fails to execute, a web feature server shall also indicate 
* this in the response.
*
* <p>--------------------------------------------------------</p>
*
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version $Revision$ $Date$
*/
public interface WFSTransactionResponse extends WFSBasicResponse {
    /**
     * The <InsertResult> is used to delimit one or more feature 
     * identifiers of newly created features. The insert results are 
     * reported in the order in which the <Insert> operations were 
     * specified in the <Transaction> request. Additionally, they can 
     * be correlated using the handle attribute.    
     */
    public WFSInsertResult[] getInsertResult();

    /**
     * A transaction can terminate with a status of:
     * <li>SUCCESS: The transaction was successfully completed.
     * <li>FAILED: One or more operations in the transaction failed.
     * <li>PARTIAL: The transaction partially succeeded and the data 
     *               may be in an inconsistent state. For systems that 
     *               do not support atomic transactions, this outcome is 
     *               a distinct possibility.
     */
    public String getStatus();

    /**
     * In the event that a transaction request fails, the <Locator> 
     * can be used to indicate which part of the transaction failed. 
     * If the failed operation is labeled using a handle attribute then 
     * that can be used to locate the failure. Otherwise, the web feature
     * server may try to identify the failure relative to the beginning 
     * of the transaction request possibly using line numbers or some other 
     * convenient mechanism.    
     */
    public String getLocator();

    /**
     * Returns any error messages.
     */
    public String getMessage();

    /**
     * The handle attribute is included to allow a server to associate 
     * any text to the response. The purpose of the handle 
     * attribute is to provide an error handling mechanism for locating 
     * a statement that might fail.
     */
    public String getHandle();

    /*#WFSInsertResult lnkInsertResult;*/
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:22  doemming
 * Initial revision
 *
 * Revision 1.3  2004/02/09 07:57:02  poth
 * no message
 *
 * Revision 1.2  2003/04/23 07:23:15  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:54  poth
 * no message
 *
 * Revision 1.4  2002/08/15 10:02:41  ap
 * no message
 *
 * Revision 1.3  2002/04/26 09:02:51  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:17:15  ap
 * no message
 *
 */
