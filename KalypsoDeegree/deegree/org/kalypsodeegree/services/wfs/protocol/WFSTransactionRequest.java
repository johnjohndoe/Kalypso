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
* The Transaction interface is used to describe data transformation 
* operations that are to be applied to web accessible features. The web 
* feature server receives a transaction request and either processes 
* it directly or possibly translates it into the language of a target 
* datastore and then has the datastore execute the transaction. When the 
* transaction has been completed, the web feature server will generate an 
* XML response document indicating the termination status of the transaction.
*
* <p>--------------------------------------------------------</p>
*
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version $Revision$ $Date$
*/
public interface WFSTransactionRequest extends WFSBasicRequest {
    /** 
     * A <Transaction> element is used to define a single transaction 
     * composed of zero or more <Insert>, <Update>, or <Delete> elements. 
     * An empty <Transaction> request is valid but not very useful.
     * <p>
     * An operation is meant to be applied to a single feature type, but 
     * multiple operations on multiple feature types can be packaged within 
     * a single Transaction request.
     */
    public WFSOperation[] getOperations();

    /**
     * A value of ALL indicates that all feature locks should be 
     * released when a transaction terminates. A value of SOME indicates 
     * that only those records that are modified should be released. 
     * The remaining locks are maintained. The default RELEASEACTION is ALL.
     */
    public String getReleaseAction();

    /**
     * returns the id that handles the locking associated to the request
     */
    public String getLockId();

    /**
     * returns a descriptor that enables easy identifying the transaction
     */
    public String getHandle();

    /**
     * returns the version of the WFS
     */
    public String getVersion();
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:22  doemming
 * Initial revision
 *
 * Revision 1.4  2004/02/09 07:57:02  poth
 * no message
 *
 * Revision 1.3  2004/01/26 08:15:37  poth
 * no message
 *
 * Revision 1.2  2003/04/23 07:23:15  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:54  poth
 * no message
 *
 * Revision 1.6  2002/08/15 10:02:41  ap
 * no message
 *
 * Revision 1.5  2002/07/10 14:17:53  ap
 * no message
 *
 * Revision 1.4  2002/04/26 09:02:51  ap
 * no message
 *
 * Revision 1.2  2002/04/25 16:17:20  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:17:15  ap
 * no message
 *
 *
 */
