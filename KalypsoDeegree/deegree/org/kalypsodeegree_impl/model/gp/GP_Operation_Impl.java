/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree
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
E-Mail: fitzke@giub.uni-bonn.de


 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.gp;

// Remote Method Invocation
import java.rmi.RemoteException;

import org.opengis.gc.GC_ParameterInfo;
import org.opengis.gp.GP_Operation;


/**
 * This interface provides descriptive information for a grid coverage processing
 * operation. The descriptive information includes such information as the name of
 * the operation, operation description, number of source grid coverages required
 * for the operation etc.
 *
 * @version 1.00
 * @since   1.00 
 */
class GP_Operation_Impl implements GP_Operation
{
   
    /** Description of the processing operation.
     * If no description, the value will be an null or empty string.
     *
     * @return the description of the processing operation.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public String getDescription() throws RemoteException {
        return null;
    }
    
    /** URL for documentation on the processing operation.
     * If no online documentation is available the string will be empty.
     *
     * @return the URL for documentation on the processing operation.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public String getDocURL() throws RemoteException {
        return null;
    }
    
    /** Name of the processing operation.
     *
     * @return the name of the processing operation.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public String getName() throws RemoteException {
        return null;
    }
    
    /** Number of parameters for the operation.
     *
     * @return the number of parameters for the operation.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public int getNumParameters() throws RemoteException {
        return -1;
    }
    
    /** Number of source grid coverages required for the operation.
     *
     * @return the number of source grid coverages required for the operation.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public int getNumSources() throws RemoteException {
        return -1;
    }
    
    /** Retrieve the parameter information for a given index.
     *
     * @param index Parameter information index to retrieve. Index starts at 0.
     * @return the parameter information for a given index.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public GC_ParameterInfo getParameterInfo(int index) throws RemoteException {
        return null;
    }
    
    /** Implementation vendor name.
     *
     * @return the implementation vendor name.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public String getVendor() throws RemoteException {
        return null;
    }
    
    /** Version number for the implementation.
     *
     * @return the version number for the implementation.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public String getVersion() throws RemoteException {
        return null;
    }
    
}
