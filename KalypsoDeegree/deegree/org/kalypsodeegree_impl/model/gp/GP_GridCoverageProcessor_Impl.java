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

// GCS dependencies
import java.rmi.RemoteException;

import org.opengis.gc.GC_GridCoverage;
import org.opengis.gc.GC_Parameter;
import org.opengis.gp.GP_GridAnalysis;
import org.opengis.gp.GP_GridCoverageProcessor;
import org.opengis.gp.GP_Operation;


/**
 * Allows for different ways of accessing the grid coverage values.
 *
 * @version 1.00
 * @since   1.00
 */
class GP_GridCoverageProcessor_Impl implements GP_GridCoverageProcessor
{
   
    /** Creates a {@link GP_GridAnalysis} interface from a grid coverage.
     * This allows grid analysis functions to be performed on a grid coverage.
     *
     * @param gridCoverage Grid coverage on which the analysis will be performed.
     * @return a new {@link GP_GridAnalysis} interface.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public GP_GridAnalysis analyse(GC_GridCoverage gridCoverage) throws RemoteException {
        return null;
    }
    
    /** Apply a process operation to a grid coverage.
     *
     * @param operationName Name of the operation to be applied to the grid coverage.
     * @param parameters List of name value pairs for the parameters required for the operation.
     * @return the grid coverage which has been applied the process operation.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public GC_GridCoverage doOperation(String operationName, GC_Parameter[] parameters) 
                                                        throws RemoteException {
        return null;
    }
    
    /** Retrieve the list of metadata keywords for the interface.<br>
     * An empty list will returned if no metadata is available.
     *
     * @return the list of metadata keywords for the interface.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public String[] getMetadataNames() throws RemoteException {
        return null;
    }
    
    /** Retrieve the metadata value for a given metadata name.
     *
     * @param name Metadata keyword for which to retrieve metadata.
     * @return the metadata value for a given metadata name.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public String getMetadataValue(String name) throws RemoteException {
        return null;
    }
    
    /** The number of operations supported by the GP_GridCoverageProcessor.
     *
     * @return the number of operations supported by the GP_GridCoverageProcessor.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public int getNumOperations() throws RemoteException {
        return -1;
    }
    
    /** Retrieve a grid processing operation information.
     * The operation information will contain the name of the operation as well
     * as a list of its parameters.<br><br>
     *
     * @param index Index for which to retrieve the operation information.
     * @return a grid processing operation information.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public GP_Operation getOperation(int index) throws RemoteException {
        return null;
    }
    
}
