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

import java.io.Serializable;
import java.rmi.RemoteException;

import org.deegree_impl.model.gc.GC_GridCoverage_Impl;
import org.opengis.gp.GP_GridAnalysis;
import org.opengis.pt.PT_Matrix;


/**
 * Performs various analysis operations on a grid coverage.
 *
 * @version 1.00
 * @since   1.00 
 */
class GP_GridAnalysis_Impl extends GC_GridCoverage_Impl 
                            implements GP_GridAnalysis, Serializable
{
    
    GP_GridAnalysis_Impl() throws java.rmi.RemoteException
    {
        super( null, false );
    }
 
    /** Determine the standard deviation from the mean of the grid values for a sample dimension.
     *
     * @param sampleDimension Index of sample dimension.
     * @return he standard deviation from the mean of the grid values for a sample dimension.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public double getStdDev(int sampleDimension) throws RemoteException {
        return -1;
    }
    
    /** Number of predetermined overviews for the grid.
     *
     * @return the number of predetermined overviews for the grid.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public int getNumOverviews() throws RemoteException {
        return -1;
    }
    
    /** Determine the mode grid value for a sample dimension.
     *
     * @param sampleDimension Index of sample dimension.
     * @return the mode grid value for a sample dimension.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public double getModeValue(int sampleDimension) throws RemoteException {
        return -1;
    }
    
    /** Determine the minimum grid value for a sample dimension.
     *
     * @param sampleDimension Index of sample dimension.
     * @return the minimum grid value for a sample dimension.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public double getMinValue(int sampleDimension) throws RemoteException {
        return -1;
    }
    
    /** Determine the median grid value for a sample dimension.
     *
     * @param sampleDimension Index of sample dimension.
     * @return the median grid value for a sample dimension.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public double getMedianValue(int sampleDimension) throws RemoteException {
        return -1;
    }
    
    /** Determine the mean grid value for a sample dimension.
     *
     * @param sampleDimension Index of sample dimension.
     * @return the mean grid value for a sample dimension.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public double getMeanValue(int sampleDimension) throws RemoteException {
        return -1;
    }
    
    /** Determine the maximum grid value for a sample dimension.
     *
     * @param sampleDimension Index of sample dimension.
     * @return the maximum grid value for a sample dimension.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public double getMaxValue(int sampleDimension) throws RemoteException {
        return -1;
    }
    
    /** Determine the histogram of the grid values for a sample dimension.
     *
     * @param sampleDimension Index of sample dimension to be histogrammed.
     * @param minimumEntryValue Minimum value stored in the first histogram entry.
     * @param maximumEntryValue Maximum value stored in the last histogram entry.
     * @param numberEntries Number of entries in the histogram.
     * @return the histogram of the grid values for a sample dimension.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public int[] getHistogram(int sampleDimension, double minimumEntryValue, 
                              double maximumEntryValue, int numberEntries) 
                              throws RemoteException {
        return null;
    }
    
    /** Determine the correlation between sample dimensions in the grid.
     *
     * @return the correlation between sample dimensions in the grid.
     * @throws RemoteException if a remote method call failed.
     *
     */
    public PT_Matrix getCorrelation() throws RemoteException {
        return null;
    }
    
}
