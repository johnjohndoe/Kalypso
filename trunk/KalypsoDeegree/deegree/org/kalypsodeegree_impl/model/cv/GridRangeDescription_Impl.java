/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree_impl.model.cv;

import org.deegree.model.coverage.GridRangeDescription;
import org.deegree.model.coverage.Histogram;
import org.deegree.model.coverage.Observable;
import org.deegree.model.coverage.PseudoColorTable;
import org.deegree.model.coverage.RangeAxis;

/**
 * In the case of a grid coverage layer, the range description adds several 
 * optional elements of the generic range component description, using a 
 * repeatable GridRangeDescription 
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
public class GridRangeDescription_Impl implements GridRangeDescription {
                                                    
    private String colorInterpretation          = null;
    private Histogram histogram                 = null;
    private PseudoColorTable pseudoColorTable   = null;
    private String sampleEncoding               = null;
    private String description                  = null;
    private String id                           = null;
    private Observable observable               = null;
    private RangeAxis[] rangeAxis               = null;
    private String title                        = null;

    public GridRangeDescription_Impl(String id, String title, String description,
                              Observable observable, RangeAxis[] rangeAxis,
                              String colorInterpretation, Histogram histogram,
                              PseudoColorTable pseudoColorTable, String sampleEncoding)
    {
        this.id = id;
        this.title = title;
        this.description = description;
        this.observable = observable;
        this.rangeAxis = rangeAxis;
        this.colorInterpretation = colorInterpretation;
        this.histogram = histogram;
        this.pseudoColorTable = pseudoColorTable;
        this.sampleEncoding = sampleEncoding;
    }

    /**  indicates how to read the image colors
     *
     */
    public String getColorInterpretation() {
        return colorInterpretation;
    }    
    
    /** A <tt>Histogram</tt> records image statistics (mean and median value,
     * etc.) and lists the counts and percentages of pixels in each of several
     * brightness “bins.”
     *
     */
    public Histogram getHistogram() {
        return histogram;
    }
    
    /** A <tt>PseudoColorTable</tt> lists indexed color values, expressed as
     * red-green-blue-alpha values, or hue-lightness-saturation,
     * cyan-magenta-yellow-black, or grayscale.
     *
     */
    public PseudoColorTable getPseudoColorTable() {
        return pseudoColorTable;
    }
    
    /** details the encoding of image samples
     *
     */
    public String getSampleEncoding() {
        return sampleEncoding;
    }

    /** returns a short description of the range set
     *
     */
    public String getDescription() {
        return description;
    }    

    /** returns the ID of the range set
     *
     */
    public String getID() {
        return id;
    }

    /** The optional <tt>Observable</tt> field provides a structured description
     * of the observations (quantities or properties) reported in this range
     * component. This description consists of a name, a free-text description,
     * a docURL (an index into a registry of observation types); and a
     * referenceSystem that associates the reported values with real-world
     * quantities or categories.
     *
     */
    public Observable getObservable() {
        return observable;
    }

    /** The optional RangeAxis element is for compound observations. It describes
     * and lists the values at which the range component reports properties, or
     * the “bins” by which the range componet reports counts or other aggregate
     * values
     *
     */
    public RangeAxis[] getRangeAxis() {
        return rangeAxis;
    }

    /** returns the title of the range set
     *
     */
    public String getTitle() {
        return title;
    }
}
