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
 * Defines the operations for the WFS. For example from the FeatureTypeList-interface.
 *
 * <p>-----------------------------------------------------</p>
 *
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer</a>
 * @version $Revision$ $Date$
 */
public interface WFSOperation {
    /**
     * The &lt;InsertFeature&gt; element is used to indicate that the WFS is capable
     * of creating new instances of a feature type.
     */
    public static final int OPERATION_INSERT = 0;

    /**
     * The &lt;UpdateFeature&gt; element indicates that the WFS can change the
     * existing state of a feature.
     */
    public static final int OPERATION_UPDATE = 1;

    /**
     * The &lt;DeleteFeature&gt; element indicates that the WFS can delete or
     * remove instances of a feature type from the datastore.
     */
    public static final int OPERATION_DELETE = 2;

    /**
     * The &lt;QueryFeature&gt; element indicates that the WFS is capable of
     * executing a query on a feature type.
     */
    public static final int OPERATION_QUERY = 3;

    /**
     * The &lt;LockFeature&gt; element indicates that the WFS is capable of
     * locking instances of a feature type.
     */
    public static final int OPERATION_LOCK = 4;
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:22  doemming
 * Initial revision
 *
 * Revision 1.2  2003/04/23 07:23:15  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:53  poth
 * no message
 *
 * Revision 1.5  2002/08/15 10:02:41  ap
 * no message
 *
 * Revision 1.4  2002/07/10 14:17:53  ap
 * no message
 *
 * Revision 1.3  2002/04/26 09:02:51  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:17:15  ap
 * no message
 *
 *
 */
