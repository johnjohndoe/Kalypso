// $Header$
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
package org.deegree_impl.clients.gazetteer.model;

import java.util.*;

import org.deegree.services.gazetteer.SI_LocationInstance;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author last edited by: $Author$
 *
 * @version 1.0. $Revision$, $Date$
 *
 * @since 1.1
 */
public class GetTermsResultSet {
    private HashMap terms = null;
    private String childType = null;
    private String parentType = null;
    private String parentValues = null;

    /**
     * @param parentType
     * @param parentValues
     * @param childType
     * @param terms
     */
    public GetTermsResultSet( String parentType, String parentValues, String childType, 
                              HashMap terms ) {
        this.parentType = parentType;
        this.parentValues = parentValues;
        this.childType = childType;
        this.terms = terms;
    }

    /**
     * @return Returns the childType.
     */
    public String getChildType() {
        return childType;
    }

    /**
     * @param childType The childType to set.
     */
    public void setChildType( String childType ) {
        this.childType = childType;
    }

    /**
     * @return Returns the parentType.
     */
    public String getParentType() {
        return parentType;
    }

    /**
     * @param parentType The parentType to set.
     */
    public void setParentType( String parentType ) {
        this.parentType = parentType;
    }

    /**
     * @return Returns the parentValues.
     */
    public String getParentValues() {
        return parentValues;
    }

    /**
     * @param parentValues The parentValues to set.
     */
    public void setParentValues( String parentValues ) {
        this.parentValues = parentValues;
    }

    /**
     * @return Returns the terms.
     */
    public SI_LocationInstance[] getTerms( String gazetteer ) {
        return (SI_LocationInstance[])terms.get( gazetteer );
    }

    /**
     * @param terms The terms to set.
     */
    public void setTerms( HashMap terms ) {
        this.terms = terms;
    }
    
    /**
     * returns the names of all gazetteers which request results (terms) are 
     * contained in this result set
     */
    public String[] getGazetteerNames() {
        return (String[])terms.keySet().toArray( new String[ terms.size() ] );
    }
}
/* ********************************************************************
   Changes to this class. What the people have been up to:
   $Log$
   Revision 1.1  2004/05/11 16:43:27  doemming
   Initial revision

   Revision 1.2  2004/03/16 08:07:23  poth
   no message

   Revision 1.1  2004/03/15 07:38:05  poth
   no message



********************************************************************** */