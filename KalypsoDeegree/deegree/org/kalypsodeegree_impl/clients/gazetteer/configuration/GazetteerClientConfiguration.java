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
package org.deegree_impl.clients.gazetteer.configuration;

import java.io.InputStreamReader;
import java.net.URL;
import java.util.HashMap;


/**
 * enables access to the configuration parameters of the gazetteer client. Mainly 
 * these are the names and addresses of the gazetteers services to be called
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author last edited by: $Author$
 *
 * @version 1.0. $Revision$, $Date$
 *
 * @since 1.1
 */
public class GazetteerClientConfiguration {
    private static GazetteerClientConfiguration conf = null;
    private HashMap gazetteers = null;

    /**
     * Creates a new GazetteerClientConfiguration object.
     *
     * @param gazetteers 
     */
    GazetteerClientConfiguration(HashMap gazetteers) {
        this.gazetteers = gazetteers;
    }

    /**
     *
     *
     * @param confURL 
     *
     * @return 
     *
     * @throws Exception 
     */
    public synchronized static GazetteerClientConfiguration getInstance( URL confURL )
                                                           throws Exception {
        InputStreamReader isr = new InputStreamReader( confURL.openStream() );
        conf = ConfigurationFactory.createConfiguration( isr );
        isr.close();

        return conf;
    }

    /**
     *
     *
     * @return 
     */
    public static GazetteerClientConfiguration getInstance() {
        if ( conf == null ) {
            conf = new GazetteerClientConfiguration( new HashMap() );
        }

        return conf;
    }

    /**
    * returns the address of the submitted thesaurus
    */
    public URL getGazetteerAddress( String gazetteer ) {
        return (URL)this.gazetteers.get( gazetteer );
    }

    /**
    * returns the names of the thesauri known by the client
    */
    public String[] getGazetteerNames() {
        String[] tn = new String[gazetteers.size()];
        return (String[])gazetteers.keySet().toArray( tn );
    }
}
/* ********************************************************************
   Changes to this class. What the people have been up to:
   $Log$
   Revision 1.1  2004/05/11 16:43:27  doemming
   Initial revision

   Revision 1.1  2004/03/24 12:36:22  poth
   no message

   Revision 1.1  2004/03/15 07:38:05  poth
   no message



********************************************************************** */