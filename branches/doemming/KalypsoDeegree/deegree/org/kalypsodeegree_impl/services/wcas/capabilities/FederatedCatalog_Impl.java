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
package org.deegree_impl.services.wcas.capabilities;

import java.net.URL;
import org.deegree.services.wcas.capabilities.FederatedCatalog;

/**
 *
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-01
 */
public class FederatedCatalog_Impl implements FederatedCatalog {
    
    private String name 	= null;
    private String title	= null;
    private String abstract_	= null;
    private URL url		= null;
    
    FederatedCatalog_Impl(String name, String title, String abstract_, URL url) {
        setName( name );
        setTitle( title );
        setAbstract( abstract_ );
        setCatalogURL( url );
    }
    
    /**
     * returns the name of the federated catalog (mandatory)
     */
    public String getName() {
        return name;
    }
    
    /**
     * @see FederatedCatalog_Impl#getName()
     */
    public void setName(String name) {
        this. name = name;
    }
    
    /**
     * returns the title of the federated catalog
     */
    public String getTitle() {
        return title;
    }
    
    /**
     * @see FederatedCatalog_Impl#getTitle()
     */
    public void setTitle(String title) {
        this.title = title;
    }
    
    /**
     * returns an abstract that describes the federated catalog in detail
     */
    public String getAbstract() {
        return abstract_;
    }
    
    /**
     * @see FederatedCatalog_Impl#getAbstract()
     */
    public void setAbstract(String abstract_) {
        this.abstract_ = abstract_;
    }
    
    /**
     * returns the URL of the federated catalog
     */
    public URL getCatalogURL() {
        return url;
    }
    
    /**
     * @see FederatedCatalog_Impl#getCatalogURL()
     */
    public void setCatalogURL(URL url) {
        this.url = url;
    }
    
    
    public String toString() {
        String ret = null;
        ret = "name = " + name + "\n";
        ret += "title = " + title + "\n";
        ret += "abstract_ = " + abstract_ + "\n";
        ret += "url = " + url + "\n";
        return ret;
    }
    
}
