
package org.kalypso.services.metadoc.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.metadoc.impl.jaxws.PrepareNewDocument;

@XmlRootElement(name = "prepareNewDocument", namespace = "http://impl.metadoc.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "prepareNewDocument", namespace = "http://impl.metadoc.services.kalypso.org/")
public class PrepareNewDocument {

    @XmlElement(name = "arg0", namespace = "")
    private String arg0;

    /**
     * 
     * @return
     *     returns String
     */
    public String getArg0() {
        return this.arg0;
    }

    /**
     * 
     * @param arg0
     *     the value for the arg0 property
     */
    public void setArg0(String arg0) {
        this.arg0 = arg0;
    }

}
