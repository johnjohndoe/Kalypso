
package org.kalypso.services.metadoc.impl.jaxws;

import javax.activation.DataHandler;
import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.metadoc.impl.jaxws.CommitNewDocument;

@XmlRootElement(name = "commitNewDocument", namespace = "http://impl.metadoc.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "commitNewDocument", namespace = "http://impl.metadoc.services.kalypso.org/", propOrder = {
    "arg0",
    "arg1"
})
public class CommitNewDocument {

    @XmlElement(name = "arg0", namespace = "")
    private org.kalypso.services.metadoc.DocumentBean arg0;
    @XmlElement(name = "arg1", namespace = "")
    private DataHandler arg1;

    /**
     * 
     * @return
     *     returns DocumentBean
     */
    public org.kalypso.services.metadoc.DocumentBean getArg0() {
        return this.arg0;
    }

    /**
     * 
     * @param arg0
     *     the value for the arg0 property
     */
    public void setArg0(org.kalypso.services.metadoc.DocumentBean arg0) {
        this.arg0 = arg0;
    }

    /**
     * 
     * @return
     *     returns DataHandler
     */
    public DataHandler getArg1() {
        return this.arg1;
    }

    /**
     * 
     * @param arg1
     *     the value for the arg1 property
     */
    public void setArg1(DataHandler arg1) {
        this.arg1 = arg1;
    }

}
